# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# calculate_waits.R
# Angus Morton
# 2024-08-26
# 
# Apply new wait calculation rules
# 
# R version 4.1.2 (2021-11-01)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Step 0 : Housekeeping ----

source("code/settings.R")

library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(tidylog)
library(openxlsx)
library(ggplot2)
library(phsstyles)

#### Step 1 : read in boxi data ----

waits_init <- read.xlsx(boxi_extract, sheet = "Ongoing waits",
                   sep.names = "_",
                   detectDates = TRUE) |> 
  select(-c("Ongoing/Completed", "Number_Seen/On_list"))

unavail_init <- read.xlsx(boxi_extract, sheet = "Unavailability",
                          sep.names = "_",
                          detectDates = TRUE) |> 
  select(-c("Patient_Type_Cohort_Description",
            "Sending_Location_Code")) |> 
  rename(CHI = Pat_CHI_Number,
         MUI = Mandatory_Unique_Identifier)

offers_init <- read.xlsx(boxi_extract, sheet = "Appointments & Offers",
                         sep.names = "_",
                         detectDates = TRUE) |> 
  select(-Patient_Type_Cohort_Description) |> 
  rename(CHI = Pat_CHI_Number,
         MUI = Mandatory_Unique_Identifier)


#### Step 2 : trim off post target data ----
# If running multiple times run from here to save you reading in the files
# multiple times

target_date <- waits_init |> 
  select(Date) |> 
  distinct(Date) |> 
  dmy()

waits <- waits_init

unavail <- unavail_init |> 
  filter(Unavail_Start_Date <= target_date) |> 
  mutate(Unavail_End_Date = if_else(Unavail_End_Date > target_date,
                                    target_date, Unavail_End_Date)) |> 
  mutate(Number_Days_Unavailable = Unavail_End_Date - Unavail_Start_Date)

offers <- offers_init |> 
  filter(Offer_Date <= target_date) |> 
  mutate(
    Non_Attendance_Category_Description = if_else(Non_Attendance_Date > target_date,
                                                  NA, Non_Attendance_Category_Description),
    
    Non_Attendance_Outcome_Description = if_else(Non_Attendance_Date > target_date,
                                                 NA, Non_Attendance_Outcome_Description),
    
    Non_Attendance_Date = if_else(Non_Attendance_Date > target_date,
                                  NA, Non_Attendance_Date),
    
    Offer_Type_Description = if_else(Offer_Date > target_date,
                                     NA, Offer_Type_Description),
    
    Offer_Order = if_else(Offer_Date > target_date,
                          NA, Offer_Order),
    
    Offer_Outcome_Description = if_else(Response_Rcvd_Date > target_date,
                                        NA, Offer_Outcome_Description),
    
    Offer_Date = if_else(Offer_Date > target_date,
                         NA, Offer_Date),
    
    `Appt/Adm_Date` = if_else(`Appt/Adm_Date` > target_date,
                              NA, `Appt/Adm_Date`))

#### Step 3 : define cohort ----
# This is where you filter the dataset based on what you want to look at

waits <- waits |> 
  filter(Patient_Type == "Inpatient/Day case")



#### Step 5 : find latest clock resets ----
# This section finds the latest clock reset for each wait and appends this
# information to the waits table

# Create a list of waits with the date of their latest CNA/DNA
# left join on waits to only get offers relevant to the cohort

last_non_attendances <- waits |>
  left_join(offers, by = c("MUI", "CHI")) |> 
  filter(Non_Attendance_Category_Description %in% c("Could Not Attend",
                                                    "Did Not Attend")) |> 
  group_by(MUI, CHI) |> 
  summarise(
    last_non_attendance = max(Non_Attendance_Date)
  ) |> 
  ungroup() |> 
  select(MUI, CHI, last_non_attendance)


# Create a list of waits with the date of their latest declined pair
# left join on waits to only get offers relevant to the cohort

last_declined_pairs <- waits |>
  left_join(offers, by = c("MUI", "CHI")) |> 
  mutate(
    rejected_reasonable = if_else(
      (`Appt/Adm_Date` - Offer_Date >= 10) & 
        str_detect(Offer_Outcome_Description, "Declined"),
      "rejected reasonable", NA)) |> 
  arrange(MUI, CHI, desc(Offer_Order)) |> 
  group_by(MUI, CHI) |> 
  mutate(
    declined_pair = if_else(rejected_reasonable == "rejected reasonable" &
                              lag(rejected_reasonable) == "rejected reasonable", 1, 0)
  ) |> 
  ungroup() |> 
  filter(declined_pair == 1) |> 
  group_by(MUI, CHI) |> 
  summarise(
    last_rejection = max(Response_Rcvd_Date)
  ) |> 
  select(MUI, CHI, last_rejection)


# Join these clock reset dates onto the waits file

waits <- waits |> 
  left_join(last_non_attendances, by = c("MUI", "CHI")) |> 
  left_join(last_declined_pairs, by = c("MUI", "CHI"))


# In cases with a CNA/DNA and a declined pair. Take the latest of the two
# (This block is slow because of rowwise operations. Could be more efficient)

waits <- waits |> 
  rowwise() |> 
  mutate(last_reset = max(last_non_attendance,
                          last_rejection,
                          na.rm = TRUE)) |> 
  ungroup()


#### Step 6 : find relevant unavailability periods ----
# This section sums up all periods of unavailability for each wait and 
# appends this information to the waits table
#
# Under the new guidance all unavailability is counted so we don't need to
# deal with specific codes individually


# Only count unavailability after the last reset (new effective start date)

unavail <- waits |> 
  left_join(unavail, by = c("MUI", "CHI")) |> 
  mutate(Unavail_End_Date = if_else(Unavail_End_Date < last_reset,
                                    NA, Unavail_End_Date),
         Unavail_Start_Date = case_when(
           is.na(Unavail_End_Date) ~ NA,
           Unavail_Start_Date < last_reset ~ last_reset,
           TRUE ~ Unavail_Start_Date
         )) |> 
  mutate(Number_Days_Unavailable = Unavail_End_Date - Unavail_Start_Date) |> 
  group_by(MUI, CHI) |> 
  summarise(
    total_unavailability = sum(Number_Days_Unavailable, na.rm = TRUE)
  ) |> 
  ungroup()

### trying to do old unavailability rules
# unavail <- waits |> 
#     left_join(unavail, by = c("MUI", "CHI")) |> 
#     mutate(Unavail_End_Date = if_else(Unavail_End_Date < last_reset |
#                                         Unavail_End_Date > (last_reset+days(84)),
#                                       NA, Unavail_End_Date),
#            Unavail_Start_Date = case_when(
#              is.na(Unavail_End_Date) ~ NA,
#              Unavail_Start_Date < last_reset ~ last_reset,
#              Unavail_Start_Date > (last_reset+days(84)) ~ last_reset+days(84),
#              TRUE ~ Unavail_Start_Date
#            )) |> 
#     mutate(Number_Days_Unavailable = Unavail_End_Date - Unavail_Start_Date) #|> 
#   group_by(MUI, CHI) |> 
#     summarise(
#       total_unavailability = sum(Number_Days_Unavailable, na.rm = TRUE)
#     ) |> 
#     ungroup()
###
  
# Join onto waits table
  
waits <- waits |> 
  left_join(unavail, by = c("MUI", "CHI"))


#### Step 7 : calculate new length of wait ----
# Calculate wait length in weeks based on initial start date or last reset

waits_final <- waits |> 
  mutate(
    Effective_Start_Date = ymd(Effective_Start_Date),
    last_reset = ymd(last_reset)) |> 
  mutate(new_effective_start_date = if_else(is.na(last_reset),
                                            Init_Start_Date,
                                            last_reset)) |> 
  mutate(new_wait_length = target_date-new_effective_start_date-total_unavailability,
         old_wait_length = target_date-Effective_Start_Date) |> 
  mutate(new_wait_length = as.numeric(new_wait_length)/7,
         old_wait_length = as.numeric(old_wait_length)/7)


#### Step 8 : Investigations ----

# create bands based on planned care targets to look at records which
# have changed
waits_final_2 <- waits_final |> 
  mutate(
    bin_new = case_when(
      new_wait_length < 52 ~ "0-52",
      between(new_wait_length, 52, 78) ~ "52-78",
      between(new_wait_length, 78, 104) ~ "78-104",
      new_wait_length >= 104 ~ "104+"
    ),
    bin_old = case_when(
      old_wait_length < 52 ~ "0-52",
      between(old_wait_length, 52, 78) ~ "52-78",
      between(old_wait_length, 78, 104) ~ "78-104",
      old_wait_length >= 104 ~ "104+"
    )) |> 
  mutate(bin_new = factor(bin_new, levels = c("0-52","52-78",
                                             "78-104","104+")),
         bin_old = factor(bin_old, levels = c("0-52","52-78",
                                              "78-104","104+"))) |> 
  filter(bin_new != bin_old) |> 
  count(bin_old, bin_new)

# number of waits which have changed length
changed_waits <- waits_final |> 
  filter(old_wait_length != new_wait_length) |> 
  nrow()

# Percentage of waits which have changed
changed_waits_p <- 100*changed_waits/(nrow(waits_final))

# Mean change for an adjusted wait that has changed
mean_difference <- waits_final |> 
  filter(old_wait_length != new_wait_length) |> 
  summarise(mean_diff = mean(new_wait_length-old_wait_length))


#### Step 9 : Graphs ----


dow_hist <- waits_final |> 
  pivot_longer(cols = c(new_wait_length, old_wait_length)) |>
  ggplot(aes(x=value, fill=name)) + 
  geom_histogram(breaks = seq(0, 200, by = 13),
                 position = position_dodge())+
  scale_x_continuous(breaks = seq(0, 200, by=13))+
  scale_fill_manual(values = phs_colours(c("phs-magenta", "phs-purple")))+
  xlab("weeks waited") + 
  ylab("number waiting") +
  theme_phs()


#### Step 10 : optional exports ----

ggsave(
  "output/DoW_chart.jpg",
  scale = 3,
  plot = dow_hist)  
