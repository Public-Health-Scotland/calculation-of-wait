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
library(scales)

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
                         detectDates = TRUE)

offers_names <- names(offers_init)

offers_init <- offers_init |> 
  select(-Patient_Type_Cohort_Description) |> 
  rename(CHI = Pat_CHI_Number,
         MUI = Mandatory_Unique_Identifier)

offers_init_2 <- read.xlsx(boxi_extract, sheet = "Appointments & Offers(1)",
                           colNames = FALSE,
                           detectDates = TRUE)

names(offers_init_2) <- offers_names

offers_init_2 <- offers_init_2 |> 
  select(-Patient_Type_Cohort_Description) |> 
  rename(CHI = Pat_CHI_Number,
         MUI = Mandatory_Unique_Identifier)

offers_init <- bind_rows(offers_init, offers_init_2)

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
                         NA, Offer_Date))

#### Step 3 : define cohort ----
# This is where you filter the dataset based on what you want to look at

waits <- waits |> 
  filter(Patient_Type == "New Outpatient")



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

last_declined_pairs2 <- waits |>
  left_join(offers, by = c("MUI", "CHI")) |> 
  mutate(
    rejected_reasonable = if_else(
      (`Appt/Adm_Date` - Offer_Date >= 7) & 
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

check <- anti_join(last_declined_pairs, last_declined_pairs2)

# In cases with a declined pair and a cna/dna take the latest

clock_resets <- last_declined_pairs2 |> 
  full_join(last_non_attendances, by = c("MUI", "CHI")) |> 
  pivot_longer(last_rejection:last_non_attendance) |> 
  group_by(MUI, CHI) |> 
  summarise(value = max(value, na.rm = TRUE)) |> 
  ungroup() |> 
  rename(last_reset = value)

non_attendances <- waits |>
  left_join(offers, by = c("MUI", "CHI")) |> 
  filter(Non_Attendance_Category_Description %in% c("Could Not Attend",
                                                    "Did Not Attend")) |> 
  select(MUI, CHI, last_non_attendance = Non_Attendance_Date)

declined_pairs2 <- waits |>
  left_join(offers, by = c("MUI", "CHI")) |> 
  mutate(
    rejected_reasonable = if_else(
      (`Appt/Adm_Date` - Offer_Date >= 7) & 
        str_detect(Offer_Outcome_Description, "Declined"),
      "rejected reasonable", NA)) |> 
  arrange(MUI, CHI, desc(Offer_Order)) |> 
  group_by(MUI, CHI) |> 
  mutate(
    declined_pair = if_else(rejected_reasonable == "rejected reasonable" &
                              lag(rejected_reasonable) == "rejected reasonable" #&
                            #Offer_Outcome_Description != "Declined - New offer made/to be made"
                            , 1, 0)
  ) |> 
  ungroup() |> 
  filter(declined_pair == 1) |> 
  select(MUI, CHI, last_rejection = Response_Rcvd_Date)

all_clock_resets <- bind_rows(declined_pairs2, non_attendances) |> 
  pivot_longer(c("last_rejection", "last_non_attendance")) |> 
  select(-name) |> 
  rename(reset_date = value) |> 
  filter(!is.na(reset_date))

# Join these clock reset dates onto the waits file

waits_all_resets <- waits |> 
  left_join(all_clock_resets, by = c("MUI", "CHI"))

waits <- waits |> 
  left_join(clock_resets, by = c("MUI", "CHI"))


#### Step 6 : find relevant unavailability periods ----
# This section sums up all periods of unavailability for each wait and 
# appends this information to the waits table
#
# Under the new guidance all unavailability is counted so we don't need to
# deal with specific codes individually


# Only count unavailability after the last reset (new effective start date)

unavail_new <- waits |> 
  left_join(unavail, by = c("MUI", "CHI")) |> 
  mutate(Unavail_End_Date = if_else(Unavail_End_Date < last_reset,
                                    NA, Unavail_End_Date,
                                    missing = Unavail_End_Date),
         Unavail_Start_Date = case_when(
           is.na(Unavail_End_Date) ~ NA,
           Unavail_Start_Date < last_reset ~ last_reset,
           TRUE ~ Unavail_Start_Date
         )) |> 
  mutate(Number_Days_Unavailable = as.numeric(Unavail_End_Date - Unavail_Start_Date)+1) |> 
  group_by(MUI, CHI) |> 
  summarise(
    total_unavailability = sum(Number_Days_Unavailable, na.rm = TRUE)
  ) |> 
  ungroup()

waits_new <- waits |>
  left_join(unavail_new, by = c("MUI", "CHI")) |>
  mutate(total_unavailability = replace_na(total_unavailability,0))

### trying to do old unavailability rules
unavail_old <- waits |>
  left_join(unavail, by = c("MUI", "CHI")) |>
  mutate(Unavail_End_Date = if_else(Unavail_End_Date < Effective_Start_Date,
                                    NA, Unavail_End_Date,
                                    missing = Unavail_End_Date),
         Unavail_Start_Date = case_when(
           is.na(Unavail_End_Date) ~ NA,
           Unavail_Start_Date < Effective_Start_Date ~ Effective_Start_Date,
           TRUE ~ Unavail_Start_Date
         )) |>
  mutate(Number_Days_Unavailable = as.numeric(Unavail_End_Date - Unavail_Start_Date)+1,
         esd_lapse = as.numeric(Unavail_Start_Date - Effective_Start_Date)) |>
  group_by(MUI, CHI) |>
  arrange(Unavail_Start_Date) |>
  mutate(
    n_periods = n(),
    unavail_order = row_number()
  ) |>
  mutate(unavail_sum = lag(cumsum(Number_Days_Unavailable),
                           default = 0),
         wl_days_at_start = esd_lapse - unavail_sum,
         discarded = if_else(wl_days_at_start > 84, 1, 0)) |>
  mutate(counting = if_else(cumsum(discarded)==0,1,0)) |>
  ungroup() |>
  filter(counting == 1) |>
  group_by(MUI, CHI) |>
  summarise(
    total_unavailability = sum(Number_Days_Unavailable, na.rm = TRUE)
  ) |>
  ungroup()

waits_save <- waits

waits_old <- waits |>
  left_join(unavail_old, by = c("MUI", "CHI")) |>
  mutate(total_unavailability = replace_na(total_unavailability,0))

###

###

resets_within_12 <- waits_all_resets |> 
  filter(!is.na(reset_date)) |> 
  arrange(reset_date) |> 
  group_by(MUI, CHI) |> 
  mutate(prev_reset = lag(reset_date, default = max(Init_Start_Date)),
         multiple_resets = if_else(n() > 1, 1, 0)) |> 
  ungroup() |> 
  left_join(unavail, by = c("MUI", "CHI")) |>
  mutate(Unavail_End_Date = case_when(
    Unavail_End_Date < prev_reset ~ NA,
    Unavail_End_Date > reset_date ~ reset_date,
    TRUE ~ Unavail_End_Date),
    Unavail_Start_Date = case_when(
      is.na(Unavail_End_Date) ~ NA,
      Unavail_Start_Date > reset_date ~ NA,
      Unavail_Start_Date < prev_reset ~ prev_reset,
      TRUE ~ Unavail_Start_Date
    )) |>
  mutate(Number_Days_Unavailable = as.numeric(Unavail_End_Date - Unavail_Start_Date)+1,
         reset_lapse = as.numeric(Unavail_Start_Date - prev_reset)) |>
  group_by(MUI, CHI, reset_date) |>
  arrange(Unavail_Start_Date) |>
  mutate(
    n_periods = n(),
    unavail_order = row_number()
  ) |>
  mutate(unavail_sum = sum(Number_Days_Unavailable, na.rm = TRUE),
         wl_days_at_start = reset_lapse - unavail_sum,
         discarded = if_else(wl_days_at_start > 84, 1, 0)) |>
  mutate(counting = if_else(cumsum(discarded)==0,1,0),
         reset_discarded = if_else(as.numeric(reset_date-prev_reset)-unavail_sum > 84, 1, 0)) |>
  ungroup() |>
  arrange(reset_date) |> 
  group_by(MUI, CHI) |>
  mutate(
    reset_counting = if_else(cumsum(reset_discarded)==0,1,0)
  ) |>
  ungroup() |> 
  filter(reset_counting == 1) |> 
  group_by(MUI, CHI) |> 
  summarise(
    reset_date = max(reset_date)
  )

waits_with_resets <- waits |> 
  left_join(resets_within_12, by = c("MUI", "CHI")) |>
  mutate(last_reset = if_else(is.na(reset_date), Init_Start_Date,
                              reset_date))


unavail_old <- waits_with_resets |>
  left_join(unavail, by = c("MUI", "CHI")) |>
  mutate(Unavail_End_Date = if_else(Unavail_End_Date < last_reset,
                                    NA, Unavail_End_Date,
                                    missing = Unavail_End_Date),
         Unavail_Start_Date = case_when(
           is.na(Unavail_End_Date) ~ NA,
           Unavail_Start_Date < last_reset ~ last_reset,
           TRUE ~ Unavail_Start_Date
         )) |>
  mutate(Number_Days_Unavailable = as.numeric(Unavail_End_Date - Unavail_Start_Date)+1,
         esd_lapse = as.numeric(Unavail_Start_Date - last_reset)) |>
  group_by(MUI, CHI) |>
  arrange(Unavail_Start_Date) |>
  mutate(
    n_periods = n(),
    unavail_order = row_number()
  ) |>
  mutate(unavail_sum = lag(cumsum(Number_Days_Unavailable),
                           default = 0),
         wl_days_at_start = esd_lapse - unavail_sum,
         discarded = if_else(wl_days_at_start > 84, 1, 0)) |>
  mutate(counting = if_else(cumsum(discarded)==0,1,0)) |>
  ungroup() |>
  filter(counting == 1) |>
  group_by(MUI, CHI) |>
  summarise(
    total_unavailability = sum(Number_Days_Unavailable, na.rm = TRUE)
  ) |>
  ungroup()


waits_old <- waits_with_resets |>
  left_join(unavail_old, by = c("MUI", "CHI")) |>
  mutate(total_unavailability = replace_na(total_unavailability,0))

###


# Join onto waits table

# waits_new <- waits_with_resets |>
#   left_join(unavail_new, by = c("MUI", "CHI"))



#### Step 7 : calculate new length of wait ----
# Calculate wait length in weeks based on initial start date or last reset

waits_final <- waits_new |> 
  mutate(
    Effective_Start_Date = ymd(Effective_Start_Date),
    last_reset = ymd(last_reset)) |> 
  mutate(new_effective_start_date = if_else(is.na(last_reset),
                                            Init_Start_Date,
                                            last_reset)) |> 
  mutate(new_wait_length = target_date-days(total_unavailability)-new_effective_start_date) |>
  mutate(new_wait_length = if_else(new_wait_length < 0, 0,
                                   as.numeric(new_wait_length))) |> 
  rename(old_wait_length = Number_of_waiting_list_days) |> 
  mutate(new_wait_length = as.numeric(new_wait_length)/7,
         old_wait_length = as.numeric(old_wait_length)/7)


waits_final_check <- waits_old |>
  mutate(
    Effective_Start_Date = ymd(Effective_Start_Date),
    last_reset = ymd(last_reset)) |>
  mutate(new_effective_start_date = if_else(is.na(last_reset),
                                            Init_Start_Date,
                                            last_reset)) |>
  mutate(new_wait_length = target_date-days(total_unavailability)-new_effective_start_date) |>
  mutate(new_wait_length = if_else(new_wait_length < 0, 0,
                                   as.numeric(new_wait_length))) |>
  rename(old_wait_length = Number_of_waiting_list_days)

waits_final_check |>
  filter(Effective_Start_Date <= dmy("30/09/2024")) |> 
  mutate(match = if_else(old_wait_length_check == old_wait_length,
                         1, 0)) |>
  count(match)

waits_final_check |>
  mutate(match = if_else(new_wait_length == old_wait_length,
                         1, 0)) |>
  count(match)

waits_final_check |>
  filter(new_wait_length != old_wait_length) |>
  mutate(diff = new_wait_length - old_wait_length) |> 
  View()

non_matching_chis_all_dec_pairs <- waits_final_check |>
  filter(new_wait_length != old_wait_length) |> 
  select(CHI, MUI)


non_matching_chis_notbm <- waits_final_check |>
  filter(new_wait_length != old_wait_length) |> 
  select(CHI, MUI)

troublesome_chis <- inner_join(non_matching_chis_all_dec_pairs, non_matching_chis_notbm)


waits_final_check |> 
  filter(Effective_Start_Date >= dmy("30/09/2024")) |> 
  mutate(match = if_else(old_wait_length_check == old_wait_length,
                         1, 0)) |>
  filter(match == 0) |> 
  mutate(
    derived_esd = if_else(is.na(last_reset), Init_Start_Date,
                          last_reset),
    check = as.numeric(target_date - derived_esd - total_unavailability)/7,
    check_match = if_else(check == old_wait_length,
                          1, 0)) |> 
  filter(check_match == 0) |> 
  View()

# Only 1  non match. I'll take it

median_check <- waits_final_check |> 
  select(old_wait_length) |> 
  pull() |> 
  median()
# 


board_medians_check <- waits_final_check |> 
  group_by(NHS_Board_of_Treatment) |> 
  summarise(median = median(old_wait_length*7),
            `90th` = quantile(old_wait_length*7, 0.9)) |> 
  ungroup()

spec_medians_check <- waits_final_check |> 
  group_by(Specialty) |> 
  summarise(median = median(old_wait_length*7),
            `90th` = quantile(old_wait_length*7, 0.9)) |> 
  ungroup()

#### Step 8 : Investigations ----

#
median_wait <- waits_final |> 
  summarise(median_new = median(new_wait_length)*7,
            median_old = median(old_wait_length)*7,
            `90th new` = quantile(new_wait_length, 0.9)*7,
            `90th old` = quantile(old_wait_length, 0.9)*7)

board_medians <- waits_final |> 
  group_by(NHS_Board_of_Treatment) |> 
  summarise(median_new = median(new_wait_length)*7,
            median_old = median(old_wait_length)*7,
            `90th new` = quantile(new_wait_length, 0.9)*7,
            `90th old` = quantile(old_wait_length, 0.9)*7,
            med_diff_p = 100*(median_new-median_old)/median_old,
            `90th_diff_p` = 100*(`90th new`-`90th old`)/`90th old`) |> 
  ungroup()

spec_medians <- waits_final |> 
  group_by(Specialty) |> 
  summarise(median_new = median(new_wait_length)*7,
            median_old = median(old_wait_length)*7,
            `90th new` = quantile(new_wait_length, 0.9)*7,
            `90th old` = quantile(old_wait_length, 0.9)*7) |> 
  ungroup()

pub_path <- paste0("/PHI_conf/WaitingTimes/SoT/Publications/Inpatient, Day case and Outpatient Stage of Treatment Waiting Times/20240827/Output/R Output/PerformanceIPDC.csv")

pub_medians <- read_csv(pub_path) |> 
  mutate(Date = dmy(Date)) |> 
  filter(`Ongoing/Completed` == "Ongoing",
         `NHS Board of Treatment` != "NHS Scotland (Excluding NHS Tayside)",
         Date == dmy("30/06/2024")) |> 
  select(`NHS Board of Treatment`, Specialty, Median, `90th Percentile`) |> 
  rename(NHS_Board_of_Treatment = `NHS Board of Treatment`)

new_pub_comp <- waits_final |> 
  left_join(pub_medians, by = c("NHS_Board_of_Treatment", "Specialty")) |> 
  group_by(NHS_Board_of_Treatment) |> 
  summarise(median_new = median(new_wait_length)*7,
            median_old = median(old_wait_length)*7,
            `90th new` = quantile(new_wait_length, 0.9)*7,
            `90th old` = quantile(old_wait_length, 0.9)*7) |> 
  ungroup()

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

write_csv(waits_final_2, "output/band_changes.csv")

write_csv(board_medians, "output/board_medians.csv")
