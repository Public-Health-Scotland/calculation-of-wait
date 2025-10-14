# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# calculate_waits_resets_beyond_12.R
# Angus Morton
# 2024-11-11
# 
# Calculate waits factoring in clock resets beyond 12 weeks
# 
# R version 4.1.2 (2021-11-01)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

waits <- waits_init
offers <- offers_init
unavail <- unavail_init

#### Step 1 : Clock resets ----

unavail <- unavail |>
  left_join(select(waits, CHI, MUI, WTS), by = c("CHI", "MUI")) |>
  filter(!(Unavailability_Reason_Description == "Non-TTG - no response to PFB offer of appointment" &
             Patient_Type_Description %in% c("Inpatient", "Daycase") &
             WTS != "050"))

# unavail <- unavail |>
#   left_join(select(waits, CHI, MUI, Patient_Type, WTS), by = c("CHI", "MUI")) |>
#   filter(!(Unavailability_Reason_Description == "Non-TTG - no response to PFB offer of appointment" &
#              Patient_Type == "Inpatient/Day case" &
#              WTS != "050"))

#### Step 1 : Clock resets ----

# indefinite unavailability resets
unavail_resets <- waits |> 
  left_join(unavail, by = c("MUI", "CHI")) |> 
  filter(Unavailability_Reason_Description %in% c("Medical - indefinitely unavailable",
                                                  "Patient Advised - indefinitely unavailable")) |> 
  mutate(last_unavail_reset = Unavail_End_Date+1) |> 
  select(MUI, CHI, last_unavail_reset)


non_attendances <- waits |>
  left_join(offers, by = c("MUI", "CHI")) |> 
  filter(Non_Attendance_Category_Description %in% c("Could Not Attend",
                                                    "Did Not Attend")) |> 
  select(MUI, CHI, last_non_attendance = Non_Attendance_Date)

non_attendances_max <- non_attendances |> 
  group_by(MUI, CHI) |> 
  filter(last_non_attendance == max(last_non_attendance)) |> 
  ungroup()

declined_pairs2 <- waits |>
  left_join(offers, by = c("MUI", "CHI")) |> 
  left_join(non_attendances_max, by = c("MUI","CHI")) |> 
  mutate(last_non_attendance = if_else(is.na(last_non_attendance),
                                       dmy("01/01/1900"),last_non_attendance)) |> 
  mutate(
    rejected_reasonable = if_else(
      (`Appt/Adm_Date` - Offer_Date >= 7) & 
        str_detect(Offer_Outcome_Description, "Declined"),
      "rejected reasonable", "not rejected reasonable")) |> 
  filter(`Appt/Adm_Date` - Offer_Date >= 7,
         !is.na(Offer_Outcome_Description)) |>  # get rid of irrelevant offers
  arrange(MUI, CHI, desc(Offer_Order)) |> 
  group_by(MUI, CHI) |> 
  mutate(
    declined_pair = if_else(rejected_reasonable == "rejected reasonable" &
                              lag(rejected_reasonable, default = "not rejected reasonable") == "rejected reasonable" &
                              !(last_non_attendance > lag(Response_Rcvd_Date) &
                                  last_non_attendance <= Response_Rcvd_Date) &
                              Urgency_Category != "Urgent", 1, 0)) |>
  # mutate(declined_pair = if_else(lag(declined_pair, default = 0) == 1, 0, declined_pair)) |>
  filter(declined_pair == 1) |> 
  mutate(remove = row_number() %% 2 == 0) |> 
  filter(remove == FALSE) |> 
  #filter(Response_Rcvd_Date == max(Response_Rcvd_Date)) |> 
  ungroup() |> 
  select(MUI, CHI, last_rejection = Response_Rcvd_Date)

clock_resets <- bind_rows(declined_pairs2, non_attendances, unavail_resets) |> 
  pivot_longer(c("last_rejection", "last_non_attendance", "last_unavail_reset")) |> 
  group_by(MUI, CHI) |> 
  summarise(value = max(value, na.rm = TRUE)) |> 
  ungroup() |> 
  rename(last_reset = value) |> 
  filter(!is.na(last_reset))

waits <- waits |> 
  left_join(clock_resets, by = c("MUI", "CHI"))


#### Step 2 : unavailability ----

unavail <- waits |>
  mutate(Effective_Start_Date = if_else(is.na(last_reset),
                                        Init_Start_Date,
                                        last_reset)) |> 
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

waits <- waits |>
  mutate(Effective_Start_Date = if_else(is.na(last_reset),
                                        Init_Start_Date,
                                        last_reset)) |> 
  left_join(unavail, by = c("MUI", "CHI")) |>
  mutate(total_unavailability = replace_na(total_unavailability,0))


#### Step 3 : Final wait calculation ----

# waits <- waits |>
#   mutate(
#     Effective_Start_Date = ymd(Effective_Start_Date),
#     last_reset = ymd(last_reset)) |>
#   mutate(new_effective_start_date = if_else(is.na(last_reset),
#                                             Init_Start_Date,
#                                             last_reset)) |>
#   mutate(new_wait_length = target_date-days(total_unavailability)-new_effective_start_date) |>
#   mutate(new_wait_length = if_else(new_wait_length < 0, 0,
#                                    as.numeric(new_wait_length))) |>
#   rename(old_wait_length = Number_of_waiting_list_days)

waits <- waits |>
  mutate(
    Effective_Start_Date = ymd(Effective_Start_Date),
    last_reset = ymd(last_reset)) |>
  mutate(new_effective_start_date = if_else(is.na(last_reset),
                                            Init_Start_Date,
                                            last_reset)) |>
  mutate(new_wait_length = List_removal_date-days(total_unavailability)-new_effective_start_date) |>
  mutate(new_wait_length = if_else(new_wait_length < 0, 0,
                                   as.numeric(new_wait_length))) |>
  rename(old_wait_length = Number_of_waiting_list_days)

resets_beyond_12 <- waits |> 
  select(MUI, CHI,
         length_resets_beyond_12 = new_wait_length)
