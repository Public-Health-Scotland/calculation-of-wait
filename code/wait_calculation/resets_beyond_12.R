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

last_declined_pairs <- waits |>
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


clock_resets <- last_declined_pairs |> 
  full_join(last_non_attendances, by = c("MUI", "CHI")) |> 
  pivot_longer(last_rejection:last_non_attendance) |> 
  group_by(MUI, CHI) |> 
  summarise(value = max(value, na.rm = TRUE)) |> 
  ungroup() |> 
  rename(last_reset = value)

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

waits <- waits |>
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


