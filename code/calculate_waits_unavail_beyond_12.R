# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# calculate_waits_unavail_beyond_12.R
# Angus Morton
# 2024-11-11
# 
# Calculate waits factoring in unavailability beyond 12 weeks
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

non_attendances <- waits |>
  left_join(offers, by = c("MUI", "CHI")) |> 
  filter(Non_Attendance_Category_Description %in% c("Could Not Attend",
                                                    "Did Not Attend")) |> 
  select(MUI, CHI, last_non_attendance = Non_Attendance_Date)

declined_pairs <- waits |>
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

all_clock_resets <- bind_rows(declined_pairs, non_attendances) |> 
  pivot_longer(c("last_rejection", "last_non_attendance")) |> 
  select(-name) |> 
  rename(reset_date = value) |> 
  filter(!is.na(reset_date))

waits_all_resets <- waits |> 
  left_join(all_clock_resets, by = c("MUI", "CHI"))

# Find clock resets within 12 factoring in unavailability

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

waits <- waits |> 
  left_join(resets_within_12, by = c("MUI", "CHI")) |>
  mutate(last_reset = if_else(is.na(reset_date), Init_Start_Date,
                              reset_date))


#### Step 2 : unavailability ----

unavail <- waits |> 
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

waits <- waits |>
  left_join(unavail, by = c("MUI", "CHI")) |>
  mutate(total_unavailability = replace_na(total_unavailability,0))


# Step 3 : Final wait calculation ----

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

