# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# format_non_matches.R
# Angus Morton
# 2025-10-02
# 
# Create easy to read output to investigate non matches
# 
# R version 4.4.2 (2024-10-31)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Step 0 : Housekeeping ----

library(readr)
library(dplyr)

#### Step 1 : Import data ----

waits <- read_csv("temp/comp_25_months/non_matching_waits_r.csv")
unavail <- read_csv("temp/comp_25_months/non_matching_unavail.csv")
offers <- read_csv("temp/comp_25_months/non_matching_offers.csv")

###
waits <- non_matching_waits_r
unavail <- non_matching_unavail
offers <- non_matching_offers
###

#### Step 2 : Select columns ----

waits <- waits |> 
  select(MUI, CHI, Date, Init_Start_Date, List_removal_date,
         Urgency_Category, Wait_Calculation_Used, WTS,
         Wait_wh = Number_of_waiting_list_days,
         Wait_r = length_all_old_rules,
         NHS_Board_of_Treatment)

unavail <- unavail |> 
  select(MUI, CHI, Unavail_Start_Date, Unavail_End_Date,
         Number_Days_Unavailable,
         Unavailability_Reason_Description)

offers <- offers |> 
  arrange(desc(Offer_Order)) |> 
  select(MUI, CHI, Offer_Order,
         Offer_Date,
         App_date = `Appt/Adm_Date`,
         Response_date = Response_Rcvd_Date,
         Offer_Outcome_Description,
         NA_date = Non_Attendance_Date,
         NA_cat = Non_Attendance_Category_Description)

#### Step 3 : Combine ----

w_offers <- waits |> 
  left_join(offers, by = c("MUI","CHI"))

w_unavail <- waits |> 
  left_join(unavail, by = c("MUI","CHI"))

all <- bind_rows(w_offers, w_unavail) |> 
  arrange(CHI, MUI, desc(Offer_Order)) |> 
  mutate(notice = App_date-Offer_Date) |> 
  relocate(notice, .after = App_date)

chis <- all |> 
  select(CHI) |> 
  distinct()

#### Step 5 : Save out

write_csv(all, "temp/comp_25_months/check_overnight.csv")
write_csv(chis, "temp/comp_25_months/notes_overnight.csv")
