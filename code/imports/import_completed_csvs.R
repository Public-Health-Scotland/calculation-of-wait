# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# import_data_completed.R
# Angus Morton
# 2024-11-11
# 
# Import BOXI data and do required filtering
# 
# R version 4.1.2 (2021-11-01)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Step 0 : Housekeeping ----

library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(openxlsx)

#### Step 1 : read in BOXI data ----

waits_init <- read_csv(paste0(csv_folder, "Completed.csv"),
                       skip = 1,
                       col_names = c("Patient_Type",
                                     "Ongoing/Completed",
                                     "NHS_Board_of_Treatment",
                                     "Specialty",
                                     "Date",
                                     "Number Seen/On list",
                                     "MUI",
                                     "CHI",
                                     "List_removal_date",
                                     "Init_Start_Date",
                                     "Effective_Start_Date",
                                     "Days_on_Waiting_List",
                                     "Urgency_Category",
                                     "Wait_Calculation_Used",
                                     "WTS"),
                       col_types = "ccccccccccccc") |> 
  select(-c("Ongoing/Completed", "Number Seen/On list")) |> 
  mutate(
    Urgency_Category = case_when(
      Urgency_Category %in% c("Routine",
                              "Soon",
                              "Priority 3a <12 weeks",
                              "Priority 4a >12 weeks") ~ "Routine",
      Urgency_Category %in% c("Urgent",
                              "Priority 1a <24 hours",
                              "Priority 2a <4 weeks") ~ "Urgent",
      TRUE ~ "Not Known"
    ),
  Init_Start_Date = dmy(Init_Start_Date),
  Effective_Start_Date = dmy(Effective_Start_Date),
  List_removal_date = dmy(List_removal_date),
  Days_on_Waiting_List = as.numeric(str_remove(Days_on_Waiting_List,",")))

unavail_init <- read_csv(paste0(csv_folder, "Unavailability.csv"),
                         skip = 1,
                         col_names = c("CHI",
                                       "Patient_Type_Cohort_Description",
                                       "MUI",
                                       "Unavail_Start_Date",
                                       "Unavail_End_Date",
                                       "Unavailability_Reason_Description",
                                       "Number_Days_Unavailable",
                                       "Patient_Type_Description",
                                       "Sending_Location_Code"),
                         col_types = "cccccccc") |> 
  select(-c("Patient_Type_Cohort_Description",
            "Sending_Location_Code")) |> 
  mutate(Unavail_Start_Date = dmy(Unavail_Start_Date),
         Unavail_End_Date = dmy(Unavail_End_Date))

offers_init <- read_csv(paste0(csv_folder, "Appointments & Offers.csv"),
                        skip = 1,
                        col_names = c("CHI",
                                      "Patient_Type_Cohort_Description",
                                      "MUI",
                                      "Appt/Adm_Date",
                                      "Non_Attendance_Category_Description",
                                      "Non_Attendance_Date",
                                      "Non_Attendance_Outcome_Description",
                                      "Offer_Date",
                                      "Offer_Outcome_Description",
                                      "Offer_Type_Description",
                                      "Response_Rcvd_Date",
                                      "Offer_Order",
                                      "Sending_Location_Code",
                                      "Patient_Type_Description"),
                        col_types = "cccccccccccccccc") |> 
  select(-Patient_Type_Cohort_Description) |> 
  mutate(`Appt/Adm_Date` = dmy(`Appt/Adm_Date`),
         Non_Attendance_Date = dmy(Non_Attendance_Date),
         Offer_Date = dmy(Offer_Date),
         Response_Rcvd_Date = dmy(Response_Rcvd_Date))

#### Step 2 : trim off post target data ----
# If running multiple times run from here to save you reading in the files
# multiple times

list_removals <- waits_init |> 
  select(MUI, CHI, List_removal_date)


unavail_init <- unavail_init |> 
  left_join(list_removals, by = c("MUI","CHI")) |> 
  filter(Unavail_Start_Date <= List_removal_date) |> 
  mutate(Unavail_End_Date = if_else(Unavail_End_Date > List_removal_date,
                                    List_removal_date, Unavail_End_Date)) |>
  mutate(Number_Days_Unavailable = Unavail_End_Date - Unavail_Start_Date)

offers_init <- offers_init |> 
  left_join(list_removals, by = c("MUI","CHI")) |> 
  filter(Offer_Date <= List_removal_date) |> 
  mutate(
    Non_Attendance_Category_Description = if_else(Non_Attendance_Date > List_removal_date,
                                                  NA, Non_Attendance_Category_Description),
    
    Non_Attendance_Outcome_Description = if_else(Non_Attendance_Date > List_removal_date,
                                                 NA, Non_Attendance_Outcome_Description),
    
    Non_Attendance_Date = if_else(Non_Attendance_Date > List_removal_date,
                                  NA, Non_Attendance_Date),
    
    Offer_Type_Description = if_else(Offer_Date > List_removal_date,
                                     NA, Offer_Type_Description),
    
    Offer_Order = if_else(Offer_Date > List_removal_date,
                          NA, Offer_Order),
    
    Offer_Outcome_Description = if_else(Response_Rcvd_Date > List_removal_date,
                                        NA, Offer_Outcome_Description),
    
    Offer_Date = if_else(Offer_Date > List_removal_date,
                         NA, Offer_Date))


waits_init <- waits_init |> 
  #filter(Date == "31/12/2024") |> 
  rename(Number_of_waiting_list_days = Days_on_Waiting_List)
