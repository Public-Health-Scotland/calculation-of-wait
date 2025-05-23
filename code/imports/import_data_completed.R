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

waits_init <- read.xlsx(boxi_extract, sheet = "Completed",
                        sep.names = "_",
                        detectDates = TRUE)

waits_names <- names(waits_init)

waits_init <- waits_init |> 
  select(-c("Ongoing/Completed", "Number_Seen/On_list"))

waits_init_1 <- read.xlsx(boxi_extract, sheet = "Completed(1)",
                        sep.names = "_",
                        detectDates = TRUE)

names(waits_init_1) <- waits_names 

waits_init_1 <- waits_init_1 |> 
  select(-c("Ongoing/Completed", "Number_Seen/On_list"))

waits_init <- bind_rows(waits_init, waits_init_1)

waits_init <- waits_init |> 
  filter(Date == "31/12/2024") |> 
  rename(Number_of_waiting_list_days = Days_on_Waiting_List)

waits_init <- waits_init |> 
  rename(target_date = List_removal_date)

unavail_init <- read.xlsx(boxi_extract, sheet = "Unavailability",
                          sep.names = "_",
                          detectDates = TRUE) |> 
  select(-c("Patient_Type_Cohort_Description",
            "Sending_Location_Code")) |> 
  rename(CHI = Pat_UPI,
         MUI = Mandatory_Unique_Identifier)

offers_init <- read.xlsx(boxi_extract, sheet = "Appointments & Offers",
                         sep.names = "_",
                         detectDates = TRUE) |>
  select(-c(16))

offers_names <- names(offers_init)

offers_init <- offers_init |> 
  mutate(Urgency_Category = case_when(
    Urgency_Category_Description %in% c("Routine",
                                        "Soon",
                                        "Priority 3a <12 weeks",
                                        "Priority 4a >12 weeks") ~ "Routine",
    Urgency_Category_Description %in% c("Urgent",
                                        "Priority 1a <24 hours",
                                        "Priority 2a <4 weeks") ~ "Urgent",
    TRUE ~ "Not Known"
  )) |> 
  select(-Urgency_Category_Description)
  

offers_init <- offers_init |> 
  select(-Patient_Type_Cohort_Description) |> 
  rename(CHI = Pat_UPI,
         MUI = Mandatory_Unique_Identifier)

offers_init_1 <- read.xlsx(boxi_extract, sheet = "Appointments & Offers(1)",
                           colNames = FALSE,
                           detectDates = TRUE) |> 
  select(-c(16))

names(offers_init_1) <- offers_names

offers_init_1 <- offers_init_1 |> 
  mutate(Urgency_Category = case_when(
    Urgency_Category_Description %in% c("Routine",
                                        "Soon",
                                        "Priority 3a <12 weeks",
                                        "Priority 4a >12 weeks") ~ "Routine",
    Urgency_Category_Description %in% c("Urgent",
                                        "Priority 1a <24 hours",
                                        "Priority 2a <4 weeks") ~ "Urgent",
    TRUE ~ "Not Known"
  )) |> 
  select(-Urgency_Category_Description)



offers_init_1 <- offers_init_1 |> 
  select(-Patient_Type_Cohort_Description) |> 
  rename(CHI = Pat_UPI,
         MUI = Mandatory_Unique_Identifier)

offers_init_2 <- read.xlsx(boxi_extract, sheet = "Appointments & Offers(2)",
                           colNames = FALSE,
                           detectDates = TRUE) |> 
  select(-c(16))

names(offers_init_2) <- offers_names

offers_init_2 <- offers_init_2 |> 
  mutate(Urgency_Category = case_when(
    Urgency_Category_Description %in% c("Routine",
                                        "Soon",
                                        "Priority 3a <12 weeks",
                                        "Priority 4a >12 weeks") ~ "Routine",
    Urgency_Category_Description %in% c("Urgent",
                                        "Priority 1a <24 hours",
                                        "Priority 2a <4 weeks") ~ "Urgent",
    TRUE ~ "Not Known"
  )) |> 
  select(-Urgency_Category_Description)

offers_init_2 <- offers_init_2 |> 
  select(-Patient_Type_Cohort_Description) |> 
  rename(CHI = Pat_UPI,
         MUI = Mandatory_Unique_Identifier)

offers_init_3 <- read.xlsx(boxi_extract, sheet = "Appointments & Offers(3)",
                           colNames = FALSE,
                           detectDates = TRUE) |> 
  select(-c(16))

names(offers_init_3) <- offers_names

offers_init_3 <- offers_init_3 |> 
  mutate(Urgency_Category = case_when(
    Urgency_Category_Description %in% c("Routine",
                                        "Soon",
                                        "Priority 3a <12 weeks",
                                        "Priority 4a >12 weeks") ~ "Routine",
    Urgency_Category_Description %in% c("Urgent",
                                        "Priority 1a <24 hours",
                                        "Priority 2a <4 weeks") ~ "Urgent",
    TRUE ~ "Not Known"
  )) |> 
  select(-Urgency_Category_Description)


offers_init_3 <- offers_init_3 |> 
  select(-Patient_Type_Cohort_Description) |> 
  rename(CHI = Pat_UPI,
         MUI = Mandatory_Unique_Identifier)

offers_init <- bind_rows(offers_init, offers_init_1, offers_init_2,
                         offers_init_3)

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
  filter(Date == "31/12/2024") |> 
  rename(Number_of_waiting_list_days = Days_on_Waiting_List)
