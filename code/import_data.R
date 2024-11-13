# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# import_data.R
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

unavail_init <- unavail_init |> 
  filter(Unavail_Start_Date <= target_date) |> 
  mutate(Unavail_End_Date = if_else(Unavail_End_Date > target_date,
                                    target_date, Unavail_End_Date)) |>
  mutate(Number_Days_Unavailable = Unavail_End_Date - Unavail_Start_Date)

offers_init <- offers_init |> 
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

