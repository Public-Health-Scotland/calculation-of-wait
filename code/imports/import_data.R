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
  select(-c("Ongoing/Completed", "Number_Seen/On_list")) |> 
  mutate(Urgency_Category = case_when(
    Urgency_Category %in% c("Routine",
                            "Soon",
                            "Priority 3a <12 weeks",
                            "Priority 4a >12 weeks") ~ "Routine",
    Urgency_Category %in% c("Urgent",
                            "Priority 1a <24 hours",
                            "Priority 2a <4 weeks") ~ "Urgent",
    TRUE ~ "Not Known"
  ))

unavail_init <- read.xlsx(boxi_extract, sheet = "Unavailability",
                          sep.names = "_",
                          detectDates = TRUE) |> 
  select(-c("Patient_Type_Cohort_Description",
            "Sending_Location_Code")) |> 
  rename(CHI = Pat_CHI_Number,
         MUI = Mandatory_Unique_Identifier)


# Read the initial Appointments & Offers sheet with column names
offers_init <- read.xlsx(boxi_extract, sheet = "Appointments & Offers",
                         sep.names = "_",
                         detectDates = TRUE)

# Save the column names for consistency
offers_names <- names(offers_init)

# Clean the initial sheet
offers_init <- offers_init |>
  select(-Patient_Type_Cohort_Description) |>
  rename(CHI = Pat_CHI_Number,
         MUI = Mandatory_Unique_Identifier)

# Get all sheet names
sheet_names <- getSheetNames(boxi_extract)

# Get any additional "Appointments & Offers" sheets
additional_offers_sheets <- sheet_names[-(1:3)]

# If additional sheets exist, pull the data, clean and combine with offers_init.
if (length(additional_offers_sheets) > 0) {
  
  read_additional_offers <- function(sheet) {
    df <- read.xlsx(boxi_extract, sheet = sheet,
                    colNames = FALSE,
                    detectDates = TRUE)
    names(df) <- offers_names
    df |>
      select(-Patient_Type_Cohort_Description) |>
      rename(CHI = Pat_CHI_Number,
             MUI = Mandatory_Unique_Identifier)
  }
  
  # Read and clean each additional sheet
  additional_offers_data <- lapply(additional_offers_sheets, read_additional_offers) |>
    bind_rows()
  
  # Combine all data
  offers_init <- bind_rows(offers_init, additional_offers_data)
  
}

# Remove duplicated offers dataframe with the extra sheets
rm(additional_offers_data)


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

