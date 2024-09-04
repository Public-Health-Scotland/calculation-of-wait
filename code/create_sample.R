# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# create_sample.R
# Angus Morton
# 2024-08-22
# 
# While the boxi is being created this pulls data extracted from the
# MUI audit BOXI report
# 
# R version 4.1.2 (2021-11-01)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Step 0 : Housekeeping ----

source("code/settings.R")

library(readr)
library(dplyr)
library(lubridate)
library(tidylog)
library(janitor)
library(purrr)

#### Step 1 : function ----

df1_cols <- c("Mandatory Unique Identifier",
              "Init Start Date",
              "Effective Start Date")

df2_cols <- c("Unavail Start Date",
              "Unavail End Date",
              "Unavailability Reason Description")

df3_cols <- c("Non Attendance Category Description",
              "Non Attendance Date",
              "Offer Order")

reformat_report <- function(name) {
  
  df <- read_csv(paste0(mui_audit, name),
                 col_names = FALSE)
  
  g2 <- which(df$X1 == "Waiting Target Case")
  g3 <- which(df$X1 == "Cardiac Flag")
  g4 <- which(df$X1 == "Error Severity")
  
  tables <- c(rep(1,(g2-1)), rep(2,(g3-g2)), rep(3,(g4-g3)), rep(4, (nrow(df)+1-g4)))
  
  df <- df |> mutate(group = tables)
  
  df1 <- df |> 
    filter(group == 1) |> 
    row_to_names(row_number = 1) |> 
    select(df1_cols)
  
  if (nrow(df1) == 0) {
    
    df1 <- add_row(df1)
    
  }
  
  df2 <- df |> 
    filter(group == 2) |> 
    row_to_names(row_number = 1) |> 
    select(df2_cols) |> 
    mutate(row = row_number()) |> 
    pivot_wider(names_from = row, values_from = df2_cols)
  
  if (nrow(df2) == 0) {
    
    df2 <- add_row(df2)
    
  }
  
  df3 <- df |> 
    filter(group == 3) |> 
    row_to_names(row_number = 1) |> 
    select(df3_cols) |> 
    mutate(row = row_number()) |> 
    pivot_wider(names_from = row, values_from = df3_cols)
  
  if (nrow(df3) == 0) {
    
    df3 <- add_row(df3)
    
  }
  
  
  df <- bind_cols(df1, df2, df3)
  
  df
  
}


#### Step x : run function ----

# Run function for each file in the MUI audit folder
muis <- list.files(path = mui_audit)

waits <- map(muis, reformat_report)

waits <- bind_rows(waits)

#### Step x : export ----

write_csv(waits, "MUIs/sample.csv")

