# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# run_scripts
# Angus Morton
# 2024-11-11
# 
# Run each of the scripts for the new calculation of wait
# 
# R version 4.1.2 (2021-11-01)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Packages ----
library(readr)
library(dplyr)

#### Edit Filepaths ----

# in 
csv_folder <- "MUIs/WTs guidance review ongoing waits snapshot - August/"

# out
run_name <- "aug_25"

#### Implement Rules ----
# source("code/imports/import_csvs.R")
source("code/imports/import_csvs.R")

###
non_matching_chis <- read_csv("temp/comp_25_months/non_matching_chis.csv") |>
  select(MUI, CHI) |>
  mutate(MUI = as.character(MUI)) |>
  distinct()

waits_init <- waits_init |>
  filter(CHI %in% non_matching_chis$CHI,
         MUI %in% non_matching_chis$MUI)
offers_init <- offers_init |>
  filter(CHI %in% non_matching_chis$CHI,
         MUI %in% non_matching_chis$MUI)
unavail_init <- unavail_init |>
  filter(CHI %in% non_matching_chis$CHI,
         MUI %in% non_matching_chis$MUI)

waits_init <- waits_init |> 
  filter(List_removal_date >= dmy("01-04-2025"))
###

# Replicates existing rules and exports the non matching mui-chi pairs
source("code/wait_calculation/all_old_rules.R")

# Implement each rule change separately
# source("code/wait_calculation/reasonable_offer.R")
# source("code/wait_calculation/unavail_beyond_12.R")
# source("code/wait_calculation/resets_beyond_12.R")
# source("code/wait_calculation/no_urgency.R")

# Implement all new rule changes at once
source("code/wait_calculation/all_new_rules.R")

#### Save data ----

waits_final <- waits_init |> 
  left_join(all_old_rules, by = c("MUI","CHI")) |> 
  left_join(all_new_rules, by = c("MUI","CHI"))

# Filter out any records whose wait can't be replicated 
waits_final <- waits_init |> 
  left_join(all_old_rules, by = c("MUI","CHI")) |> 
  filter(Wait_Calculation_Used != 2023 |
           is.na(Wait_Calculation_Used))

non_matching_chis <- waits_final |> 
  filter(Number_of_waiting_list_days != length_all_old_rules) |> 
  select(MUI, CHI)

non_matching_waits_r <- waits_final |> 
  filter(Number_of_waiting_list_days != length_all_old_rules) 

non_matching_waits <- waits_init |> 
  right_join(non_matching_chis, by = c("MUI","CHI"))

non_matching_unavail <- unavail_init |> 
  right_join(non_matching_chis, by = c("MUI","CHI"))

non_matching_offers <- offers_init |> 
  right_join(non_matching_chis, by = c("MUI","CHI"))

write_csv(non_matching_chis, paste0("temp/", run_name,"/non_matching_chis.csv"))
write_csv(non_matching_waits_r, paste0("temp/", run_name,"/non_matching_waits_r.csv"))
write_csv(non_matching_waits, paste0("temp/", run_name,"/non_matching_waits.csv"))
write_csv(non_matching_offers, paste0("temp/", run_name,"/non_matching_offers.csv"))
write_csv(non_matching_unavail, paste0("temp/", run_name,"/non_matching_unavail.csv"))

# For completed waits WTS = 0 
waits_final <- waits_final |> 
  mutate(Number_of_waiting_list_days = if_else(Patient_Type == "Inpatient/Day case" &
                                                 WTS == "023", 0, Number_of_waiting_list_days),
         length_all_old_rules = if_else(Patient_Type == "Inpatient/Day case" &
                                                 WTS == "023", 0, length_all_old_rules),
         length_all_new_rules = if_else(Patient_Type == "Inpatient/Day case" &
                                                 WTS == "023", 0, length_all_new_rules))

dir.create(paste0("temp/", run_name))
dir.create(paste0("output/", run_name))

write_rds(non_matching_chis, paste0("temp/", run_name,
                                    "/non_matching_chis.rds"))
write_rds(waits_final, paste0("output/", run_name,
                              "/waits.rds"))


# main analysis
source("code/analysis/analysis.R")

# analysis with all specialties
source("code/analysis/analysis_spec_detailed.R")


# Publication report table for old vs new length of wait comparison
source("code/analysis/publication_table.R")



