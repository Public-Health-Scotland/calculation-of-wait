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

boxi_extract <- "MUIs/ongoing_waits_live.xlsx"


#### Imports ----
source("code/imports/import_data.R")

# Replicates existing rules and exports the non matching mui-chi pairs
source("code/wait_calculation/all_old_rules.R")

# Implement all new rule changes at once
source("code/wait_calculation/all_new_rules.R")


#### Save data ----

# Filter out any records whose wait can't be replicated 
waits_old_new_wh <- waits_init |> 
  rename(length_wh = Number_of_waiting_list_days) |> 
  left_join(all_old_rules, by = c("MUI","CHI")) |> 
  left_join(all_new_rules, by = c("MUI","CHI"))


matches <- waits_old_new_wh |> 
  mutate(same_old_new = length_all_old_rules == length_all_new_rules,
         same_new_wh = length_all_new_rules == length_wh,
         same_old_wh = length_all_old_rules == length_wh) |> 
  count(NHS_Board_of_Treatment, same_new_wh) |> 
  pivot_wider(names_from = same_new_wh, values_from = n)

write_csv(matches, paste0("output/", Sys.Date(), "_matches.csv"))



