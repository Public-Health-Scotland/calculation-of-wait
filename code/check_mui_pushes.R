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
library(writexl)

#### Edit Filepaths ----

csv_folder <- "MUIs/ongoing_cutover/"

#
#boxi_extract <- "MUIs/ongoing_waits_live.xlsx"
#

#### Imports ----
source("code/imports/import_csvs.R")

# Replicates existing rules and exports the non matching mui-chi pairs
source("code/wait_calculation/all_old_rules.R")

# Implement all new rule changes at once
source("code/wait_calculation/all_new_rules.R")


#### Export count of matches ----

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


#### Export non-matching MUIs ----
# Save the mui lists for waits that didn't match between the warehouse and new calculation method applied. 

# Get non-matching waits and calculate difference in wait length
non_matching_new_wh <- waits_old_new_wh |> 
  mutate(same_old_new = length_all_old_rules == length_all_new_rules,
         same_new_wh = length_all_new_rules == length_wh,
         same_old_wh = length_all_old_rules == length_wh) |>
  filter(!same_new_wh) |>
  mutate(diff_new_wh = length_wh - length_all_new_rules)

# Get count of wait calculations used in non-matching records
non_matching_new_wh_summary <- non_matching_new_wh |>
  group_by(NHS_Board_of_Treatment, Wait_Calculation_Used) |>
  summarise(n = n())

# Save to output
write_xlsx(x = list("MUI List" = non_matching_new_wh, 
                    "Wait Calculations" = non_matching_new_wh_summary), 
           path = paste0("output/", Sys.Date(), "_non_matching_new_wh.xlsx"))

