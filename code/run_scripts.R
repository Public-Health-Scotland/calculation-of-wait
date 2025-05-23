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
boxi_extract <- "MUIs/completed_waits_dec24.xlsx"

# out
run_name <- "uat_muis_completed"

#### Implement Rules ----
source("code/imports/import_data_completed.R")

# Replicates existing rules and exports the non matching mui-chi pairs
source("code/wait_calculation/all_old_rules.R")

# Implement each rule change separately
source("code/wait_calculation/reasonable_offer.R")
source("code/wait_calculation/unavail_beyond_12.R")
source("code/wait_calculation/resets_beyond_12.R")
source("code/wait_calculation/no_urgency.R")

# Implement all new rule changes at once
source("code/wait_calculation/all_new_rules.R")

#### Save data ----

# Filter out any records whose wait can't be replicated 
waits_final <- waits_init |> 
  rename(length_all_old_rules = Number_of_waiting_list_days) |> 
  anti_join(non_matching_chis, by = c("MUI","CHI")) |> 
  left_join(reasonable_offer, by = c("MUI","CHI")) |>
  left_join(unavail_beyond_12, by = c("MUI","CHI")) |>
  left_join(resets_beyond_12, by = c("MUI","CHI")) |>
  left_join(no_urgency, by = c("MUI","CHI")) |>
  left_join(all_new_rules, by = c("MUI","CHI"))


dir.create(paste0("temp/", run_name))
dir.create(paste0("output/", run_name))

write_rds(non_matching_chis, paste0("temp/", run_name,
                                    "/non_matching_chis.rds"))
write_rds(waits_final, paste0("output/", run_name,
                              "/waits.rds"))


# analysis (gitea back up)
source("code/analysis.R")



