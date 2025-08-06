# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# publication_table.R
# Ross Burns
# 2025-07-30
# 
# Produce the publication table showing board comparison for 
# the new vs old calculation impact on length of wait:median, 90th, >52 weeks
# 
# 
# R version 4.1.2 (2021-11-01)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Step 0 : Housekeeping ----
library(readr)
library(dplyr)

# Optionally overwrite run_name
#run_name <- "qe_jun_25"

#### Step 1 : join NOP & IPDC summary tables ----

# Join rows for the board NOP and IPDC LoW statistics, using the outputs produced by analysis.R script
board_comparison <- bind_rows(
  read_csv(paste0("output/", run_name, "/NOP_length_all_new_rules_board_medians.csv")) |>
    mutate(patient_type = "New Outpatient", .before = 1),
  read_csv(paste0("output/", run_name, "/IPDC_length_all_new_rules_board_medians.csv")) |>
    mutate(patient_type = "Inpatient/Day case", .before = 1)
) |>
  # Rename columns
  rename("board" = "NHS_Board_of_Treatment", "90th_new" = "90th new", "90th_old" = "90th old") |>
  # Move percentage diff columns beside associated indicators
  relocate("med_diff_p", .after = "median_old") |>
  relocate("90th_diff_p", .after = "90th_old")


# Repeat for Scotland figures
scot_comparison <- bind_rows(
  read_csv(paste0("output/", run_name, "/NOP_length_all_new_rules_scotland_medians.csv")) |>
    mutate(patient_type = "New Outpatient", .before = 1),
  read_csv(paste0("output/", run_name, "/IPDC_length_all_new_rules_scotland_medians.csv")) |>
    mutate(patient_type = "Inpatient/Day case", .before = 1)
) |>
  # Rename columns
  rename("90th_new" = "90th new", "90th_old" = "90th old") |>
  # Move percentage diff columns beside associated indicators
  relocate("med_diff_p", .after = "median_old") |>
  relocate("90th_diff_p", .after = "90th_old")

#### Step 2 : write tables ----

# Create folder for publication
dir.create(paste0("output/", run_name, "/publication"))

# Save in output publication folder
write_csv(board_comparison, paste0("output/", run_name, "/publication/board_publication_table.csv"))
write_csv(scot_comparison, paste0("output/", run_name, "/publication/scot_publication_table.csv"))





