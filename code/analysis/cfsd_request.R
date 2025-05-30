# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# cfsd_request.R
# Angus Morton
# 2025-05-23
# 
# Analysis for the outputs requested by cfsd
# 
# R version 4.4.2 (2024-10-31)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Step 0 : Housekeeping ----
library(readr)
library(dplyr)
library(openxlsx)

#### Step 1 : Imports ----

run_name <- "qe_mar_25"

waits <- read_rds(paste0("output/", run_name,
                         "/waits.rds"))

#### Step 2 : Clean data ----

waits <- waits |> 
  select(Patient_Type, NHS_Board_of_Treatment, Specialty,
         length_all_old_rules, length_all_new_rules) |> 
  mutate(
    length_group_old = case_when(
    between(length_all_old_rules, 281, 364) ~ "40 to 52",
    length_all_old_rules > 364 ~ "over 52"),
    length_group_new = case_when(
      between(length_all_new_rules  , 281, 364) ~ "40 to 52",
      length_all_new_rules > 364 ~ "over 52"))


#### Step 3 : Add specialty groupings ----

ipdc_groupings <- read.xlsx("spec_groupings/IPDC.xlsx")
nop_groupings <- read.xlsx("spec_groupings/NOP.xlsx")

groupings <- ipdc_groupings |> 
  full_join(nop_groupings, by = "Specialty") |> 
  mutate(grouped_specialty.x = if_else(is.na(grouped_specialty.x),
                                       grouped_specialty.y,
                                       grouped_specialty.x)) |> 
  select(Specialty, grouped_specialty = grouped_specialty.x)

waits <- waits |> 
  left_join(groupings, by = "Specialty")

#### Step 4 : Ungrouped spec ----

old <- waits |> 
  group_by(Patient_Type, NHS_Board_of_Treatment, Specialty,
           length_group_old) |> 
  summarise(old = n()) |> 
  ungroup() |> 
  filter(!is.na(length_group_old)) |> 
  rename(length_group = length_group_old)

new <- waits |> 
  group_by(Patient_Type, NHS_Board_of_Treatment, Specialty,
           length_group_new) |> 
  summarise(new = n()) |> 
  ungroup() |> 
  filter(!is.na(length_group_new)) |> 
  rename(length_group = length_group_new)

ungrouped_specs <- full_join(old, new, by = c("Patient_Type","NHS_Board_of_Treatment",
                                              "Specialty","length_group")) |> 
  mutate(old = replace_na(old, 0),
         new = replace_na(new, 0)) |> 
  mutate(diff = new-old,
         diff_p = 100*(new-old)/old)

#### Step 5 : grouped spec ----

old <- waits |> 
  group_by(Patient_Type, NHS_Board_of_Treatment, grouped_specialty,
           length_group_old) |> 
  summarise(old = n()) |> 
  ungroup() |> 
  filter(!is.na(length_group_old)) |> 
  rename(length_group = length_group_old)

new <- waits |> 
  group_by(Patient_Type, NHS_Board_of_Treatment, grouped_specialty,
           length_group_new) |> 
  summarise(new = n()) |> 
  ungroup() |> 
  filter(!is.na(length_group_new)) |> 
  rename(length_group = length_group_new)

grouped_specs <- full_join(old, new, by = c("Patient_Type","NHS_Board_of_Treatment",
                                              "grouped_specialty","length_group")) |> 
  mutate(old = replace_na(old, 0),
         new = replace_na(new, 0)) |> 
  mutate(diff = new-old,
         diff_p = 100*(new-old)/old)

#### Step x : write out ----

write_csv(ungrouped_specs, paste0("output/", run_name, "/",
                                  "cfsd_board_spec_breakdown_ungrouped.csv"))
write_csv(grouped_specs, paste0("output/", run_name, "/",
                                  "cfsd_board_spec_breakdown_gropued.csv"))
