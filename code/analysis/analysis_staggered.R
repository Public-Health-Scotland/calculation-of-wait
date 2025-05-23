# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# analysis_staggered.R
#
# Angus Morton
# 2025-03-04
# 
# Create outputs for staggerred switch on of new rules
# 
# R version 4.1.2 (2021-11-01)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(readr)
library(networkD3)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(openxlsx)
library(phsstyles)

run_name <- "qe_mar_25"

waits <- read_rds(paste0("output/", run_name,
                         "/waits.rds"))

ipdc_groupings <- read.xlsx("spec_groupings/IPDC.xlsx")
nop_groupings <- read.xlsx("spec_groupings/NOP.xlsx")

groupings <- ipdc_groupings |> 
  full_join(nop_groupings, by = "Specialty") |> 
  mutate(grouped_specialty.x = if_else(is.na(grouped_specialty.x),
                                       grouped_specialty.y,
                                       grouped_specialty.x)) |> 
  select(Specialty, grouped_specialty = grouped_specialty.x)

initial_boards <- c("NHS Ayrshire & Arran",
                    "NHS Dumfries & Galloway",
                    "NHS Forth Valley",
                    "NHS Highland",
                    "NHS Lothian",
                    "NHS Western Isles")

waits <- waits |> 
  left_join(groupings, by = "Specialty") |> 
  mutate(length_staggered = case_when(
    NHS_Board_of_Treatment %in% initial_boards ~ length_all_new_rules,
    TRUE ~ length_all_old_rules))

top_10_nop <- waits |> 
  filter(Patient_Type =="New Outpatient") |> 
  count(grouped_specialty) |> 
  arrange(desc(n)) |> 
  head(10) |> 
  select(grouped_specialty) |> 
  pull()

scot_medians_nop <- waits |> 
  filter(Patient_Type =="New Outpatient") |> 
  summarise(median_old = median(length_all_old_rules),
            median_stag = median(length_staggered),
            median_new = median(length_all_new_rules),
            `90th old` = quantile(length_all_old_rules, 0.9),
            `90th stag` = quantile(length_staggered, 0.9),
            `90th new` = quantile(length_all_new_rules, 0.9),
            over_52_old = sum(length_all_old_rules>364),
            over_52_stag = sum(length_staggered>364),
            over_52_new = sum(length_all_new_rules>364),
            med_d_p_stag = 100*(median_stag-median_old)/median_old,
            med_d_p_full = 100*(median_new-median_old)/median_old,
            `90th_d_p_stag` = 100*(`90th stag`-`90th old`)/`90th old`,
            `90th_d_p_full` = 100*(`90th new`-`90th old`)/`90th old`,
            over_52_d_p_stag = 100*(over_52_stag-over_52_old)/over_52_old,
            over_52_d_p_full = 100*(over_52_new-over_52_old)/over_52_old) |> 
  mutate(grouped_specialty = "All Specialties", .before = 1)

spec_medians_nop <- waits |> 
  filter(grouped_specialty %in% top_10_nop) |> 
  filter(Patient_Type =="New Outpatient") |> 
  group_by(grouped_specialty) |> 
  summarise(median_old = median(length_all_old_rules),
            median_stag = median(length_staggered),
            median_new = median(length_all_new_rules),
            `90th old` = quantile(length_all_old_rules, 0.9),
            `90th stag` = quantile(length_staggered, 0.9),
            `90th new` = quantile(length_all_new_rules, 0.9),
            over_52_old = sum(length_all_old_rules>364),
            over_52_stag = sum(length_staggered>364),
            over_52_new = sum(length_all_new_rules>364),
            med_d_p_stag = 100*(median_stag-median_old)/median_old,
            med_d_p_full = 100*(median_new-median_old)/median_old,
            `90th_d_p_stag` = 100*(`90th stag`-`90th old`)/`90th old`,
            `90th_d_p_full` = 100*(`90th new`-`90th old`)/`90th old`,
            over_52_d_p_stag = 100*(over_52_stag-over_52_old)/over_52_old,
            over_52_d_p_full = 100*(over_52_new-over_52_old)/over_52_old) |> 
  ungroup()

spec_meds_nop <- bind_rows(scot_medians_nop, spec_medians_nop)

write_csv(spec_meds_nop, paste0("output/", run_name, "/",
                                "spec_medians_nop.csv"))


top_10_ipdc <- waits |> 
  filter(Patient_Type =="Inpatient/Day case") |> 
  count(grouped_specialty) |> 
  arrange(desc(n)) |> 
  head(10) |> 
  select(grouped_specialty) |> 
  pull()

scot_medians_ipdc <- waits |> 
  filter(Patient_Type =="Inpatient/Day case") |> 
  summarise(median_old = median(length_all_old_rules),
            median_stag = median(length_staggered),
            median_new = median(length_all_new_rules),
            `90th old` = quantile(length_all_old_rules, 0.9),
            `90th stag` = quantile(length_staggered, 0.9),
            `90th new` = quantile(length_all_new_rules, 0.9),
            over_52_old = sum(length_all_old_rules>364),
            over_52_stag = sum(length_staggered>364),
            over_52_new = sum(length_all_new_rules>364),
            med_d_p_stag = 100*(median_stag-median_old)/median_old,
            med_d_p_full = 100*(median_new-median_old)/median_old,
            `90th_d_p_stag` = 100*(`90th stag`-`90th old`)/`90th old`,
            `90th_d_p_full` = 100*(`90th new`-`90th old`)/`90th old`,
            over_52_d_p_stag = 100*(over_52_stag-over_52_old)/over_52_old,
            over_52_d_p_full = 100*(over_52_new-over_52_old)/over_52_old) |> 
  mutate(grouped_specialty = "All Specialties", .before = 1)

spec_medians_ipdc <- waits |> 
  filter(grouped_specialty %in% top_10_ipdc) |> 
  filter(Patient_Type =="Inpatient/Day case") |> 
  group_by(grouped_specialty) |> 
  summarise(median_old = median(length_all_old_rules),
            median_stag = median(length_staggered),
            median_new = median(length_all_new_rules),
            `90th old` = quantile(length_all_old_rules, 0.9),
            `90th stag` = quantile(length_staggered, 0.9),
            `90th new` = quantile(length_all_new_rules, 0.9),
            over_52_old = sum(length_all_old_rules>364),
            over_52_stag = sum(length_staggered>364),
            over_52_new = sum(length_all_new_rules>364),
            med_d_p_stag = 100*(median_stag-median_old)/median_old,
            med_d_p_full = 100*(median_new-median_old)/median_old,
            `90th_d_p_stag` = 100*(`90th stag`-`90th old`)/`90th old`,
            `90th_d_p_full` = 100*(`90th new`-`90th old`)/`90th old`,
            over_52_d_p_stag = 100*(over_52_stag-over_52_old)/over_52_old,
            over_52_d_p_full = 100*(over_52_new-over_52_old)/over_52_old) |> 
  ungroup()

spec_meds_ipdc <- bind_rows(scot_medians_ipdc, spec_medians_ipdc)

write_csv(spec_meds_ipdc, paste0("output/", run_name, "/",
                                "spec_medians_ipdc.csv"))
