# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# analysis.R
# Angus Morton
# 2024-11-11
#
# Update by Ross Burns (20/08/2025)
# 
# Produce the analytical outputs for the new wait calculation with detailed specialty outputs for the CoW paper.
# Includes all specialties (instead of top 10) with waits under 12 weeks and overall list size.
# Modified from analysis.R
# 
# R version 4.4.2 (2024-10-31)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(readr)
library(networkD3)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(openxlsx)
library(phsstyles)

# Optionally overwrite run_name
#run_name <- "qe_jun_25"

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

waits <- waits |> 
  left_join(groupings, by = "Specialty")


perform_analysis <- function(ptype, w_length) {
  
  if (ptype == "NOP") {
    data <- waits |> 
      filter(Patient_Type == "New Outpatient")
  } else if (ptype == "IPDC") {
    data <- waits |> 
      filter(Patient_Type == "Inpatient/Day case")
  } else {
    data <- waits
  }
  
  #### Step 1 : top line figures ----
  
  # number of waits which have changed length
  changed_waits <- data |> 
    filter(length_all_old_rules != {{ w_length }}) |> 
    nrow()
  
  # Percentage of waits which have changed
  changed_waits_p <- 100*changed_waits/(nrow(data))
  
  # Mean change for an adjusted wait that has changed
  mean_difference <- data |> 
    filter(length_all_old_rules != {{ w_length }}) |> 
    summarise(mean_diff = mean({{ w_length }}-length_all_old_rules)) |> 
    pull()
  
  
  top_line <- tibble(
    measure = c("Number of waits which have changed length",
                "Percentage of waits which have changed length",
                "Mean change for an adjusted wait that has changed"),
    value = c(changed_waits,
              changed_waits_p,
              mean_difference)
  )
  
  #### Step 2 : Tables ----
  
  scotland_medians <- data |> 
    summarise(list_size = n(),
              median_new = median({{ w_length }}),
              median_old = median(length_all_old_rules),
              `90th new` = quantile({{ w_length }}, 0.9),
              `90th old` = quantile(length_all_old_rules, 0.9),
              under_12_new = sum({{ w_length }}<84),
              under_12_old = sum(length_all_old_rules<84),
              over_52_new = sum({{ w_length }}>364),
              over_52_old = sum(length_all_old_rules>364),
              med_diff_p = 100*(median_new-median_old)/median_old,
              `90th_diff_p` = 100*(`90th new`-`90th old`)/`90th old`,
              over_52_diff_p = 100*(over_52_new-over_52_old)/over_52_old,
              under_12_diff_p = 100*(under_12_new-under_12_old)/under_12_old)
  
  board_medians <- data |> 
    group_by(NHS_Board_of_Treatment) |> 
    summarise(list_size = n(),
              median_new = median({{ w_length }}),
              median_old = median(length_all_old_rules),
              `90th new` = quantile({{ w_length }}, 0.9),
              `90th old` = quantile(length_all_old_rules, 0.9),
              under_12_new = sum({{ w_length }}<84),
              under_12_old = sum(length_all_old_rules<84),
              over_52_new = sum({{ w_length }}>364),
              over_52_old = sum(length_all_old_rules>364),
              med_diff_p = 100*(median_new-median_old)/median_old,
              `90th_diff_p` = 100*(`90th new`-`90th old`)/`90th old`,
              over_52_diff_p = 100*(over_52_new-over_52_old)/over_52_old,
              under_12_diff_p = 100*(under_12_new-under_12_old)/under_12_old) |> 
    ungroup()
  
  spec_medians <- data |> 
    group_by(grouped_specialty) |> 
    summarise(list_size = n(),
              median_new = median({{ w_length }}),
              median_old = median(length_all_old_rules),
              `90th new` = quantile({{ w_length }}, 0.9),
              `90th old` = quantile(length_all_old_rules, 0.9),
              under_12_new = sum({{ w_length }}<84),
              under_12_old = sum(length_all_old_rules<84),
              over_52_new = sum({{ w_length }}>364),
              over_52_old = sum(length_all_old_rules>364),
              med_diff_p = 100*(median_new-median_old)/median_old,
              `90th_diff_p` = 100*(`90th new`-`90th old`)/`90th old`,
              over_52_diff_p = 100*(over_52_new-over_52_old)/over_52_old,
              under_12_diff_p = 100*(under_12_new-under_12_old)/under_12_old) |> 
    ungroup()
  
  board_spec_medians <- data |> 
    group_by(NHS_Board_of_Treatment, grouped_specialty) |> 
    summarise(list_size = n(),
              median_new = median({{ w_length }}),
              median_old = median(length_all_old_rules),
              `90th new` = quantile({{ w_length }}, 0.9),
              `90th old` = quantile(length_all_old_rules, 0.9),
              under_12_new = sum({{ w_length }}<84),
              under_12_old = sum(length_all_old_rules<84),
              over_52_new = sum({{ w_length }}>364),
              over_52_old = sum(length_all_old_rules>364),
              med_diff_p = 100*(median_new-median_old)/median_old,
              `90th_diff_p` = 100*(`90th new`-`90th old`)/`90th old`,
              over_52_diff_p = 100*(over_52_new-over_52_old)/over_52_old,
              under_12_diff_p = 100*(under_12_new-under_12_old)/under_12_old) |> 
    ungroup()
  
  
  # create bands based on planned care targets to look at records which
  # have changed
  
  wait_band_changes <- data |> 
    mutate(
      bin_new = case_when(
        {{ w_length }}/7 < 12 ~ "0-12",
        between({{ w_length }}/7, 12, 52) ~ "12-52",
        between({{ w_length }}/7, 52, 78) ~ "52-78",
        between({{ w_length }}/7, 78, 104) ~ "78-104",
        {{ w_length }}/7 >= 104 ~ "104+"
      ),
      bin_old = case_when(
        length_all_old_rules/7 < 12 ~ "0-12",
        between(length_all_old_rules/7, 12, 52) ~ "12-52",
        between(length_all_old_rules/7, 52, 78) ~ "52-78",
        between(length_all_old_rules/7, 78, 104) ~ "78-104",
        length_all_old_rules/7 >= 104 ~ "104+"
      )) |> 
    mutate(bin_new = factor(bin_new, levels = c("0-12","12-52","52-78",
                                                "78-104","104+")),
           bin_old = factor(bin_old, levels = c("0-12","12-52","52-78",
                                                "78-104","104+"))) |> 
    filter(bin_new != bin_old) |> 
    count(bin_old, bin_new)
  
  
  #### Step 4 : Exports ----
  
  write_csv(top_line, paste0("output/", run_name, "/spec_detailed/",
                             ptype, "_",
                             as.character(w_length),
                             "_top_line_figures.csv"))
  
  write_csv(wait_band_changes, paste0("output/", run_name, "/spec_detailed/",
                                      ptype, "_",
                                      as.character(w_length),
                                      "_band_changes.csv"))
  
  write_csv(scotland_medians, paste0("output/", run_name, "/spec_detailed/",
                                     ptype, "_",
                                     as.character(w_length),
                                     "_scotland_medians.csv"))
  
  write_csv(board_medians, paste0("output/", run_name, "/spec_detailed/",
                                  ptype, "_",
                                  as.character(w_length),
                                  "_board_medians.csv"))
  
  write_csv(spec_medians, paste0("output/", run_name, "/spec_detailed/",
                                 ptype, "_",
                                 as.character(w_length),
                                 "_spec_medians.csv"))
  
  write_csv(board_spec_medians, paste0("output/", run_name, "/spec_detailed/",
                                       ptype, "_",
                                       as.character(w_length),
                                       "_board_spec_medians.csv"))
  
  
}


rules <- c(expr(length_reasonable_offer),
           expr(length_unavail_beyond_12),
           expr(length_resets_beyond_12),
           expr(length_no_urgency),
           expr(length_all_new_rules))

map(rules, perform_analysis, ptype = "IPDC")
map(rules, perform_analysis, ptype = "NOP")
map(rules, perform_analysis, ptype = "All")

