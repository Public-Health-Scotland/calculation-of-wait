# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# analysis.R
# Angus Morton
# 2024-11-11
# 
# Produce the analytical outputs for the new wait calculation
# 
# R version 4.1.2 (2021-11-01)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(readr)
library(networkD3)
library(dplyr)
library(tidyr)
library(ggplot2)
library(openxlsx)
library(phsstyles)

non_matching_chis <- read_rds(paste0("temp/", run_name,
                                     "/non_matching_chis.rds")) |> 
  mutate(MUI = as.character(MUI))

waits <- waits |> 
  read_rds(paste0("output/", run_name,
                  "/waits.rds")) |> 
  anti_join(non_matching_chis)

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
    summarise(median_new = median({{ w_length }}),
              median_old = median(length_all_old_rules),
              `90th new` = quantile({{ w_length }}, 0.9),
              `90th old` = quantile(length_all_old_rules, 0.9),
              over_52_new = sum({{ w_length }}>364),
              over_52_old = sum(length_all_old_rules>364),
              med_diff_p = 100*(median_new-median_old)/median_old,
              `90th_diff_p` = 100*(`90th new`-`90th old`)/`90th old`,
              over_52_diff_p = 100*(over_52_new-over_52_old)/over_52_old)
  
  board_medians <- data |> 
    group_by(NHS_Board_of_Treatment) |> 
    summarise(median_new = median({{ w_length }}),
              median_old = median(length_all_old_rules),
              `90th new` = quantile({{ w_length }}, 0.9),
              `90th old` = quantile(length_all_old_rules, 0.9),
              over_52_new = sum({{ w_length }}>364),
              over_52_old = sum(length_all_old_rules>364),
              med_diff_p = 100*(median_new-median_old)/median_old,
              `90th_diff_p` = 100*(`90th new`-`90th old`)/`90th old`,
              over_52_diff_p = 100*(over_52_new-over_52_old)/over_52_old) |> 
    ungroup()
  
  top_10 <- data |> 
    count(grouped_specialty) |> 
    arrange(desc(n)) |> 
    head(10) |> 
    select(grouped_specialty) |> 
    pull()
  
  
  spec_medians <- data |> 
    filter(grouped_specialty %in% top_10) |> 
    group_by(grouped_specialty) |> 
    summarise(median_new = median({{ w_length }}),
              median_old = median(length_all_old_rules),
              `90th new` = quantile({{ w_length }}, 0.9),
              `90th old` = quantile(length_all_old_rules, 0.9),
              over_52_new = sum({{ w_length }}>364),
              over_52_old = sum(length_all_old_rules>364),
              med_diff_p = 100*(median_new-median_old)/median_old,
              `90th_diff_p` = 100*(`90th new`-`90th old`)/`90th old`,
              over_52_diff_p = 100*(over_52_new-over_52_old)/over_52_old) |> 
    ungroup()
  
  
  # create bands based on planned care targets to look at records which
  # have changed
  
  wait_band_changes <- data |> 
    mutate(
      bin_new = case_when(
        {{ w_length }}/7 < 52 ~ "0-52",
        between({{ w_length }}/7, 52, 78) ~ "52-78",
        between({{ w_length }}/7, 78, 104) ~ "78-104",
        {{ w_length }}/7 >= 104 ~ "104+"
      ),
      bin_old = case_when(
        length_all_old_rules/7 < 52 ~ "0-52",
        between(length_all_old_rules/7, 52, 78) ~ "52-78",
        between(length_all_old_rules/7, 78, 104) ~ "78-104",
        length_all_old_rules/7 >= 104 ~ "104+"
      )) |> 
    mutate(bin_new = factor(bin_new, levels = c("0-52","52-78",
                                                "78-104","104+")),
           bin_old = factor(bin_old, levels = c("0-52","52-78",
                                                "78-104","104+"))) |> 
    filter(bin_new != bin_old) |> 
    count(bin_old, bin_new)
  
  
  #### Step 3 : DoW graph ----
  
  dow_hist <- data |> 
    mutate({{ w_length }} := {{ w_length }}/7,
           length_all_old_rules = length_all_old_rules/7) |> 
    pivot_longer(cols = c({{ w_length }}, length_all_old_rules)) |>
    ggplot(aes(x=value, fill=name)) + 
    geom_histogram(breaks = seq(0, 200, by = 13),
                   position = position_dodge())+
    scale_x_continuous(breaks = seq(0, 200, by=13))+
    scale_fill_manual(values = phs_colours(c("phs-magenta", "phs-purple")))+
    xlab("weeks waited") + 
    ylab("number waiting") +
    theme_phs()
  
  #### Step 5 : Exports ----
  
  write_csv(top_line, paste0("output/", run_name, "/",
                             ptype, "_",
                             deparse(substitute(w_length)),
                             "_top_line_figures.csv"))
  
  write_csv(wait_band_changes, paste0("output/", run_name, "/",
                                      ptype, "_",
                                      deparse(substitute(w_length)),
                                      "_band_changes.csv"))
  
  write_csv(scotland_medians, paste0("output/", run_name, "/",
                                     ptype, "_",
                                     deparse(substitute(w_length)),
                                     "_scotland_medians.csv"))
  
  write_csv(board_medians, paste0("output/", run_name, "/",
                                  ptype, "_",
                                  deparse(substitute(w_length)),
                                  "_board_medians.csv"))
  
  write_csv(spec_medians, paste0("output/", run_name, "/",
                                 ptype, "_",
                                 deparse(substitute(w_length)),
                                 "_spec_medians.csv"))
  
  ggsave(
    paste0("output/", run_name, "/",
           ptype, "_",
           deparse(substitute(w_length)),
           "_DoW_chart.jpg"),
    scale = 3,
    plot = dow_hist)
  
}

c(length_reasonable_offer, length_unavail_beyond_12)

perform_analysis("IPDC", length_reasonable_offer)
perform_analysis("IPDC", length_unavail_beyond_12)
perform_analysis("IPDC", length_resets_beyond_12)
perform_analysis("IPDC", length_reasonable_offer)

perform_analysis("NOP", length_reasonable_offer)
perform_analysis("NOP", length_unavail_beyond_12)
perform_analysis("NOP", length_resets_beyond_12)
perform_analysis("NOP", length_reasonable_offer)

perform_analysis("All", length_reasonable_offer)
perform_analysis("All", length_unavail_beyond_12)
perform_analysis("All", length_resets_beyond_12)
perform_analysis("All", length_reasonable_offer)
