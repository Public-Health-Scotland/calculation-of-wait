# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# report_tables.R
# Angus Morton
# 2025-10-03
# 
# report tables
# 
# R version 4.4.2 (2024-10-31)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(lubridate)
library(dplyr)

#### Step 0 : Housekeeping ----

get_summary <- function(df, ptype) {
  
  summary <- df |> 
    filter(Patient_Type == ptype) |> 
    mutate(month = month(Date),
           in12_23 = Number_of_waiting_list_days <= 84,
           over52_23 = Number_of_waiting_list_days > 364,
           in12_12 = length_all_old_rules <= 84,
           over52_12 = length_all_old_rules > 364) |> 
    group_by(month) |> 
    summarise(
      total = n(),
      in12_23 = sum(in12_23),
      over52_23 = sum(over52_23),
      in12_12 = sum(in12_12),
      over52_12 = sum(over52_12)
    ) |> 
    ungroup() |> 
    mutate(
      change_in12 = in12_23-in12_12,
      change_over52 = over52_23-over52_12
    )
  
  summary
  
}

get_med_90 <- function(df) {
  
  med_90 <- df |> 
    mutate(month = month(Date)) |> 
    group_by(Patient_Type, month) |> 
    summarise(median_new = median(Number_of_waiting_list_days),
              `90th new` = quantile(Number_of_waiting_list_days, 0.9),
              median_old = median(length_all_old_rules),
              `90th old` = quantile(length_all_old_rules, 0.9),
              med_diff = median_new-median_old,
              `90th_diff` = `90th new`-`90th old`) |> 
    ungroup() |> 
    arrange(desc(month))
  
  med_90
  
}

#### Import waits

waits <- read_rds("output/comp_apr_sep/waits.rds")

waits_nop <- waits |> 
  filter(Patient_Type == "New Outpatient") |> 
  mutate(month = month(List_removal_date),
         in12_23 = Number_of_waiting_list_days <= 84,
         over52_23 = Number_of_waiting_list_days > 364,
         in12_12 = length_all_old_rules <= 84,
         over52_12 = length_all_old_rules > 364) |> 
  group_by(month) |> 
  summarise(
    total = n(),
    in12_23 = sum(in12_23),
    over52_23 = sum(over52_23),
    in12_12 = sum(in12_12),
    over52_12 = sum(over52_12)
  ) |> 
  ungroup() |> 
  mutate(
    change_in12 = in12_23-in12_12,
    change_over52 = over52_23-over52_12
  ) |> 
  arrange(desc(month))

waits_ipdc <- waits |> 
  filter(Patient_Type == "Inpatient/Day case") |> 
  mutate(month = month(List_removal_date),
         in12_23 = Number_of_waiting_list_days <= 84,
         over52_23 = Number_of_waiting_list_days > 364,
         in12_12 = length_all_old_rules <= 84,
         over52_12 = length_all_old_rules > 364) |> 
  group_by(month) |> 
  summarise(
    total = n(),
    in12_23 = sum(in12_23),
    over52_23 = sum(over52_23),
    in12_12 = sum(in12_12),
    over52_12 = sum(over52_12)
  ) |> 
  ungroup() |> 
  mutate(
    change_in12 = in12_23-in12_12,
    change_over52 = over52_23-over52_12
  ) |> 
  arrange(desc(month))

write_csv(waits_nop, "output/report/table2a.csv")
write_csv(waits_ipdc, "output/report/table7a.csv")

#

waits <- read_rds("output/ongo_sep25/waits.rds")
sum_sep_nop <- get_summary(waits, "New Outpatient")
sum_sep_ipdc <- get_summary(waits, "Inpatient/Day case")

waits <- read_rds("output/ongo_aug25/waits.rds")
sum_aug_nop <- get_summary(waits, "New Outpatient")
sum_aug_ipdc <- get_summary(waits, "Inpatient/Day case")

waits <- read_rds("output/ongo_jul25/waits.rds")
sum_jul_nop <- get_summary(waits, "New Outpatient")
sum_jul_ipdc <- get_summary(waits, "Inpatient/Day case")

waits <- read_rds("output/ongo_jun25/waits.rds")
sum_jun_nop <- get_summary(waits, "New Outpatient")
sum_jun_ipdc <- get_summary(waits, "Inpatient/Day case")

waits <- read_rds("output/ongo_may25/waits.rds")
sum_may_nop <- get_summary(waits, "New Outpatient")
sum_may_ipdc <- get_summary(waits, "Inpatient/Day case")

waits <- read_rds("output/ongo_apr25/waits.rds")
sum_apr_nop <- get_summary(waits, "New Outpatient")
sum_apr_ipdc <- get_summary(waits, "Inpatient/Day case")

sum_nop <- bind_rows(sum_sep_nop, sum_aug_nop, sum_jul_nop,
                     sum_jun_nop, sum_may_nop, sum_apr_nop) |> 
  arrange(desc(month))

sum_ipdc <- bind_rows(sum_sep_ipdc, sum_aug_ipdc, sum_jul_ipdc,
                      sum_jun_ipdc, sum_may_ipdc, sum_apr_ipdc) |> 
  arrange(desc(month))

write_csv(sum_nop, "output/report/table2b.csv")
write_csv(sum_ipdc, "output/report/table7b.csv")

#### Table 4 and 8 ----

# completed
waits <- read_rds("output/comp_apr_sep/waits.rds")

med_90_comp <- waits |> 
  mutate(month = month(List_removal_date)) |> 
  group_by(Patient_Type, month) |> 
  summarise(median_new = median(Number_of_waiting_list_days),
            `90th new` = quantile(Number_of_waiting_list_days, 0.9),
            median_old = median(length_all_old_rules),
            `90th old` = quantile(length_all_old_rules, 0.9),
            med_diff = median_new-median_old,
            `90th_diff` = `90th new`-`90th old`) |> 
  ungroup() |> 
  arrange(desc(month))

# ongoing
waits <- read_rds("output/ongo_sep25/waits.rds")
med_90_sep <- get_med_90(waits)

waits <- read_rds("output/ongo_aug25/waits.rds")
med_90_aug <- get_med_90(waits)

waits <- read_rds("output/ongo_jul25/waits.rds")
med_90_jul <- get_med_90(waits)

waits <- read_rds("output/ongo_jun25/waits.rds")
med_90_jun <- get_med_90(waits)

waits <- read_rds("output/ongo_may25/waits.rds")
med_90_may <- get_med_90(waits)

waits <- read_rds("output/ongo_apr25/waits.rds")
med_90_apr <- get_med_90(waits)

med_90_ongo <- bind_rows(med_90_sep, med_90_aug, med_90_jul,
                         med_90_jun, med_90_may, med_90_apr) |> 
  arrange(desc(month))

med_90 <- bind_rows(med_90_comp, med_90_ongo)

med_90_nop <- med_90 |> filter(Patient_Type == "New Outpatient")
med_90_ipdc <- med_90 |> filter(Patient_Type == "Inpatient/Day case")

write_csv(med_90_nop, "output/report/table4.csv")
write_csv(med_90_ipdc, "output/report/table8.csv")
