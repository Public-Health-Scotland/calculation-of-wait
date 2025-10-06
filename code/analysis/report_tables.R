# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# report_tables.R
# Angus Morton
# 2025-10-03
# 
# report tables
# 
# R version 4.4.2 (2024-10-31)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Step 0 : Housekeeping ----

#### Import waits

waits <- read_rds("output/comp_6_months/waits.rds")

waits_post_nop <- waits |> 
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
  )

waits_post_ipdc <- waits |> 
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
  )

write_csv(waits_post_nop, "output/report/table2.csv")
write_csv(waits_post_ipdc, "output/report/table5.csv")

#

waits <- read_rds("output/aug_25/waits.rds")

waits_post_nop <- waits |> 
  filter(Patient_Type == "New Outpatient") |> 
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

waits_post_ipdc <- waits |> 
  filter(Patient_Type == "Inpatient/Day case") |> 
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

write_csv(waits_post_nop, "output/report/table3_aug.csv")
write_csv(waits_post_ipdc, "output/report/table6_aug.csv")

#

waits <- read_rds("output/jul_25/waits.rds")

waits_post_nop <- waits |> 
  filter(Patient_Type == "New Outpatient") |> 
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

waits_post_ipdc <- waits |> 
  filter(Patient_Type == "Inpatient/Day case") |> 
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

write_csv(waits_post_nop, "output/report/table3_jul.csv")
write_csv(waits_post_ipdc, "output/report/table6_jul.csv")
