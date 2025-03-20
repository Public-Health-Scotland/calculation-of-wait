# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# export_muis.R
# Angus Morton
# 2025-03-20
# 
# Export MUIs for each rule change
# 
# R version 4.1.2 (2021-11-01)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Step 0 : Housekeeping ----

library(readr)
library(dplyr)
library(lubridate)
library(tidylog)

#### Read in data ----

waits <- read_rds("output/uat_muis/waits.rds")

#### Separate MUIs ----

reasonable <- waits |> 
  filter(length_all_old_rules != length_reasonable_offer) |> 
  select(CHI, MUI)

unavailability <- waits |> 
  filter(length_all_old_rules != length_unavail_beyond_12) |> 
  select(CHI, MUI)

resets <- waits |> 
  filter(length_all_old_rules != length_resets_beyond_12) |> 
  select(CHI, MUI)

urgency <- waits |> 
  filter(length_all_old_rules != length_no_urgency) |> 
  select(CHI, MUI)

all_rules <- waits |> 
  filter(length_all_old_rules != length_all_new_rules) |> 
  select(CHI, MUI)

#### Insert flags ----

waits <- waits |> 
  mutate(flag_reasonable_offer = if_else(length_all_old_rules != length_reasonable_offer,
                                         TRUE, FALSE),
         flag_unavail_beyond_12 = if_else(length_all_old_rules != length_unavail_beyond_12,
                                          TRUE, FALSE),
         flag_resets_beyond_12 = if_else(length_all_old_rules != length_resets_beyond_12,
                                         TRUE, FALSE),
         flag_no_urgency = if_else(length_all_old_rules != length_no_urgency,
                                   TRUE, FALSE),
         flag_all_new_rules = if_else(length_all_old_rules != length_all_new_rules,
                                      TRUE, FALSE))



#### Save out ----

write_csv(reasonable, paste0("output/uat_muis/", "reasonable.csv"))
write_csv(unavailability, paste0("output/uat_muis/", "unavailability.csv"))
write_csv(resets, paste0("output/uat_muis/", "resets.csv"))
write_csv(urgency, paste0("output/uat_muis/", "urgency.csv"))
write_csv(all_rules, paste0("output/uat_muis/", "all_rules.csv"))

write_csv(waits, paste0("output/uat_muis/", "all_data.csv"))
