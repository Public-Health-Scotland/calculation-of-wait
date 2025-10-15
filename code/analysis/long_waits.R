# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# long_waits.R
# Angus Morton
# 2025-10-15
# 
# Ongoing waits over 78 and 104 since April
# 
# R version 4.4.2 (2024-10-31)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Step 0 : Housekeeping ----

library(readr)
library(dplyr)


get_long_waits <- function(df) {
  
  long_waits <- df |> 
    mutate(over_78_old = if_else(length_all_old_rules > 546, 1, 0),
           over_104_old = if_else(length_all_old_rules > 728, 1, 0),
           over_78_new = if_else(Number_of_waiting_list_days > 546, 1, 0),
           over_104_new = if_else(Number_of_waiting_list_days > 728, 1, 0)) |> 
    group_by(Patient_Type, month(Date)) |> 
    summarise(
      over_78_old = sum(over_78_old),
      over_104_old = sum(over_104_old),
      over_78_new = sum(over_78_new),
      over_104_new = sum(over_104_new)
    ) |> 
    ungroup()
    
  long_waits
  
}

waits <- read_rds("output/ongo_sep25/waits.rds")

long_waits <- waits |> 
  mutate(over_78_old = if_else(length_all_old_rules > 546, 1, 0),
         over_104_old = if_else(length_all_old_rules > 728, 1, 0),
         over_78_new = if_else(Number_of_waiting_list_days > 546, 1, 0),
         over_104_new = if_else(Number_of_waiting_list_days > 728, 1, 0)) |> 
  group_by(Patient_Type, month(Date)) |> 
  summarise(
    over_78_old = sum(over_78_old),
    over_104_old = sum(over_104_old),
    over_78_new = sum(over_78_new),
    over_104_new = sum(over_104_new)
  ) |> 
  ungroup()

waits <- read_rds("output/ongo_sep25/waits.rds")
long_waits_sep <- get_long_waits(waits)

waits <- read_rds("output/ongo_aug25/waits.rds")
long_waits_aug <- get_long_waits(waits)

waits <- read_rds("output/ongo_jul25/waits.rds")
long_waits_jul <- get_long_waits(waits)

waits <- read_rds("output/ongo_jun25/waits.rds")
long_waits_jun <- get_long_waits(waits)

waits <- read_rds("output/ongo_may25/waits.rds")
long_waits_may <- get_long_waits(waits)

waits <- read_rds("output/ongo_apr25/waits.rds")
long_waits_apr <- get_long_waits(waits)
  

long_waits <- bind_rows(long_waits_sep, long_waits_aug, long_waits_jul,
                        long_waits_jun, long_waits_may, long_waits_apr) |> 
  arrange(Patient_Type, desc(`month(Date)`))

write_csv(long_waits, "output/report/long_waits.csv")
