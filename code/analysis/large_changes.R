# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# long_wait_changes.R
# Angus Morton
# 2024-11-26
# 
# create a quick output for waits going from >104 weeks to short time bands
# 
# R version 4.1.2 (2021-11-01)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Step 0 : Housekeeping ----

library(readr)
library(dplyr)
library(lubridate)
library(tidylog)

run_name <- "qe_mar_25"

waits <- read_rds(paste0("output/", run_name,
                         "/waits.rds"))

breakdown <- waits |> 
  filter(length_all_old_rules > (104*7),
         length_all_new_rules < (12*7)) |> 
  summarise(
    waits_under_12 = sum(length_all_new_rules < (12*7)),
    waits_under_4 = sum(length_all_new_rules < (4*7))
  )

write_csv(breakdown, paste0("output/", run_name, "/",
                            "all_new_rules_long_to_short.csv"))
  
