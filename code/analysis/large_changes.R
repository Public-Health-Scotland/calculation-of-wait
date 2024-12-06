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

breakdown <- waits |> 
  filter(old_wait_length > (104*7),
         new_wait_length < (12*7)) |> 
  summarise(
    waits_under_12 = sum(new_wait_length < (12*7)),
    waits_under_4 = sum(new_wait_length < (4*7))
  )

write_csv(breakdown, "output/all_new_rules_long_to_short.csv")
  
