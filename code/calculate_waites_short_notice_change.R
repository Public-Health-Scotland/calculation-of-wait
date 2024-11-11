# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# calculate_waits_short_notice_change.R
# Angus Morton
# 2024-11-11
# 
# Calculate waits applying the 7-10 day change for the short notice period
# 
# R version 4.1.2 (2021-11-01)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Step 0 : Housekeeping ----

library(readr)
library(dplyr)
library(lubridate)
library(tidylog)