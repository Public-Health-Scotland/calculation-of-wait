# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# calculate_waits_resets_beyond_12.R
# Angus Morton
# 2024-11-11
# 
# Calculate waits factoring in clock resets beyond 12 weeks
# 
# R version 4.1.2 (2021-11-01)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Step 0 : Housekeeping ----

library(readr)
library(dplyr)
library(lubridate)
library(tidylog)

