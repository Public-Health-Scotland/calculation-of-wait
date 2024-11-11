# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# run_scripts
# Angus Morton
# 2024-11-11
# 
# Run each of the scripts for the new calculation of wait
# 
# R version 4.1.2 (2021-11-01)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Step 0 : Housekeeping ----

source("code/settings.R")

# import data
source("code/import_data.R")

# calculate waits
if (rule == "all") {
  source("code/calculate_waits_all_new_rules.R")
}

if (rule == "resets_beyond_12") {
  source("code/calculate_waits_resets_beyond_12.R")
}

if (rule == "unavailability_beyond_12") {
  source("code/calculate_waits_unavail_beyond_12.R")
}

if (rule == "short_notice_change") {
  source("code/calculate_waites_short_notice_change.R")
}


# analysis
source("code/analysis.R")