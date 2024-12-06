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
  
} else if (rule == "resets_beyond_12") {
  
  source("code/calculate_waits_resets_beyond_12.R")
  
} else if (rule == "unavailability_beyond_12") {
  
  source("code/calculate_waits_unavail_beyond_12.R")
  
} else if (rule == "short_notice_change") {
  
  source("code/calculate_waits_short_notice_change.R")
  
} else if (rule == "all_old") {
  
  source("code/calculate_waits_all_old_rules.R")
  
} else if (rule == "no_urgency") {
  
  source("code/calculate_waits_no_urgency.R")
  
}


# analysis (add line)
source("code/analysis.R")



