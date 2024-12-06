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


source("code/wait_calculation/all_old_rules.R")

source("code/wait_calculation/short_notice_change.R")
source("code/wait_calculation/unavail_beyond_12.R")
source("code/wait_calculation/resets_beyond_12.R")
source("code/wait_calculation/no_urgency.R")
source("code/wait_calculation/all_new_rules.R")



# analysis (add line)
source("code/analysis.R")



