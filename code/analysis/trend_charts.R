# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# trend_chart.R
# Angus Morton
# 2025-08-25
# 
# Produce trend charts with lines for the old and new guidance
# 
# R version 4.4.2 (2024-10-31)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Step 0 : Housekeeping ----

library(readr)
library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)

#### Step x : Imports ----

waits <- read_rds("output/waits_old_new_wh.rds")

pub_nop <- read_csv("/PHI_conf/WaitingTimes/SoT/Publications/Inpatient, Day case and Outpatient Stage of Treatment Waiting Times/20250826/Output/R Output/PerformanceOP.csv")
pub_ipdc <- read_csv("/PHI_conf/WaitingTimes/SoT/Publications/Inpatient, Day case and Outpatient Stage of Treatment Waiting Times/20250826/Output/R Output/PerformanceIPDC.csv")

pub <- bind_rows(pub_nop, pub_ipdc)

long_nop <- read_csv("/PHI_conf/WaitingTimes/SoT/Publications/Inpatient, Day case and Outpatient Stage of Treatment Waiting Times/20250826/Output/R Output/DistributionofWaitsOPLong.csv")
long_ipdc <- read_csv("/PHI_conf/WaitingTimes/SoT/Publications/Inpatient, Day case and Outpatient Stage of Treatment Waiting Times/20250826/Output/R Output/DistributionofWaitsIPDCLong.csv")

long <- bind_rows(long_nop, long_ipdc)

#### Step x : Wrangling ----
waits <- waits |> 
  mutate(old_length = if_else(Wait_Calculation_Used == "2023" | is.na(Wait_Calculation_Used),
                              length_all_old_rules,
                              length_wh),
         new_length = if_else(Wait_Calculation_Used == "2023" | is.na(Wait_Calculation_Used),
                              length_wh,
                              length_all_new_rules))


scotland_medians <- waits |> 
  group_by(Patient_Type) |> 
  summarise(median_new = median(new_length),
            median_old = median(old_length),
            `90th new` = quantile(new_length, 0.9),
            `90th old` = quantile(old_length, 0.9),
            over_52_new = sum(new_length>364),
            over_52_old = sum(old_length>364)) |> 
  ungroup() |> 
  mutate(Date = "31/07/2025")


pub <- pub |> 
  mutate(Median = as.numeric(Median),
         `90th Percentile` = as.numeric(`90th Percentile`)) |> 
  filter(`Ongoing/Completed` == "Ongoing",
         # Date %in% c("30/06/2025",
         #             "31/05/2025",
         #             "30/04/2025",
         #             "31/03/2025"),
         Specialty == "All Specialties",
         `NHS Board of Treatment` == "NHS Scotland") |> 
  select(Patient_Type = `Patient Type`,
         Date,
         median_old = Median,
         `90th old` = `90th Percentile`)

long <- long |> 
  filter(`Ongoing/Completed` == "Ongoing",
         # Date %in% c("31/07/2025",
         #             "30/06/2025",
         #             "31/05/2025",
         #             "30/04/2025",
         #             "31/03/2025"),
         Specialty == "All Specialties",
         `NHS Board of Treatment` == "NHS Scotland") |> 
  select(Patient_Type = `Patient Type`,
         Date,
         over_52_old = `Over 52`)

pub_long <- left_join(pub, long, by = c("Patient_Type", "Date"))

old_line <- scotland_medians |> 
  select(Patient_Type, Date, median_old, `90th old`, over_52_old) |> 
  bind_rows(pub_long) |> 
  mutate(Date = dmy(Date)) |>
  arrange(desc(Date)) |> 
  filter(Patient_Type == "Inpatient/Day case",
         Date > dmy("01/01/2024"))

new_line <- pub_long |> 
  filter(Date %in% c("30/06/2025",
                     "31/07/2025")) |> 
  rename(median_new = median_old,
         `90th new` = `90th old`,
         over_52_new = over_52_old) |> 
  bind_rows(scotland_medians) |> 
  select(Patient_Type, Date, median_new, `90th new`, over_52_new) |> 
  mutate(Date = dmy(Date)) |>
  arrange(desc(Date)) |> 
  filter(Patient_Type == "Inpatient/Day case",
         Date > dmy("01/01/2024"))


ggplot() +
  geom_line(data = old_line, aes(x = Date, y = median_old), colour = "green") +
  geom_line(data = new_line, aes(x = Date, y = median_new), colour = "blue")
  
  