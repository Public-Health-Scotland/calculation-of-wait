# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# dow_charts
# Angus Morton
# 2025-10-14
# 
# description
# 
# R version 4.4.2 (2024-10-31)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Step 0 : Housekeeping ----

library(ggplot2)
library(openxlsx)
library(phsstyles)

#### Imports

ongoing <- read_rds("output/ongo_sep25/waits.rds")
  
completed <- read_rds("output/comp_apr_sep/waits.rds") |> 
  filter(month(List_removal_date) == 9)

#### Graphs ----

dow_nop_ongo <- ongoing |> 
  filter(Patient_Type == "New Outpatient") |> 
  mutate(Number_of_waiting_list_days := Number_of_waiting_list_days/7,
         length_all_old_rules = length_all_old_rules/7) |> 
  pivot_longer(cols = c(Number_of_waiting_list_days, length_all_old_rules)) |>
  ggplot(aes(x=value, fill=name)) + 
  geom_histogram(breaks = seq(0, 200, by = 13),
                 position = position_dodge())+
  scale_x_continuous(breaks = seq(0, 200, by=13))+
  scale_fill_manual(values = phs_colours(c("phs-magenta", "phs-purple")))+
  xlab("weeks waited") + 
  ylab("number waiting") +
  theme_phs()



dow_nop_comp <- completed |> 
  filter(Patient_Type == "New Outpatient") |> 
  mutate(Number_of_waiting_list_days := Number_of_waiting_list_days/7,
         length_all_old_rules = length_all_old_rules/7) |> 
  pivot_longer(cols = c(Number_of_waiting_list_days, length_all_old_rules)) |>
  ggplot(aes(x=value, fill=name)) + 
  geom_histogram(breaks = seq(0, 200, by = 13),
                 position = position_dodge())+
  scale_x_continuous(breaks = seq(0, 200, by=13))+
  scale_fill_manual(values = phs_colours(c("phs-magenta", "phs-purple")))+
  xlab("weeks waited") + 
  ylab("number waiting") +
  theme_phs()

dow_ipdc_ongo <- ongoing |> 
  filter(Patient_Type == "Inpatient/Day case") |> 
  mutate(Number_of_waiting_list_days := Number_of_waiting_list_days/7,
         length_all_old_rules = length_all_old_rules/7) |> 
  pivot_longer(cols = c(Number_of_waiting_list_days, length_all_old_rules)) |>
  ggplot(aes(x=value, fill=name)) + 
  geom_histogram(breaks = seq(0, 200, by = 13),
                 position = position_dodge())+
  scale_x_continuous(breaks = seq(0, 200, by=13))+
  scale_fill_manual(values = phs_colours(c("phs-magenta", "phs-purple")))+
  xlab("weeks waited") + 
  ylab("number waiting") +
  theme_phs()

dow_ipdc_comp <- completed |> 
  filter(Patient_Type == "Inpatient/Day case") |> 
  mutate(Number_of_waiting_list_days := Number_of_waiting_list_days/7,
         length_all_old_rules = length_all_old_rules/7) |> 
  pivot_longer(cols = c(Number_of_waiting_list_days, length_all_old_rules)) |>
  ggplot(aes(x=value, fill=name)) + 
  geom_histogram(breaks = seq(0, 200, by = 13),
                 position = position_dodge())+
  scale_x_continuous(breaks = seq(0, 200, by=13))+
  scale_fill_manual(values = phs_colours(c("phs-magenta", "phs-purple")))+
  xlab("weeks waited") + 
  ylab("number waiting") +
  theme_phs()

ggsave("output/report/dow_nop_ongo.png",
  scale = 3,
  plot = dow_nop_ongo)

ggsave("output/report/dow_nop_comp.png",
       scale = 3,
       plot = dow_nop_comp)

ggsave("output/report/dow_ipdc_ongo.png",
       scale = 3,
       plot = dow_ipdc_ongo)

ggsave("output/report/dow_ipdc_comp.png",
       scale = 3,
       plot = dow_ipdc_comp)

