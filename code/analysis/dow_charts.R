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

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(openxlsx)
library(phsstyles)
library(readr)
library(lubridate)
library(gridExtra)



#### Imports

ongoing <- read_rds("output/ongo_sep25/waits.rds")
  
completed <- read_rds("output/comp_apr_sep/waits.rds") |> 
  filter(month(List_removal_date) == 9)


# Define custom bins ----
# 4-week bins up to 52, then 13-week bins to 156, then a final 156+ group
bin_edges <- c(seq(0, 52, by = 4), seq(65, 156, by = 13), Inf)

# Manually create range labels for each bin
bin_labels <- c(
  paste(seq(0, 48, by = 4), seq(4, 52, by = 4), sep = "–"), # 0–4, 4–8, …, 48–52
  paste(c(52, 65, 78, 91, 104, 117, 130, 143), # 13-week bands
        c(65, 78, 91, 104, 117, 130, 143, 156),
        sep = "–"),
  "156+" # final bin
)

#### Graphs ----

# ---- New Outpatient – Completed ----

y_max <- 50000

dow_nop_comp <- completed |> 
  filter(Patient_Type == "New Outpatient") %>% 
  mutate(Number_of_waiting_list_days := Number_of_waiting_list_days/7,
         length_all_old_rules = length_all_old_rules/7) %>% 
  rename("2012 guidance" = length_all_old_rules,
         "2023 guidance" = Number_of_waiting_list_days)  %>%  
  pivot_longer(cols = c("2012 guidance", "2023 guidance"),
               names_to = "guidance", values_to = "weeks") %>% 
  mutate(bin = cut(
      weeks,
      breaks = bin_edges,
      include.lowest = TRUE,
      right = TRUE,
      labels = bin_labels
    )) |> 
  ggplot(aes(x = bin, fill = guidance)) +
  geom_bar(position = "dodge", width = 0.9) +
  scale_fill_manual(values = c("2012 guidance" = "#12436D", "2023 guidance" = "#28A197")) +
  scale_y_continuous(expand = c(0, 0), labels = label_comma(),
                     limits = c(0, y_max),
                     breaks = waiver()) +
  geom_vline(xintercept = 13.5, linetype = 2) +
  annotate("text", x=13.67, y=y_max*0.7,
           label = "Change from 4 week to \n13 week time bands",
           hjust = "left", size = 6.5/.pt) +
  labs(
    title = "Completed Waits",
    x = "Wait length in weeks",
    y = NULL,
    fill = "Guidance rules"
  ) +
  theme_phs() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8, family = "arial"),
    axis.text.y = element_text(size = 9, family = "arial"),
    axis.title.x = element_text(size = 9, family = "arial"),
    legend.text = element_text(size = 9, family = "arial"),
    legend.position = "bottom",
    axis.ticks = element_line(),
    panel.grid.major.y = element_line(colour = "black", linewidth = 0.1),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )



print(dow_nop_comp)


# New Outpatient – Ongoing ----

y_max <- 125000


dow_nop_ongo <- ongoing |> 
  filter(Patient_Type == "New Outpatient") |> 
  mutate(Number_of_waiting_list_days := Number_of_waiting_list_days/7,
         length_all_old_rules = length_all_old_rules/7) %>% 
  rename("2012 guidance" = length_all_old_rules,
         "2023 guidance" = Number_of_waiting_list_days)  %>%  
  pivot_longer(cols = c("2012 guidance", "2023 guidance"),
               names_to = "guidance", values_to = "weeks") %>% 
  mutate(bin = cut(
    weeks,
    breaks = bin_edges,
    include.lowest = TRUE,
    right = TRUE,
    labels = bin_labels
  )) |> 
  ggplot(aes(x = bin, fill = guidance)) +
  geom_bar(position = "dodge", width = 0.9) +
  scale_fill_manual(values = c("2012 guidance" = "#12436D", "2023 guidance" = "#28A197")) +
  scale_y_continuous(expand = c(0, 0), labels = label_comma(),
                     limits = c(0, y_max),
                     breaks = waiver()) +
  geom_vline(xintercept = 13.5, linetype = 2) +
  annotate("text", x=13.6, y=y_max*0.7,
           label = "Change from 4 week to \n13 week time bands",
           hjust = "left", size = 6.5/.pt) +
  labs(
    title = "Ongoing Waits",
    x = "Wait length in weeks",
    y = NULL,
    fill = "Guidance rules"
  ) +
  theme_phs() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8, family = "arial"),
    axis.text.y = element_text(size = 9, family = "arial"),
    axis.title.x = element_text(size = 9, family = "arial"),
    legend.text = element_text(size = 9, family = "arial"),
    legend.position = "bottom",
    axis.ticks = element_line(),
    panel.grid.major.y = element_line(colour = "black", linewidth = 0.1),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )


print(dow_nop_ongo)


# IPDC – Completed ----

y_max <- 8000


dow_ipdc_comp <- completed |> 
  filter(Patient_Type == "Inpatient/Day case") |> 
  mutate(Number_of_waiting_list_days := Number_of_waiting_list_days/7,
         length_all_old_rules = length_all_old_rules/7) %>% 
  rename("2012 guidance" = length_all_old_rules,
         "2023 guidance" = Number_of_waiting_list_days)  %>%  
  pivot_longer(cols = c("2012 guidance", "2023 guidance"),
               names_to = "guidance", values_to = "weeks") %>% 
  mutate(bin = cut(
    weeks,
    breaks = bin_edges,
    include.lowest = TRUE,
    right = TRUE,
    labels = bin_labels
  )) |> 
  ggplot(aes(x = bin, fill = guidance)) +
  geom_bar(position = "dodge", width = 0.9) +
  scale_fill_manual(values = c("2012 guidance" = "#12436D", "2023 guidance" = "#28A197")) +
  scale_y_continuous(expand = c(0, 0), labels = label_comma(),
                     limits = c(0, y_max),
                     breaks = waiver()) +
  geom_vline(xintercept = 13.5, linetype = 2) +
  annotate("text", x=13.75, y=y_max*0.75,
           label = "Change from 4 week to \n13 week time bands",
           hjust = "left", size = 6.5/.pt) +
  labs(
    title = "Completed Waits",
    x = "Wait length in weeks",
    y = NULL,
    fill = "Guidance rules"
  ) +
  theme_phs() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8, family = "arial"),
    axis.text.y = element_text(size = 9, family = "arial"),
    axis.title.x = element_text(size = 9, family = "arial"),
    legend.text = element_text(size = 9, family = "arial"),
    legend.position = "bottom",
    axis.ticks = element_line(),
    panel.grid.major.y = element_line(colour = "black", linewidth = 0.1),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

print(dow_ipdc_comp)

# IPDC – Ongoing ----

y_max <- 40000

dow_ipdc_ongo <- ongoing |> 
  filter(Patient_Type == "Inpatient/Day case") |> 
  mutate(Number_of_waiting_list_days := Number_of_waiting_list_days/7,
         length_all_old_rules = length_all_old_rules/7) %>% 
  rename("2012 guidance" = length_all_old_rules,
         "2023 guidance" = Number_of_waiting_list_days)  %>%  
  pivot_longer(cols = c("2012 guidance", "2023 guidance"),
               names_to = "guidance", values_to = "weeks") %>% 
  mutate(bin = cut(
    weeks,
    breaks = bin_edges,
    include.lowest = TRUE,
    right = TRUE,
    labels = bin_labels
  )) |> 
  ggplot(aes(x = bin, fill = guidance)) +
  geom_bar(position = "dodge", width = 0.9) +
  scale_fill_manual(values = c("2012 guidance" = "#12436D", "2023 guidance" = "#28A197")) +
  scale_y_continuous(expand = c(0, 0), labels = label_comma(),
                     limits = c(0, y_max),
                     breaks = waiver()) +
  geom_vline(xintercept = 13.5, linetype = 2) +
  annotate("text", x=13.75, y=y_max*0.75,
           label = "Change from 4 week to \n13 week time bands",
           hjust = "left", size = 6.5/.pt) +
  labs(
    title = "Ongoing Waits",
    x = "Wait length in weeks",
    y = NULL,
    fill = "Guidance rules"
  ) +
  theme_phs() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8, family = "arial"),
    axis.text.y = element_text(size = 9, family = "arial"),
    axis.title.x = element_text(size = 9, family = "arial"),
    legend.text = element_text(size = 9, family = "arial"),
    legend.position = "bottom",
    axis.ticks = element_line(),
    panel.grid.major.y = element_line(colour = "black", linewidth = 0.1),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )



print(dow_ipdc_ongo)


#Export -----

ggsave("output/report/dow_nop_full.png",
       plot = grid.arrange(dow_nop_comp, dow_nop_ongo, nrow = 1),
       width = 16.5, height = 9, units = "cm")

ggsave("output/report/dow_ipdc_full.png",
       plot = grid.arrange(dow_ipdc_comp, dow_ipdc_ongo, nrow = 1),
       width = 16.5, height = 9, units = "cm")



# ggsave("output/report/dow_nop_ongo.png",
#   scale = 3,
#   plot = dow_nop_ongo)
# 
# ggsave("output/report/dow_nop_comp.png",
#        scale = 3,
#        plot = dow_nop_comp)
# 
# ggsave("output/report/dow_ipdc_ongo.png",
#        scale = 3,
#        plot = dow_ipdc_ongo)
# 
# ggsave("output/report/dow_ipdc_comp.png",
#        scale = 3,
#        plot = dow_ipdc_comp)
# 
