# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# analysis.R
# Angus Morton
# 2024-11-11
# 
# Produce the analytical outputs for the new wait calculation
# 
# R version 4.1.2 (2021-11-01)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Step 0 : Housekeeping ----


#
median_wait <- waits_final |> 
  summarise(median_new = median(new_wait_length)*7,
            median_old = median(old_wait_length)*7,
            `90th new` = quantile(new_wait_length, 0.9)*7,
            `90th old` = quantile(old_wait_length, 0.9)*7)

board_medians <- waits_final |> 
  group_by(NHS_Board_of_Treatment) |> 
  summarise(median_new = median(new_wait_length)*7,
            median_old = median(old_wait_length)*7,
            `90th new` = quantile(new_wait_length, 0.9)*7,
            `90th old` = quantile(old_wait_length, 0.9)*7,
            med_diff_p = 100*(median_new-median_old)/median_old,
            `90th_diff_p` = 100*(`90th new`-`90th old`)/`90th old`) |> 
  ungroup()

spec_medians <- waits_final |> 
  group_by(Specialty) |> 
  summarise(median_new = median(new_wait_length)*7,
            median_old = median(old_wait_length)*7,
            `90th new` = quantile(new_wait_length, 0.9)*7,
            `90th old` = quantile(old_wait_length, 0.9)*7) |> 
  ungroup()

pub_path <- paste0("/PHI_conf/WaitingTimes/SoT/Publications/Inpatient, Day case and Outpatient Stage of Treatment Waiting Times/20240827/Output/R Output/PerformanceIPDC.csv")

pub_medians <- read_csv(pub_path) |> 
  mutate(Date = dmy(Date)) |> 
  filter(`Ongoing/Completed` == "Ongoing",
         `NHS Board of Treatment` != "NHS Scotland (Excluding NHS Tayside)",
         Date == dmy("30/06/2024")) |> 
  select(`NHS Board of Treatment`, Specialty, Median, `90th Percentile`) |> 
  rename(NHS_Board_of_Treatment = `NHS Board of Treatment`)

new_pub_comp <- waits_final |> 
  left_join(pub_medians, by = c("NHS_Board_of_Treatment", "Specialty")) |> 
  group_by(NHS_Board_of_Treatment) |> 
  summarise(median_new = median(new_wait_length)*7,
            median_old = median(old_wait_length)*7,
            `90th new` = quantile(new_wait_length, 0.9)*7,
            `90th old` = quantile(old_wait_length, 0.9)*7) |> 
  ungroup()

# create bands based on planned care targets to look at records which
# have changed
waits_final_2 <- waits_final |> 
  mutate(
    bin_new = case_when(
      new_wait_length < 52 ~ "0-52",
      between(new_wait_length, 52, 78) ~ "52-78",
      between(new_wait_length, 78, 104) ~ "78-104",
      new_wait_length >= 104 ~ "104+"
    ),
    bin_old = case_when(
      old_wait_length < 52 ~ "0-52",
      between(old_wait_length, 52, 78) ~ "52-78",
      between(old_wait_length, 78, 104) ~ "78-104",
      old_wait_length >= 104 ~ "104+"
    )) |> 
  mutate(bin_new = factor(bin_new, levels = c("0-52","52-78",
                                              "78-104","104+")),
         bin_old = factor(bin_old, levels = c("0-52","52-78",
                                              "78-104","104+"))) |> 
  filter(bin_new != bin_old) |> 
  count(bin_old, bin_new)

# number of waits which have changed length
changed_waits <- waits_final |> 
  filter(old_wait_length != new_wait_length) |> 
  nrow()

# Percentage of waits which have changed
changed_waits_p <- 100*changed_waits/(nrow(waits_final))

# Mean change for an adjusted wait that has changed
mean_difference <- waits_final |> 
  filter(old_wait_length != new_wait_length) |> 
  summarise(mean_diff = mean(new_wait_length-old_wait_length))


#### Step 9 : Graphs ----


dow_hist <- waits_final |> 
  pivot_longer(cols = c(new_wait_length, old_wait_length)) |>
  ggplot(aes(x=value, fill=name)) + 
  geom_histogram(breaks = seq(0, 200, by = 13),
                 position = position_dodge())+
  scale_x_continuous(breaks = seq(0, 200, by=13))+
  scale_fill_manual(values = phs_colours(c("phs-magenta", "phs-purple")))+
  xlab("weeks waited") + 
  ylab("number waiting") +
  theme_phs()


#### Step 10 : optional exports ----

ggsave(
  "output/DoW_chart.jpg",
  scale = 3,
  plot = dow_hist)

write_csv(waits_final_2, "output/band_changes.csv")

write_csv(board_medians, "output/board_medians.csv")
