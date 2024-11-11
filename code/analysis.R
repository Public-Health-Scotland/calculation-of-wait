# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# analysis.R
# Angus Morton
# 2024-11-11
# 
# Produce the analytical outputs for the new wait calculation
# 
# R version 4.1.2 (2021-11-01)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Step 1 : top line figures ----

# number of waits which have changed length
changed_waits <- waits |> 
  filter(old_wait_length != new_wait_length) |> 
  nrow()

# Percentage of waits which have changed
changed_waits_p <- 100*changed_waits/(nrow(waits))

# Mean change for an adjusted wait that has changed
mean_difference <- waits |> 
  filter(old_wait_length != new_wait_length) |> 
  summarise(mean_diff = mean(new_wait_length-old_wait_length))


top_line <- data.frame(
  measure = c("Number of waits which have changed length",
              "Percentage of waits which have changed length",
              "Mean change for an adjusted wait that has changed"),
  value = c(changed_waits,
            changed_waits_p,
            mean_difference)
)

#### Step 2 : Tables ----

median_wait <- waits |> 
  summarise(median_new = median(new_wait_length)*7,
            median_old = median(old_wait_length)*7,
            `90th new` = quantile(new_wait_length, 0.9)*7,
            `90th old` = quantile(old_wait_length, 0.9)*7,
            over_52_new = sum(new_wait_length>52),
            over_52_old = sum(old_wait_length>52),
            med_diff_p = 100*(median_new-median_old)/median_old,
            `90th_diff_p` = 100*(`90th new`-`90th old`)/`90th old`,
            over_52_diff_p = 100*(over_52_new-over_52_old)/over_52_old)

board_medians <- waits |> 
  group_by(NHS_Board_of_Treatment) |> 
  summarise(median_new = median(new_wait_length)*7,
            median_old = median(old_wait_length)*7,
            `90th new` = quantile(new_wait_length, 0.9)*7,
            `90th old` = quantile(old_wait_length, 0.9)*7,
            over_52_new = sum(new_wait_length>52),
            over_52_old = sum(old_wait_length>52),
            med_diff_p = 100*(median_new-median_old)/median_old,
            `90th_diff_p` = 100*(`90th new`-`90th old`)/`90th old`,
            over_52_diff_p = 100*(over_52_new-over_52_old)/over_52_old) |> 
  ungroup()

spec_medians <- waits |> 
  group_by(Specialty) |> 
  summarise(median_new = median(new_wait_length)*7,
            median_old = median(old_wait_length)*7,
            `90th new` = quantile(new_wait_length, 0.9)*7,
            `90th old` = quantile(old_wait_length, 0.9)*7,
            over_52_new = sum(new_wait_length>52),
            over_52_old = sum(old_wait_length>52),
            med_diff_p = 100*(median_new-median_old)/median_old,
            `90th_diff_p` = 100*(`90th new`-`90th old`)/`90th old`,
            over_52_diff_p = 100*(over_52_new-over_52_old)/over_52_old) |> 
  ungroup()


# create bands based on planned care targets to look at records which
# have changed

waits_final_2 <- waits |> 
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


#### Step 3 : DoW graph ----

dow_hist <- waits |> 
  pivot_longer(cols = c(new_wait_length, old_wait_length)) |>
  ggplot(aes(x=value, fill=name)) + 
  geom_histogram(breaks = seq(0, 200, by = 13),
                 position = position_dodge())+
  scale_x_continuous(breaks = seq(0, 200, by=13))+
  scale_fill_manual(values = phs_colours(c("phs-magenta", "phs-purple")))+
  xlab("weeks waited") + 
  ylab("number waiting") +
  theme_phs()

#### Step 4 : Sankey chart ----


#### Step 5 : Exports ----

write_csv(top_line, paste0("output/", run_name, "_top_line_figures.csv"))

write_csv(waits_final_2, paste0("output/", run_name, "_band_changes.csv"))

write_csv(board_medians, paste0("output/", run_name, "_board_medians.csv"))

write_csv(spec_medians, paste0("output/", run_name, "_spec_medians.csv"))

ggsave(
  paste0("output/", run_name, "_DoW_chart.jpg"),
  scale = 3,
  plot = dow_hist)

