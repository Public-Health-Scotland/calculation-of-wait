# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# analysis.R
# Angus Morton
# 2024-11-11
# 
# Produce the analytical outputs for the new wait calculation
# 
# R version 4.1.2 (2021-11-01)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(networkD3)
library(dplyr)
library(ggplot2)
library(phsstyles)

ipdc_groupings <- read.xlsx("spec_groupings/IPDC.xlsx")
nop_groupings <- read.xlsx("spec_groupings/NOP.xlsx")

groupings <- ipdc_groupings |> 
  full_join(nop_groupings, by = "Specialty") |> 
  mutate(grouped_specialty.x = if_else(is.na(grouped_specialty.x),
                                       grouped_specialty.y,
                                       grouped_specialty.x)) |> 
  select(Specialty, grouped_specialty = grouped_specialty.x)

waits <- waits |> 
  left_join(ipdc_groupings, by = "Specialty")

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

top_10 <- waits |> 
  count(grouped_specialty) |> 
  arrange(desc(n)) |> 
  head(10) |> 
  select(grouped_specialty) |> 
  pull()
  

spec_medians <- waits |> 
  filter(grouped_specialty %in% top_10) |> 
  group_by(grouped_specialty) |> 
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

wait_band_changes <- waits |> 
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

sankey_flows <- wait_band_changes |> 
  mutate(bin_old = paste0(as.character(bin_old), " old"),
         bin_new = paste0(as.character(bin_new), " new"))


# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(sankey_flows$bin_old), 
         as.character(sankey_flows$bin_new)) %>% unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
sankey_flows$IDsource <- match(sankey_flows$bin_old, nodes$name)-1 
sankey_flows$IDtarget <- match(sankey_flows$bin_new, nodes$name)-1

# Make the Network
p <- sankeyNetwork(Links = sankey_flows, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "n", NodeID = "name", 
                   sinksRight=FALSE,
                   nodePadding = 100,
                   fontSize = 20,
                   margin = 100)

javascript_string <-
  'function(el, x){
    d3.select(el).selectAll(".node text")
      .text(d => d.name + " (" + d.value + ")");
  }'

htmlwidgets::onRender(x = p, jsCode = javascript_string)

p$x$nodes <-
  p$x$nodes %>% 
  mutate(is_source_node = name %in% sankey_flows$bin_old)

htmlwidgets::onRender(
  x = p,
  jsCode = '
  function(el,x) {
  d3.select(el)
    .selectAll(".node text")
    .filter(function(d) { return d.is_source_node; })
    .attr("x", x.options.nodeWidth - 17)
    .attr("text-anchor", "end");
  
  d3.select(el)
    .selectAll(".node text")
    .filter(function(d) { return !d.is_source_node; })
    .attr("x", x.options.nodeWidth + 1)
    .attr("text-anchor", "start");
  
  d3.select(el).selectAll(".node text")
      .text(d => d.name + " (" + d.value + ")");
  }
  '
)

p


#### Step 5 : Exports ----

write_csv(top_line, paste0("output/", run_name, "_top_line_figures.csv"))

write_csv(wait_band_changes, paste0("output/", run_name, "_band_changes.csv"))

write_csv(board_medians, paste0("output/", run_name, "_board_medians.csv"))

write_csv(spec_medians, paste0("output/", run_name, "_spec_medians.csv"))

ggsave(
  paste0("output/", run_name, "_DoW_chart.jpg"),
  scale = 3,
  plot = dow_hist)

