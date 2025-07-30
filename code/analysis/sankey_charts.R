# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# sankey_charts.R
# Angus Morton
# 2024-11-14
# 
# Sankey Charts
# 
# R version 4.1.2 (2021-11-01)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(networkD3)
library(readr)
library(dplyr)
library(ggplot2)
library(phsstyles)

#### Step 0 : Housekeeping ----

# Optionally overwrite run_name
#run_name <- "qe_jun_25"

rule <- expr(length_all_new_rules)


#### Step 1 : wrangling ----

waits <- read_rds(paste0("output/", run_name,
                         "/waits.rds"))

wait_band_changes <- waits |> 
  mutate(
    bin_new = case_when(
      {{ rule }}/7 < 52 ~ "0-52",
      between({{ rule }}/7, 52, 78) ~ "52-78",
      between({{ rule }}/7, 78, 104) ~ "78-104",
      {{ rule }}/7 >= 104 ~ "104+"
    ),
    bin_old = case_when(
      length_all_old_rules/7 < 52 ~ "0-52",
      between(length_all_old_rules/7, 52, 78) ~ "52-78",
      between(length_all_old_rules/7, 78, 104) ~ "78-104",
      length_all_old_rules/7 >= 104 ~ "104+"
    )) |> 
  mutate(bin_new = factor(bin_new, levels = c("0-52","52-78",
                                              "78-104","104+")),
         bin_old = factor(bin_old, levels = c("0-52","52-78",
                                              "78-104","104+"))) |> 
  filter(bin_new != bin_old) |> 
  count(bin_old, bin_new)

wait_band_changes_nop <- waits |> 
  filter(Patient_Type == "New Outpatient") |> 
  mutate(
    bin_new = case_when(
      {{ rule }}/7 < 52 ~ "0-52",
      between({{ rule }}/7, 52, 78) ~ "52-78",
      between({{ rule }}/7, 78, 104) ~ "78-104",
      {{ rule }}/7 >= 104 ~ "104+"
    ),
    bin_old = case_when(
      length_all_old_rules/7 < 52 ~ "0-52",
      between(length_all_old_rules/7, 52, 78) ~ "52-78",
      between(length_all_old_rules/7, 78, 104) ~ "78-104",
      length_all_old_rules/7 >= 104 ~ "104+"
    )) |> 
  mutate(bin_new = factor(bin_new, levels = c("0-52","52-78",
                                              "78-104","104+")),
         bin_old = factor(bin_old, levels = c("0-52","52-78",
                                              "78-104","104+"))) |> 
  filter(bin_new != bin_old) |> 
  count(bin_old, bin_new)

wait_band_changes_ipdc <- waits |> 
  filter(Patient_Type == "Inpatient/Day case") |> 
  mutate(
    bin_new = case_when(
      {{ rule }}/7 < 52 ~ "0-52",
      between({{ rule }}/7, 52, 78) ~ "52-78",
      between({{ rule }}/7, 78, 104) ~ "78-104",
      {{ rule }}/7 >= 104 ~ "104+"
    ),
    bin_old = case_when(
      length_all_old_rules/7 < 52 ~ "0-52",
      between(length_all_old_rules/7, 52, 78) ~ "52-78",
      between(length_all_old_rules/7, 78, 104) ~ "78-104",
      length_all_old_rules/7 >= 104 ~ "104+"
    )) |> 
  mutate(bin_new = factor(bin_new, levels = c("0-52","52-78",
                                              "78-104","104+")),
         bin_old = factor(bin_old, levels = c("0-52","52-78",
                                              "78-104","104+"))) |> 
  filter(bin_new != bin_old) |> 
  count(bin_old, bin_new)

#### Step 2 : Sankey diagram ----

# Change this object to look at NOP or IPDC specifically
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
                   nodePadding = 200,
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

