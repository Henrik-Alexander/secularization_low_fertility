#######################################
# Purpose: Random matching          #
# Author: Henrik-Alexander Schubert #
# E-Mail: schubert@demogr.mpg.de    #
# Date: 14.11.2023                  #
#####################################

rm(list = ls())

# Load the packages
library(tidyverse)
library(data.table)
library(ggrepel)

# Load functions
source("functions/functions.R")
source("functions/graphics.R")
source("functions/simulation_functions.R")

# Load the data
load("data/group_rel.Rda")

# Set the number of repetitions
repetitions <- 150

# Years
years <- 1996:2018
reference_year <- 2000

# Groups 
groups <- c("Homogamous: religiously unaffiliated", "Mixed", "Homogamous: religiously affiliated")


### Create the sample ---------------------------------

# Filter the data
d <- d[year >= 1995 & year <= 2017 & age >= 18 & age <= 50, ]

# Remove singles
c <- d[group != "single",]

# Recode the religious variable
c$religious <- ifelse(c$religious == "Religiously affiliated", 1, 0)

# Combine the religiosity information
c$group <- combine_cat(c$religious, c$religious_par)

### Preparation ---------------------------------------

# Remove missing observations
c <- c[!is.na(group), ]

# Estimate the share of observed partnerships
c[, total := .N, by = .(year)]
obs <- c[, .(share_obs = .N / unique(total),
             observations = .N,
             total = unique(total)), by = .(group, year)]

# Plot the observed share
ggplot(obs, aes(x = year, y = share_obs, fill = group, group = group)) +
  geom_col() +
  scale_x_continuous("Year", breaks = seq(1995, 2019, by = 2), expand = c(0, 0)) +
  scale_y_continuous("Proportin of couples (in %)", label = scales::percent, expand = c(0, 0)) +
  scale_fill_manual(values = colors[c(1, 2, 4)], name = "")
ggsave(last_plot(), filename = "figures/share_couple_comp_obs.pdf")

# Save the observed data
save(obs, file = "results/couple_comp_obs.Rda")

# 1. Deming-Stephen algorithm -----------------------------

# Create the observed frequency tables
c <- c[sex == "Female" & sex_par == "Male"]
years <- min(c$year):max(c$year)
count_fre <- obser_fre <- vector("list", length = length(years))
names(count_fre) <- names(obser_fre) <- years

# Split the data by years
data_splitted <- split(c, c$year)

# Create the first table
tmp <- data_splitted[[paste(reference_year)]]
count_fre[[paste(reference_year)]] <- obser_fre[[paste(reference_year)]] <- table("Female" = tmp$religious, "Male" = tmp$religious_par)

### Algorithm
for(year in years) {
  cat("Year:", year, "\n")
  tmp <- data_splitted[[paste(year)]]
  obser_fre[[paste(year)]] <- (table("Female" = tmp$religious, "Male" = tmp$religious_par))
  count_fre[[paste(year)]] <- deming_stephan(row_tar = rowSums(obser_fre[[paste(year)]]), 
                                             col_tar = colSums(obser_fre[[paste(year)]]),
                                             reference = obser_fre[[paste(reference_year)]])
  rm(tmp)
}

# Estimate the shares
obser_fre <- map_df(obser_fre, est_share, .id = "year")
count_fre <- map_df(count_fre, est_share, .id = "year")

# Join the data
data_ipf <- inner_join(obser_fre, count_fre, by = c("year", "Female", "Male"), suffix = c("_observed", "_counterfactual")) 
data_ipf$year <- as.numeric(data_ipf$year)


data_ipf %>% 
  pivot_longer(cols = starts_with("value"), names_prefix = "value_") %>% 
  mutate(partnership = case_when(Female == 0 & Male == 0 ~ "Homogamous: religiously unaffiliated",
                                 Female != Male  ~ "Heterogamous: religiously affiliated",
                                 Female == 1 & Male == 1  ~ "Homogamous: religiously affiliated")) %>% 
  group_by(year, name) %>% 
  mutate(total = sum(value)) %>% 
  group_by(year, name, partnership) %>% 
  summarise(value = sum(value) / unique(total), .groups = "drop") %>% 
  ggplot(aes(x = year, y = value, colour = name, group = name, linetype = name)) +
  geom_vline(xintercept = 2000, colour = "grey") +
  geom_line(size = 2) + 
  scale_colour_manual(values = colors[c(1, 4)], name = "Scenario") +
  scale_linetype_manual(name = "Scenario", values = c("counterfactual" = "dotdash", "observed" = 'solid')) +
  scale_y_continuous("Population share", expand = c(0, 0.02), n.breaks = 10, labels = scales::percent) +
  scale_x_continuous("Year", expand = c(0, 0)) +
  facet_wrap( ~ partnership, scales = "free_y") +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "bottom",
    panel.spacing.x = unit(0.5, "cm")
  )

# Save the graph
figs(last_plot(), "ipf_counterfactual_matching", height = 25, width = 30)

data_ipf %>% 
  mutate(difference = (value_observed - value_counterfactual) / value_counterfactual,
         partnership = case_when(Female == 0 & Male == 0 ~ "Homogamous: religiously unaffiliated",
                                 Female != Male  ~ "Heterogamous: religiously affiliated",
                                 Female == 1 & Male == 1  ~ "Homogamous: religiously affiliated")) %>% 
  group_by(year, partnership) %>% 
  summarise(difference = mean(difference), .groups = "drop") %>% 
  ggplot(aes(x = year, y = difference, colour = partnership, group = partnership)) +
  geom_vline(xintercept = 2000, colour = "grey") +
  geom_line(size = 2) + 
  geom_hline(yintercept = 0) +
  scale_colour_manual(values = colors[c(1, 2, 4)], name = "Scenario") +
  scale_y_continuous("Difference: observed - counterfactual", expand = c(0, 0.02), n.breaks = 10, labels = scales::percent) +
  scale_x_continuous("Year", expand = c(0, 0)) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "bottom",
    panel.spacing.x = unit(0.5, "cm")
  ) +
  guides(colour = guide_legend(nrow = 2, byrow = T))

# Save the graph
figs(last_plot(), "ipf_counterfactual_matching_diff")

# 2. Random matching simulation ---------------------------

# Create results
result <- array(NA, dim = c(length(years), repetitions, length(unique(groups))), dimnames = list(years, 1:repetitions, unique(groups)))

# Remove unneccecary variables
c <- c[, .(year, id, pid, age, age_par, religious, religious_par)]

# Create male and female data
male <- c[, .(year, pid, age_par, religious_par)]
female <- c[, .(year, id, age, religious)]

# Random matching algorithm
for (i in years) {
  cat("Year:", i, "================", "\n")
  # Select the data
  tmp <- female$religious[female$year == i]
  tmp2 <- male$religious_par[male$year == i]
  # Get the number of observed matches
  n <- length(tmp)
  for(rep in seq_along(1:repetitions)) {
    cat("Iteration:", rep, "\n")
    # Match the respontends
    rel_count <- tmp[sample(1:n, size = n, replace = F)]
    res <- combine_cat(rel_count, tmp2)
    # Estimate the share
    result[paste0(i), rep, ] <- estimate_prop(res)
    }
}

# Combine the data
result <- as.data.frame.table(result)
names(result) <- c("year", "iteration", "group", "share_sim")
result$year <- as.numeric(paste(result$year))

# Save the results
save(result, file = "results/random_matching_sim.Rda")

random_distribution <- result %>% 
  filter(year %in% c(2017)) %>% 
  ggplot(aes(x = share_sim/100, group = year, fill = group, colour = group)) +
    stat_density(alpha = 0.6) +
    facet_grid( ~ group, scales = "free_x") +
    scale_x_continuous("Proportion among randomly matched couples (%)",  labels = scales::percent_format(accuracy = 0.01)) +
    scale_y_continuous("Count",  expand = c(0, 0)) +
    scale_fill_manual(values = colors[c(1,2, 4)], name = "Couple composition:") +
    scale_colour_manual(values = colors[c(1,2, 4)], name = "Couple composition:") +
    guides(colour = "none", fill = "none") +
    theme(strip.background = element_blank(),
          panel.spacing.x =  unit(0.5, "cm"),
          axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(face = "bold", size = 18))
figs(last_plot(), "distr_sim_coupl_2017")


# Plot the data
result <- result %>% 
  filter(!is.na(group))


obs <- obs %>% 
  filter(!is.na(group))  %>% 
  mutate(group = factor(group, labels = groups))
plot_sim <- ggplot(result, aes(x = year, y = share_sim/100, colour = group)) +
  geom_violin(aes(group = year), alpha = 0.3) +
  geom_line(data = subset(obs, !is.na(group)), aes(x = year, y = share_obs, colour = group), size = 2) +
  facet_wrap(  ~ group) +
  scale_colour_manual(values = colors[c(1, 2, 4)], name = "Couple composition:") +
  scale_x_continuous("Year", expand = c(0, 0), limits = c(1995, 2018)) +
  scale_y_continuous("Share", expand = c(0, 0), limits = c(0, .85), labels = scales::percent_format(accuracy = 1L), n.breaks = 10) +
  geom_text(data = subset(obs, year == 2000), 
                  aes(x = year, y = share_obs+0.02, label = "observed"), 
                  family = "serif", size = 6) +
  geom_text(data = subset(result, iteration == "1" & year == 2000),
                  aes(x = year, y = (share_sim/100)+0.02, label = "simulated"),
                  family = "serif", size = 6) +
  guides(colour = "none") +
    theme(strip.background = element_blank(),
          strip.text = element_text(face = "bold", size = 12))

figs(plot_sim, "share_couple_comp_sim")


# 3. Counterfactual couple-TFR1s trend ------------------------------

# Load the final data
load("data/final_data.Rda")

# Filter the data to higher than 1996
d <- d[year > 1995, ]

# Get the dimensions
age_groups <- unique(d$age_group)
years <- unique(d$year)

# Create composition i: paste rel and rel_par
# Four compositions: 0-0, 1-0, 0-1, 1-1
d[, composition := paste0(rel, "-", rel_par)]
d[, age_group := cut(age, breaks = seq(15, 55, by = 5), right = F, include.lowest = T)]

## Create the fertility rates ------

# Aggregate the exposures by age and composition
expos <- d[, .(expos = .N), by = .(year, age_group, composition)]

# Aggregate the births by age and composition
births <- d[, .(births = sum(birth)), by = .(year, age_group, composition)]
  
# Estimate f(i,x): age-composition-specific fertility rate
asfr <- expos[births, on = .(year, age_group, composition)]
asfr[, asfr_group := births / expos]
asfr[, asfr_total := sum(births) / sum(expos), by = .(year, age_group)]

## Create the counterfactual population exposures -----

# Split the data
d_split <- split(d, list(d$age_group))

# Create the containers
count_expos_fem <- count_expos_mal <- count_expos_bot <- vector("list", length = length(age_groups) * length(years))
labels <- as.vector(outer(years, age_groups, FUN = paste, sep = "_"))
names(count_expos_fem) <- names(count_expos_mal) <- names(count_expos_bot) <- labels

# Create the tables
for (age_group in names(d_split)){
  print("------------------")
  print(age_group)
  tmp <- d_split[[age_group]]
  tmp <- split(tmp, tmp$year)
  tmp <- lapply(tmp, function(x) table("female" = x[["rel"]], "male" = x[["rel_par"]]))
  ref_table <- tmp$`2000`
  for(year in names(tmp)) {
    print(year)
    obs_table <- tmp[[year]]
    lab <- paste0(year, "_", age_group)
    count_row <- (rowSums(ref_table) / sum(rowSums(ref_table))) * sum(obs_table)
    count_col <- (colSums(ref_table) / sum(colSums(ref_table))) * sum(obs_table)
    count_expos_fem[[lab]] <- deming_stephan(row_tar = count_row, col_tar = colSums(obs_table), reference = obs_table)
    count_expos_mal[[lab]] <- deming_stephan(row_tar = rowSums(obs_table), col_tar = count_col, reference = obs_table)
    count_expos_bot[[lab]] <- deming_stephan(row_tar = count_row, col_tar = count_col, reference = obs_table)
  }
  rm(tmp)
}

# Clean the data
clean_count_expos <- function(data, colname) {
  selector <- unlist(sapply(data, sum)) != 0
  count_expos <- rbindlist(map(data[selector], data.table::melt), idcol = "label")
  count_expos$year <- as.numeric(str_sub(count_expos$label, start = 1, end = 4))
  count_expos$age_group <- str_sub(count_expos$label, start = 6, end = 12)
  count_expos$composition <- paste0(count_expos$female, "-", count_expos$male)
  count_expos <- count_expos[, .(year, age_group, composition, value)]
  setnames(count_expos, "value", colname)
  return(count_expos)
}

# Clean the data
count_expos_fem <- clean_count_expos(count_expos_fem, colname = "count_expos_female")
count_expos_mal <- clean_count_expos(count_expos_mal, colname = "count_expos_male")
count_expos_bot <- clean_count_expos(count_expos_bot, colname = "count_expos_both")

# Combine the data
asfr <- asfr[count_expos_fem, on = .(year, age_group, composition)]
asfr <- asfr[count_expos_mal, on = .(year, age_group, composition)]
asfr <- asfr[count_expos_bot, on = .(year, age_group, composition)]

## Estimate the TFRs ------------------------------------------

# Estimate the group weight
asfr[, count_weight_female := unique(count_expos_female) / sum(count_expos_female, na.rm = T), by = .(year, age_group)]
asfr[, count_weight_male := unique(count_expos_male) / sum(count_expos_male, na.rm = T), by = .(year, age_group)]
asfr[, count_weight_both := unique(count_expos_both) / sum(count_expos_both, na.rm = T), by = .(year, age_group)]
asfr[, obs_weight := expos / sum(expos, na.rm = T), by = .(year, age_group)]

# Estimate the asfrs
tfr1 <- asfr[, .(tfr1_obs = sum(obs_weight * asfr_group * 5),
         tfr1_count_female = sum(count_weight_female * asfr_group * 5),
         tfr1_count_male = sum(count_weight_male * asfr_group * 5),
         tfr1_count_both = sum(count_weight_both * asfr_group * 5)), by = .(year)]

# Plot the result
tfr1 %>% 
  filter(year > 1995) %>% 
  pivot_longer(cols = starts_with("tfr1"), names_prefix = "tfr1_", values_to = "tfr1") %>% 
  mutate(observed = ifelse(str_detect(name, "obs"), "observed", "counterfacual"),
         sex = str_extract(name, "[a-z]+$")) %>% 
  ggplot(aes(x = year, y = tfr1, linetype = observed, colour = sex)) +
    geom_vline(xintercept = 2000) +
    geom_line(size = 2) +
    scale_linetype_manual(name = "", values = c("dotdash", "solid")) +
    scale_colour_manual(values = colors[4:1]) +
    scale_x_continuous("Year", expand = c(0, 0)) +
    guides(linetype = "none") 
figs(last_plot(), "tfr1_count", height = 15)


# Plot the result
tfr1 %>% 
  pivot_longer(cols = contains("count"), names_prefix = "tfr1_", values_to = "tfr1") %>% 
  mutate(difference = tfr1_obs - tfr1,
         scenario = str_extract(name, "[a-z]+$")) %>% 
  ggplot(aes(x = year, y = difference, colour = scenario)) +
  geom_vline(xintercept = 2000, colour = "red") +
  geom_hline(yintercept = 0) +
  geom_line(size = 2) +
  scale_linetype_manual(name = "", values = c("dashed", "solid")) +
  scale_colour_manual(values = colors) +
  scale_x_continuous("Year", expand = c(0, 0)) +
  scale_y_continuous("Difference: observed - counterfactual") +
  guides(linetype = "none")
figs(last_plot(), "tfr1_count_diff", height = 15)

# Plot the result
tfr1 %>% 
  pivot_longer(cols = contains("count"), names_prefix = "tfr1_", values_to = "tfr1") %>% 
  mutate(difference = (tfr1_obs - tfr1)/tfr1_obs,
         scenario = str_extract(name, "[a-z]+$")) %>% 
  ggplot(aes(x = year, y = difference, colour = scenario)) +
  geom_vline(xintercept = 2000, colour = "grey", size =1.3, linetype = "dashed") +
  geom_hline(yintercept = 0) +
  geom_line(size = 2) +
  scale_linetype_manual(name = "", values = c("dashed", "solid")) +
  scale_colour_manual(values = colors) +
  scale_x_continuous("Year", expand = c(0, 0)) +
  scale_y_continuous("Difference: observed - counterfactual", labels = scales::percent_format()) +
  guides(linetype = "none", col="none") +
  annotate("text",x=2015, y=-0.01, label="Male", color=colors[3], size=7,family="serif") +
  annotate("text",x=2015, y=-0.02, label="Female", color=colors[2], size=7, family="serif") +
  annotate("text",x=2015.2, y=-0.033, label="Both", color=colors[1], size=7,family="serif")

figs(last_plot(), "tfr1_count_percent_diff", height = 15)

#
ref_value <- tfr1$tfr1_obs[tfr1$year == 2000]
tfr1 %>% 
  filter(year > 1995) %>% 
  pivot_longer(cols = !year, names_prefix = "tfr1_", values_to = "tfr1") %>% 
  mutate(tfr1 = 100 * tfr1 / ref_value,
         scenario = str_extract(name, "[a-z]+$"),
         observed = ifelse(str_detect(scenario, "obs"), "observed", "counterfactual")) %>% 
  ggplot(aes(x = year, y = tfr1, colour = scenario, linetype = observed)) +
  geom_vline(xintercept = 2000, colour = "grey") +
  geom_hline(yintercept = 100) +
  geom_line(size = 2) +
  scale_linetype_manual(name = "", values = c("dotdash", "solid")) +
  scale_colour_manual(values = colors[4:1], name = "Senario") +
  scale_x_continuous("Year", expand = c(0, 0)) +
  scale_y_continuous("Fertility in couples (Index = 100)") +
  guides(linetype = "none") +
  theme(legend.position = c(0.9, 0.8))
figs(last_plot(), "tfr1_count_index_diff", height = 15)


### END #######################################################