#############################################
# Project: Secularization and low fertility
# Purpose: Demographics                 
# Author: Henrik-Alexander Schubert         
# Date: 16.06.2023                          
# E-Mail: schubert@demogr.mpg.de            
#############################################

rm(list = ls())

# Load packages
library(tidyverse)
library(data.table)
library(ggrepel)

# Load functions
source("functions/functions.R")
source("functions/graphics.R")

# Load data
load("data/analysis.Rda")

### Functions ------------------------------------

est_tfr <- function (pars) {
  
  # Convert the vector into something we can use
  N <- length(pars)
  dim(pars) <- c(N / 2, 2)
  
  # Calculate the rate
  tfr <- sum(pars[, 1] * pars[, 2])
  
  return(tfr)
}

### Estimate births and exposures ----------------

# Filter Finnish and finnish born
basic <- basic[ori %in% c(11, 12), ]

# Impute religious
basic <- basic[order(id, year), ]

# Filter if sex and religious is not missing
basic <- basic[!is.na(sex) & !is.na(religious), ]

# Estimate exposures
exp <- basic[, .(pop = .N), by = .(year, religious, age, sex)]
exp <- exp[order(sex, religious, age, year), ]
exp <- exp[, exp := (pop + shift(pop, n = 1, type = "lag")) / 2, by = .(religious, age, sex)]

# Estimate births
births <- basic[, .(births = sum(birth)), by = .(year, religious, age, sex)]

# Combine the data
fert <- births[exp, on = .(year, religious, age, sex), nomatch = NULL]

# Estimate the age and group specific fertility rate
fert <- fert[, asfr := births / exp]

# Estimate fertility rate
plot <- fert[year <= 2019, .(tfr = sum(asfr, na.rm = TRUE)), by = .(year, religious, sex)] %>% 
  filter(sex == "Female", year <= 2017, year > 1996) %>% 
  ggplot(aes(x = year, y = tfr, group = religious, colour = as.factor(religious), shape = as.factor(religious))) +
    geom_line() +
    geom_point(size = 2) +
    scale_colour_manual(name = "", labels = c("Religiously unaffiliated", "Religiously affiliated"), values = colors) +
    scale_shape_discrete(name = "", labels = c("Religiously unaffiliated", "Religiously affiliated")) +
    scale_x_continuous(breaks = seq(1998, 2018, by = 2), expand = c(0, 0.2)) +
    scale_y_continuous(limits = c(0, 2), expand = c(0, 0)) +
    ylab("Total Fertility Rate") + xlab("Year")
ggsave(plot, filename = "figures/tfr_rel.pdf")

# Save the fertility
save(fert, file = "data/fertility_data.Rda")
rm(basic)

### Plot the population exposure -----------------

# Load the data
load("data/fertility_data.Rda")

# Filter the relevant years
fert <- fert[year >= 1996 & year < 2020, ]

# Plot the population share of religous
fert %>% 
  filter(sex == "Female" & year >= 1996) %>%  
  group_by(year) %>% 
  mutate(total = sum(exp, na.rm = T)) %>% 
  group_by(year, religious) %>% 
  summarise(share = 100 * sum(exp) / unique(total), .groups = "drop") %>% 
  ggplot(aes(x = year, y = share, fill = as.factor(religious))) +
  geom_col() +
  scale_fill_manual(values = colors, labels = c("Religiously unaffiliated", "Religiously affiliated"), name = "") +
  scale_x_continuous(expand = c(0, 0), n.breaks = 12) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
  ylab("Proportion religious (%)") +
  xlab("Year")

# Save the plot
ggsave(last_plot(), filename = "figures/rel_exp_trend.pdf", height = 10, width = 15, unit = "cm")

# Estimate the Total Fertility Rate
tfr_fin <- fert %>% 
  group_by(sex, age, year) %>% 
  summarise(asfr   = sum(births, na.rm = T) / sum(exp, na.rm = T)) %>% 
  group_by(sex, year) %>% 
  summarise(tfr_fin = sum(asfr, na.rm = T),
            mac_fin    = sum(asfr * age, na.rm = T) / sum(asfr, na.rm = T),
            .groups = "drop")

# Join finnish fertility with the other
fert_comb <- fert %>% 
  mutate( asfr   = births / exp) %>% 
  group_by(sex, year, religious) %>% 
  summarise(tfr = sum(asfr, na.rm = T),
            mac    = sum(asfr * age, na.rm = T) / sum(asfr, na.rm = T),
            .groups = "drop") %>% 
  pivot_wider(names_from = "religious", values_from = c("tfr", "mac"), names_prefix = "rel_") %>% 
  left_join(., tfr_fin, by = c("sex", "year")) 

# Plot the TFRS
tfr_data <- fert_comb  %>% 
  pivot_longer(cols = starts_with("tfr"), names_prefix = "tfr_") %>% 
  filter(!is.na(name) & !is.na(sex)) %>% 
  select(sex, year, name, value)
tfr_religious <- ggplot(tfr_data, aes(x = year, y = value, colour = name, shape = name)) +
    geom_line(size = 1.3) +
    geom_point(size = 3) +
    scale_x_continuous("Year", n.breaks = 8, expand = c(0, 0.2)) +
    scale_y_continuous("Total Fertility Rate", n.breaks = 10, expand = c(0.1, 0)) +
    scale_colour_manual(values = colors, name = "Total Fertility Rate:", labels = c("Combined", "Religiously unaffiliated", "Religiously affiliated"), na.translate = F) +
    scale_shape(name = "Total Fertility Rate:", labels = c("Combined", "Religiously unaffiliated", "Religiously affiliated")) +
    facet_wrap( ~ sex) +
    theme()
figs(tfr_religious, "tfr_rel_mf")  

# Plot the trend in the female Total Fertility Rate
tfr_religious_f <- fert_comb  %>% 
  pivot_longer(cols = starts_with("tfr"), names_prefix = "tfr_")  %>% 
  filter(sex == "Female") %>% 
  ggplot(aes(x = year)) +
  geom_line(size = 2, aes(colour = name, y = value)) +
  geom_point(size = 4, aes(colour = name, shape = name, y = value)) +
  scale_colour_manual(values = colors, name = "", labels = c("Combined", "Religiously unaffiliated", "Religiously affiliated")) +
  scale_shape_manual(values = shapes, name = "", labels = c("Combined", "Religiously unaffiliated", "Religiously affiliated")) +
  scale_x_continuous("Year", n.breaks = 12, expand = c(0, 0.2)) +
  scale_y_continuous("Total Fertility Rate", n.breaks = 10, expand = c(0, 0.2)) 
figs(tfr_religious_f, "tfr_rel_f") 

# Plot the trend in mean age of childbearing
mac_religious <- fert_comb  %>% 
  pivot_longer(cols = starts_with("mac"), names_prefix = "mac_")  %>% 
  filter(!is.na(name) & !is.na(sex)) %>% 
  ggplot(aes(x = year, y = value, colour = name, shape = sex)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous("Year", breaks = scales::pretty_breaks(), expand = c(0, 0.2)) +
  scale_y_continuous("Mean age of childbearing", breaks = scales::pretty_breaks(), expand = c(0, 0.2)) +
  scale_colour_manual(values = colors, name = "Religious affiliation:", labels = c("Combined", "Religiously unaffiliated", "Religiously affiliated")) +
  facet_wrap( ~ sex) +
  guides(shape = "none") + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18, face = "bold"))

figs(mac_religious, "mac_rel_mf", height = 13)  

### Counterfactual simulation ------------------------

# Estimate the counterfactual 
counterfactual <- fert %>% 
  filter(year == 2000) %>% 
  group_by(age, sex) %>% 
  mutate(counterfactual = exp / sum(exp, na.rm = T)) %>% 
  select(sex, age, counterfactual, religious)

# Combine the data
counterfactual <- left_join(fert, counterfactual, by = c("sex", "age", "religious")) %>% 
  group_by(age, year, sex) %>% 
  mutate(observed = exp / sum(exp)) %>% 
  group_by(sex, year) %>% 
  summarise(tfr_count = sum(asfr * counterfactual, na.rm = T),
            tfr_obs   = sum(asfr * observed, na.rm = T),
            .groups = "drop")


# Plot the result
counterfact_tfr <- ggplot(subset(counterfactual, subset = year %in% 1990:2020 & !is.na(sex)), aes(x = year, colour = sex, fill = sex)) +
  geom_vline(xintercept = 2000, size = 1.5, colour = "grey") +
  geom_line(aes(y = tfr_count, linetype = "Counterfactual"), size = 2) +
  geom_line(aes(y = tfr_obs, linetype = "Observed"), size = 2) +
  geom_ribbon(aes(ymin = tfr_obs, ymax = tfr_count), alpha = 0.2) +
  scale_colour_manual(name = "", values = colors) +
  scale_fill_manual(name = "Sex", values = colors) +
  scale_linetype_manual(name = "", values = c("dotdash", "solid")) +
  facet_wrap( ~ sex) +
  scale_y_continuous("Total Fertility Rate", expand = c(0.02, 0), n.breaks = 10) +
  scale_x_continuous("Year", expand = c(0, 0), breaks = seq(1996, 2018, by = 4)) +
  guides(fill = "none", colour = "none", linetype = "none") 

# Save the graph
figs(counterfact_tfr, "count_tfr_mf", height = 14, width = 28)

# Plot the counterfactual with the observed rates in one plot
fert_comb  %>% 
  pivot_longer(cols = starts_with("tfr"), names_prefix = "tfr_")  %>% 
  filter(sex == "Female") %>% 
  ggplot(aes(x = year)) +
  geom_line(data = counterfactual[counterfactual$sex == "Female",  ],
            aes(y = tfr_count), size = 2, colour = colors[1], linetype = "dotdash") +
  geom_ribbon(data = counterfactual[counterfactual$sex == "Female",  ],
              aes(x = year, ymin = tfr_obs, ymax = tfr_count), fill = colors[1], size = 2, alpha = .4) +
  geom_line(size = 2, aes(colour = name, y = value)) +
  #geom_point(size = 4, aes(colour = name, shape = name, y = value)) +
  scale_colour_manual(values = colors, name = "", labels = c("Combined", "Religiously unaffiliated", "Religiously affiliated")) +
  scale_shape_discrete(name = "", labels = c("Combined", "Religiously unaffiliated", "Religiously affiliated")) +
  scale_x_continuous("Year", n.breaks = 12, expand = c(0, 0)) +
  scale_y_continuous("Total Fertility Rate", n.breaks = 10, expand = c(0, 0.1))
figs(last_plot(), "trend_tfr_count_f")


# Counterfactual
counterfactual <- counterfactual %>% 
  mutate(diff = round(tfr_count - tfr_obs, 2))

counterfactual_female <- ggplot(subset(counterfactual, subset = year %in% 1990:2018 & sex == "Female"), aes(x = year)) +
  geom_vline(xintercept = 2000, colour = "grey", size = 1.5) +
  geom_line(aes(y = tfr_count, linetype = "Counterfactual"), size = 2, colour = colors[1]) +
  geom_line(aes(y = tfr_obs, linetype = "Observed"), size = 2, colour = colors[1]) +
  geom_ribbon(aes(ymin = tfr_obs, ymax = tfr_count), alpha = 0.2, fill = colors[1]) +
  geom_text_repel(data = counterfactual %>% filter(year %in% c(2002, 2016) & sex == colors[1]),
            aes(x = year, y = (tfr_count + tfr_obs) / 2, label = diff),
            size = 8, family = "serif", nudge_x = 1, nudge_y = -0.01) +
  geom_text(data = subset(counterfactual, year == 2010 & sex == "Female"),
            aes(x = year, y = (tfr_count + tfr_obs) / 2, label = diff),
            size = 8, family = "serif") +
  scale_linetype_manual(name = "", values = c("dotted", "solid")) +
  scale_y_continuous("Total Fertility Rate", n.breaks = 6) +
  scale_x_continuous("Year", expand = c(0, 0), limits = c(1996, 2017), n.breaks = 12) +
  theme(strip.background = element_blank()) +
  guides(fill = "none", colour = "none")

# Save the graph
figs(counterfactual_female, "count_tfr_f")

### END ##############################################