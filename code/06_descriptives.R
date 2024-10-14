######################################
# Project: Religion and low fertility
# Purpose: Descriptives  
# Name: Henrik-Alexander Schubert    
# Date: 05.07.2023                   
# E-mail: schubert@demogr.mpg.de     
# Pre-requisites: functions          
######################################

rm(list = ls())

# Load the packages
library(tidyverse)
library(data.table)
library(stargazer)

# Load the functions and packages
source("functions/functions.R")
source("functions/graphics.R")
source("functions/models.R")

# Load the data
load("data/analysis.Rda")
load("data/income_quantiles.Rda")

### Merge the data -----------------------------------------------

# Remove empty data
basic <- basic[!is.na(sex), ]

# Make it to tibble
basic <- as_tibble(basic)

# Select the important variables
d <- basic[!is.na(basic$pid), c("id", "year", "un_id", "pid", "start", "end", "union_duration", "right_censored", "left_censored")]
basic <- basic[, c("id", "year", "sex", "age", "birth", "set", "mar", "act", "ori", "edu", "inc", "religious", "children", "firstbirth", "inc_quant", "res")]

# Merge the data with religious data
d <- left_join(basic, d, by = c("id", "year"))
d <- left_join(d, basic,  by = c("year", "pid" = "id"), suffix = c("", "_par"))
rm(basic)

### Clean the data -----------------------------------------------

# Filter the observation period
d <- d[d$year <= 2020 & d$year >= 1995 & d$start > 1986, ]

# Filter the spells before the first childbirth or when a person does not get children
d <- d %>% group_by(id) %>% fill(firstbirth, .direction = "updown")
d <- d[d$year <= d$firstbirth | is.na(d$firstbirth), ]

# Keep only when the women is the main person
d <- d[d$sex == "Female", ]

# Select the sample
d <- d[(d$ori == 12 | d$ori == 11) & (d$ori_par == 12 | d$ori_par == 11), ]

# Estimate the age gap
d$agegap <- d$age_par - d$age

# Create the outcome variable
d$birth <- ifelse(d$firstbirth == d$year, 1, 0)
d$birth[is.na(d$birth)] <- 0
d$event <- ifelse(!is.na(d$firstbirth), "birth", "censored")

# Create period factor
d$period <- cut(d$year, breaks = seq(1995, 2020, by = 5), include.lowest = T, right = F)

### Filter missings ----------------------------------------------

# Make to factor
d$edu <- factor(d$edu, ordered = F)
d$edu_par <- factor(d$edu_par, ordered = F)
d$set <- factor(d$set, ordered = F)

# Make it to data.table
d <- as.data.table(d)

# Filter the variables
d <- d[, .(id, pid, event, period, un_id, year, birth, age, union_duration, 
           agegap, inc_quant, inc_quant_par, religious, religious_par,
           inc, inc_par, act, act_par, edu, edu_par, set, firstbirth, res)]

# Create the last spell
d[, last := max(year), by = un_id]

# Sort the data
d <- d[order(id, year), ]

# Create age at onset
d[, age_onset := age - union_duration]

# Lag the variables
d[, age := lag(age), by = .(id, un_id)]
d[, religious := lag(religious), by = .(id, un_id)]
d[, religious_par := lag(religious_par), by = .(id, un_id)]
d[, inc := lag(inc), by = .(id, un_id)]
d[, inc_par := lag(inc_par), by = .(id, un_id)]
d[, act := lag(act), by = .(id, un_id)]
d[, act_par := lag(act_par), by = .(id, un_id)]
d[, edu := lag(edu), by = .(id, un_id)]
d[, edu_par := lag(edu_par), by = .(id, un_id)]
d[, set := lag(set), by = .(id, un_id)]
d[, union_duration := lag(union_duration), by = .(id, un_id)]
d[, period := lag(period), by = .(id, un_id)]
d <- d[!is.na(religious), ]

# Get the sample sizes
sample_sizes <- sapply(d, function(x) sum(is.na(x)), simplify = T)
sample_sizes$total <- nrow(d)

# Filter the missing values
d <- d[!is.na(d$edu) & !is.na(d$edu_par) & !is.na(d$inc) & !is.na(d$inc_par) &
         !is.na(d$age) & !is.na(d$religious) & !is.na(d$religious_par) &
         !is.na(d$agegap) & !is.na(d$act) & !is.na(d$act_par) & !is.na(set) &
         !is.na(d$inc_quant) & !is.na(inc_quant_par), ]

# Get the cleaned sample sizes
sample_sizes$cleaned <- nrow(d)
save(sample_sizes, file = "data/sample_sizes.Rda")

# Take the logarithm for the income variables
d[, inc := ifelse(inc == 0, 0, log(inc))]
d[, inc_par := ifelse(inc_par == 0, 0, log(inc_par))]

# Keep the last and first value of religious affiliation constant
d <- d[order(id, un_id, year), ]
d[, rel := first(religious), by = un_id]
d[, rel_par := first(religious_par), by = un_id]

# Collect the information
save(d, file = "data/final_data.Rda")

### Descriptive statistics --------------------------------------

# Create a summary statistic of the data
stargazer(subset(d, select = c(un_id, year, birth, age, agegap, religious, religious_par, inc, inc_par, edu, edu_par, act, act_par, res)), summary = T, digits = 3, out = "results/summary_table.tex")

### Descriptive ------------------------------------------------

# Education distribution
couple_labels <- c("Both religiously unaffiliated", "Woman religiously affiliated",
                   "Man religiously affiliated", "Both religiously affiliated")
d$couple <- NA
d$couple[d$religious == 0 & d$religious_par == 0] <- couple_labels[1]
d$couple[d$religious == 1 & d$religious_par == 0] <- couple_labels[2]
d$couple[d$religious == 0 & d$religious_par == 1] <- couple_labels[3]
d$couple[d$religious == 1 & d$religious_par == 1] <- couple_labels[4]
d$couple <- factor(d$couple, levels = couple_labels)
edu_distr <- round(prop.table(table(d$edu, d$couple), margin = 1) * 100, 2)
reshape2::melt(edu_distr) %>%
  as_tibble() %>% 
  ggplot(aes(x = Var1, y = value, fill = Var2)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_x_discrete("Women's education") +
  scale_y_continuous("Share (in %)", expand = c(0, 0), limits = c(0, 100)) +
  scale_fill_manual(name = "Couple:", values = colors) +
  guides(fill = guide_legend(nrow = 2))
ggsave(last_plot(), filename = "figures/edu_couple_comp.pdf", width = 20, height = 15, unit = "cm")

# Plot the couple distribution
couple_distr <- d %>%
  group_by(year) %>%
  filter(year > 1995) %>% 
  mutate(total = n()) %>%
  group_by(year, couple) %>% 
  summarise(share = n() / unique(total), .groups = "drop") %>% 
  ggplot(aes(x = year, y = share, fill = ordered(couple, c("Both religiously unaffiliated", "Woman religiously affiliated", "Man religiously affiliated", "Both religiously affiliated")))) +
  geom_col() +
  scale_fill_manual(values = colors, name = "Couple composition:") +
  scale_x_continuous("Year", expand = c(0, 0), n.breaks = 13) +
  scale_y_continuous("% share of couples", expand = c(0, 0), n.breaks = 10, labels = scales::percent_format(accuracy = 1L)) +
  guides(fill = guide_legend(nrow = 2))
figs(couple_distr, "dyadic_couple_trend")

# Share of relationships ending in birth
d %>% 
  filter(last == year) %>% 
  as_tibble() %>% 
  group_by(couple) %>% 
  mutate(count = n()) %>% 
  group_by(birth, couple) %>% 
  summarise(share = n() / unique(count), total = unique(count), .groups = "drop") %>% 
  filter(birth == 1) %>% 
  ggplot(aes(x = couple, y = share, group = couple, fill = couple)) +
  geom_col(position = "dodge") +
  geom_text(aes(y = 0.2, label = paste0(100*round(share, 3), "%")), colour = "white", family = "serif", position = position_dodge(width = 1), size = 10) +
  scale_fill_manual(values = colors, name = "Religiosity male partner:") +
  scale_y_continuous("Relationships ending in childbirth (%)", label = scales::percent_format(accuracy = 1L),
                     expand = c(0, 0), n.breaks = 10, limits = c(0, 0.51)) +
  scale_x_discrete("", expand = c(0, 0)) +
  guides(fill = "none") +
  theme(
    plot.margin = ggplot2::margin(0.2, 0.2, -0.2, 0.2, "cm")
  )
figs(last_plot(), "share_births_dyadic")

### Look at the number of transitions ------------------

# Create a data.table
d <- data.table(d)

# Average number of religious states per couple
spells <- d[, .N, by = .(couple, un_id)]
spells <- spells[, .N, by = .(un_id)]
summary(spells$N)

# Transitions
d <- d[order(un_id, year), ]
spells <- d[, .(couple, lag = shift(couple, type = "lag", n = 1)), by = .(un_id)]
spells$transition <- ifelse(spells$couple != spells$lag & !is.na(spells$lag), 1, 0)
mean(spells$transition, na.rm = T)
transitions <- prop.table(table(from = spells$lag, to = spells$couple), margin = 1)
write.csv(transitions, file = "results/transitions.csv")
plot_transitions <- reshape2::melt(transitions) %>% 
  as_tibble() %>% 
  ggplot(aes(x = from, y = to, fill = value)) +
  geom_tile() +
  scale_fill_steps(low = "white", high = "red", name = "Transition:", breaks = seq(0, 1, 0.1)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(legend.key.width = unit(2, "cm"),
        axis.text.x = element_text(angle = 5, vjust = 0.01))
figs(plot_transitions, "transitions")

### END #################################################3