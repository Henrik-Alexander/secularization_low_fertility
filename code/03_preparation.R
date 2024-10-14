#############################################
# Project: Secularization and low fertility
# Purpose: Data preparation                 
# Author: Henrik-Alexander Schubert         
# Date: 16.06.2023                          
# E-Mail: schubert@demogr.mpg.de            
#############################################

rm(list = ls())

# Load the packages
library(data.table)
library(tidyverse)
library(lubridate)
library(mgcv)

# Load the functions
source("functions/functions.R")
source("functions/graphics.R")

# Year range
years <- 1987:2021

### Functions --------------------------------------

# The spline regression
spline_regress <- function(df) {
  gam(event ~ s(age), data = df, family = "poisson", offset = log(exp))
}

# Predict the result
predict_spline <- function(model) {
  pred <- data.frame(age = 18:40)
  prediction <- predict(model, newdata = pred, type = "response", se = T)
  pred <- bind_cols(pred, prediction)
  return(pred)
}


### 1. Create the birth dates -----------------------

# Create the oldnames in the parent data
oldnames <- c("shnro", "shnro_m", "shnro_f", "syntyv")

# Load the parent-child data
parent_child <- fread("D:/ready-made/FOLK_laps_70a/folk_19702020_tua_laps21_1.csv")
parent_child <- parent_child[, ..oldnames]

# Rename
setnames(parent_child, old = oldnames, new = c("cid", "mother", "father", "yob_child"))

# Recode the data
parent_child <- melt(parent_child, id.vars = c("cid", "yob_child"), measure.vars = c("mother", "father"),variable.name = "parent", value.name = "id")
parent_child$parent <- str_remove(parent_child$parent, "id_")

# Sort the data
parent_child <- parent_child[order(id, yob_child), ]

# Number the children
parent_child <- parent_child[, nch := seq_len(.N), by = id]

# Parent
parent_child <- parent_child[nch < 12, ]

# Create a birth variable
parent_child[, birth := 1]

# Rename year variable
parent_child$year <- parent_child$yob_child

# Create a first birth variable
parent_child <- parent_child[, firstbirth := min(yob_child), by = id]

### Combine with basic data --------------------------------------

# Load the basic and religious data
load("data/basic.Rda")

# Reduce to important variables
basic[, nch := NULL]

# Combine the data
basic <- merge(basic, parent_child, by = c("id", "year"), all.x = TRUE)
rm(parent_child)

# Clean the birth information
basic$birth <- ifelse(is.na(basic$birth), 0, 1)

# Cumulative number of children
basic[, children := cumsum(birth), by = id]

# Remove variables
basic <- basic[, .(id, year, sex, age, inc_quant, mar, act, edu, set, ori, inc, birth, children, firstbirth, inc_quant, res)]

### Combine with religious ----------------------------------------

load("data/religious.Rda")

# Select the respondents
religious <- religious[, .(id, year, religious)]

# Join the religious data with basic data
basic <- basic[religious, on = .(id, year)]
rm(religious)

# Filter when the year is larger than 1990 and Finns (born in Finland and abroad)
basic <- basic[(ori == 11 | ori == 12) & age >= 12 & age <= 55, ]

# Relationship data --------------------------------------

# Set the input path
input <- "D:/ready-made/"

# Load the data for 1987-2000
coh1 <- fread(paste0(input, "FOLK_aslii_11a/folk_20112020_tua_aslii21tot_1.csv"), fill = T)

# Load the data for 2001-2010
coh2 <- fread(paste0(input, "FOLK_aslii_0110a/folk_20012010_tua_aslii21tot_1.csv"), fill = T)

# Load the data for 2011-
coh3 <- fread(paste0(input, "FOLK_aslii_8800a/folk_19872000_tua_aslii21tot_1.csv"), fill = T)

### Clean the data ----------------------------

# Combine the data
d <- rbindlist(list(coh1, coh2, coh3))
rm(coh1, coh2, coh3)

# Reduce the data
# Variables to be selected
vars <- c("shnro", "vuosi", "spuhnro", "alku", "loppu", "pu_kuolpv", "pu_pnro", "pu_jarnro")

# Filter the important variables
d <- d[, ..vars]

# Rename the variables
d <- d[, .(id = shnro,
           year = vuosi,
           pid = spuhnro,
           start = alku,
           end = loppu,
           coh_nr  = pu_pnro,
           mar_nr  = pu_jarnro,
           year    = vuosi)]

# Indicator for right-censoring of relationship
d$right_censored <- ifelse(d$start == "2020-10-31", 1, 0)
d$left_censored <- ifelse(d$end == "1986-12-31", 1, 0)

# Transform the to year
vars <- c("start", "end")
d[, (vars) := lapply(.SD, year), .SDcols = (vars)]

# Estimate the length
d$length <- NA
d$length[(d$right_censored==0&d$left_censored==0)] <- d$end - d$start

# Filter unions that have a positive length
d <- d[length >= 0, ]

# Create union ID
d[, un_id :=.GRP, by = .(id, pid)]

# Select the variables
d <- d[, .(id, pid, start, end, un_id, right_censored, left_censored, length)]

# Remove duplicates
d <- unique(d)

# Numerate the unions
d <- d[order(id, start), ]
d[, nr := seq_len(.N), by = id]

# Save the data
save(d, file = "data/relationships.Rda")

### Prepare the relationship data --------------------------------------------

# Load basic and taxes
load("data/relationships.Rda")

# Create long data
d <- uncount(d, length, .id = "union_duration")
d[, year := start + union_duration - 1]

# Merge the data
basic <- merge(basic, d,  by = c("id", "year"), all.x = T)
rm(d)

# Filter spells in the analysis period
basic <- basic[year >= 1995 & year <= 2020, ]
basic <- basic[, inc_quant.1 := NULL]

# Save the analysis data
save(basic, file = "data/analysis.Rda")

### Study the effect on timing of union -------------------------------

# Load the data
load("data/analysis.Rda")

# Estimate the birth year
basic[, yob := year - age]

# Filter young respondents
basic <- basic[yob >= 1978 & age >= 18, ]

# Single
basic$union <- ifelse(is.na(basic$nr), 0, 1)

# First relationship
basic[, first_union := min(start, na.rm = T), by = id]

# Select if it is before or on the first union
basic <- basic[year <= first_union | is.na(first_union), ]

# Create event variable
basic$event <- ifelse(basic$first_union == basic$year, 1, 0)

# Aggregate the data
expos <- basic[, .(exp = .N), by = .(age, religious, sex)]
event <- basic[, .(event = sum(event, na.rm = T)), by = .(age, religious, sex)]

# Combine the data
d <- expos[event, on = .(age, religious, sex)]

# Split the data
d <- split(d, by = c("religious", "sex"))

# Estimate the model
models <- map(d, spline_regress)

# Predict the results
predict <- map(models, predict_spline)

# Predict models
predict <- bind_rows(predict, .id = "id")

# Create column names
predict <- predict %>% 
  mutate(religious = str_sub(id, 1, 1),
         sex       = str_extract(id, "[A-Z][a-z]*"))

# Plot the result
partnership_rate <- ggplot(subset(predict, religious != "N"), aes(x = age, y = fit, colour = religious, group = religious, shape = religious)) +
  geom_point() +
  geom_line() +
  facet_wrap( ~  sex) +
  geom_ribbon(aes(ymin = fit - 1.96*se.fit, ymax = fit + 1.96*se.fit, fill = religious), alpha = .3) +
  scale_x_continuous("Age", expand = c(0, 0), breaks = seq(18, 40, 2)) +
  scale_y_continuous("First parnership hazard rate", limits = c(0, 0.2), expand = c(0, 0)) +
  scale_fill_manual(values = colors[c(1, 4)], name = "", labels = c("Religiously unaffiliated", "Religiously affiliated")) +
  scale_colour_manual(values = colors[c(1, 4)], name = "", labels = c("Religiously unaffiliated", "Religiously affiliated")) +
  scale_shape(name = "", labels = c("Religiously unaffiliated", "Religiously affiliated")) +
  theme(panel.spacing = unit(0.8, "cm"))

# Save the prediction plot
figs(partnership_rate, "first_partnership_rate", height = 15, width = 20)

# Estimate the first partnership rate
first_birth_rate <- aggregate(fit  ~ id, data = predict, FUN = sum)

# Estimate the mean age of first childbirth
predict %>% 
  group_by(id) %>% 
  summarize(mac = sum(age * fit) / sum(fit))

### END ###############################################################