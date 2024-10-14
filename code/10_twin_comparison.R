### 10. Twins #######################
# Purpose: Twin comparison          #
# Author: Henrik-Alexander Schubert #
# Date: 16.06.2023                  #
# E-Mail: schubert@demogr.mpg.de    #
#####################################


options(scipen = 999)

rm(list = ls())

# Load the packages
library(tidyverse)
library(data.table)
library(fixest)
library(mgcv)

# Load the main funcitons
source("functions/functions.R")
source("functions/graphics.R")
source("functions/models.R")

# Functions -----------------------------

# Test for dummy variable
is_dummy <- function(x) {
  tmp <- unique(x[!is.na(x)])
  return(all(tmp == c(0, 1)) | all(tmp == c(1, 0)))
}


# Demean the variable
demean <- function(x) {
  if (!is.numeric(x)) {
    return(x)
  } else if (is_dummy(x)) {# Dummy variables do not have to be transforme
    return(x)
  } else {
    return(x - mean(x))
  }
}


# Prepare twin data
prepare_twin <- function(data, female = T) {
  tmp <- data[twins == 1, ]
  tmp[, twin_spells := .N, by = twin_id]
  # Create the spells
  tmp[, spells := 1:.N, by = un_id]
  # Select the variables
  tmp <- tmp[union_duration <= 5, ]
  tmp <- tmp[, .(id = unique(id), birth = max(birth), age_onset = unique(age_onset),
                 rel = first(rel), rel_par = first(rel_par), agegap = unique(agegap), 
                 edu = first(edu), edu_par = first(edu_par),
                 union_duration = max(union_duration),
                 inc_quant = first(inc_quant), inc_quant_par = first(inc_quant_par),
                 act = first(act), act_par = first(act_par),
                 twin_id = unique(twin_id), age_onset2 = unique(age_onset)^2), by = un_id]
  
  return(tmp)
}


# Function to filter discordant twins
filter_discordant <- function(data, female = T) {
  if(!is.data.table(data)) stop("Must be data.table!")
  # Total count of twin spells
  data[, total := .N, by = twin_id]
  if (female) {
    data[, disc := .N, by = .(twin_id, rel)]
  } else {
    data[, disc := .N, by = .(twin_id, rel_par)]
  }
  data <- data[total > disc]
  data[, disc := NULL]
  data[, total := NULL]
  return(data)
}

# 1. Identify the twins -----------------

# Load the parent-child data
parent_child <- fread("D:/ready-made/FOLK_laps_70a/folk_19702020_tua_laps21_1.csv")

# Subset and rename the variables
oldnames <- c("shnro", "shnro_m", "shnro_f", "syntyv")
parent_child <- parent_child[, ..oldnames]
setnames(parent_child, old = oldnames, new = c("cid", "mother", "father", "yob_child"))

# Twins
parent_child[, twins := ifelse(.N > 1, 1, 0), by = .(mother, father, yob_child)]
twins <- parent_child[twins == 1 & mother != "" & father != "" & yob_child > 1950, ]
twins[, twin_id := paste(.GRP), by = .(mother, father, yob_child)]

# Save the twin data 
save(twins, file = "data/twin_ids.Rda")
rm(parent_child)

# 2. Compare the twin partnering --------

# 3. Twin comparison of first birth -----

# Load the main data
load("data/final_data.Rda")

# Remove the unimportant infos from the parent data
twins <- twins[, .(cid, twins, twin_id)]
d_twin <- merge(d, twins, by.x = "id", by.y = "cid", all.x = T)

# Clean the twin data
twin_f <- prepare_twin(d_twin)

# Filter the discordant pairs
twin_f <- filter_discordant(twin_f)

# Descriptives
twin_f[, .(birth = sum(birth), cases = .N), by = rel_par]

# Descriptive finding
prop.table(table(religion = twin_f$rel,
                 birth = twin_f$birth),
           margin = 1)

# Estimate the model
twin_fes <- feols(birth ~ age_onset + age_onset2 + agegap + edu + edu_par + inc_quant + inc_quant_par + act + act_par + mvsw(rel, rel_par) | twin_id, data = twin_f)
twin_f_int <- feols(birth ~  age_onset + age_onset2 + agegap + edu + edu_par + inc_quant + inc_quant_par + act + act_par + rel + rel_par + rel:rel_par | twin_id, data = twin_f)

coefplot(twin_fes)$prms %>% 
  mutate(estimate_names = clean_labels(estimate_names)) %>% 
  ggplot(aes(x = estimate_names, y = y, colour = factor(id))) +
  geom_hline(yintercept = 0) +
  #geom_text(aes(label = round(y, 2)), position = position_dodge(width = 1.4)) +
  geom_point(position = position_dodge(width = 1)) +
  geom_linerange(aes(ymin = ci_low, ymax = ci_high), position = position_dodge(width = 1)) +
  scale_colour_manual(values = colors, name = "Model:") +
  coord_flip()

etable(twin_fes, twin_f_int,
       se.below = T, tex = T,
       keep = "rel",
       dict = c(rel = "Female religiously affiliated",
                rel_par = "Male religiously affiliated",
                "rel:rel_par" = "Both religiously affiliated"),
       label = "fig: female_twin_fe",
       file = "results/twin_female_fe.tex",
       replace = TRUE,
       title = "Female twin FE comparison for the effect of religious affiliation on fertility.")

# Twin comparison for male twins --------

# Remove the unimportant infos from the parent data
twins <- twins[, .(cid, twins, twin_id)]

# Merge the two datasets, and select female twins
d_twin <- merge(d, twins, by.x = "pid", by.y = "cid", all.x = T)
twin_m <- prepare_twin(d_twin, female = F)

# Filter the discordant pairs
twin_m <- filter_discordant(twin_m, female = F)

# Descriptives
twin_m[, .(birth = sum(birth), cases = .N), by = rel_par]

# Proportionate table
prop.table(table(religion = twin_m$rel_par,
                 birth = twin_m$birth),
           margin = 1)
  
# Estimate the model
twin_fes <- feols(birth ~ age_onset + age_onset2 + agegap + edu + edu_par + inc_quant + inc_quant_par + act + act_par + mvsw(rel, rel_par) | twin_id, data = twin_m)
twin_inter <- feols(birth ~ age_onset + age_onset2 + agegap + edu + edu_par + inc_quant + inc_quant_par + act + act_par + rel + rel_par + rel:rel_par | twin_id, data = twin_m)

# Make the coefficient plot
coefplot(twin_fes)$prms %>% 
  mutate(estimate_names = clean_labels(estimate_names)) %>% 
  filter(!str_detect(estimate_names, "age")) %>% 
  ggplot(aes(x = estimate_names, y = y, colour = factor(id))) +
  geom_hline(yintercept = 0) +
  #geom_text(aes(label = round(y, 2)), position = position_dodge(width = 1.4)) +
  geom_point(position = position_dodge(width = 1)) +
  geom_linerange(aes(ymin = ci_low, ymax = ci_high), position = position_dodge(width = 1)) +
  scale_colour_viridis_d(option = "D", name = "Model:") +
  scale_y_continuous("Effect size") +
  coord_flip() +
  theme(
    axis.title.y = element_blank()
  )
ggsave(last_plot(), filename = "figures/twin_male_effect.pdf", height = 15, width = 15, unit = "cm")

# Create the regression table
etable(twin_fes, twin_inter, 
       se.below = T, tex = T,
       dict = c(rel = "Female religiously affiliated",
                rel_par = "Male religiously affiliated", 
                "rel:rel_par" = "Both religiously affiliated"),
       keep = "rel",
       replace = TRUE,
       label = "fig: male_twin_fe",
       file = "results/twin_male_fe.tex",
       title = "Male twin FE comparison for the effect of religious affiliation on fertility.")

### END #################################