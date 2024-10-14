#############################################
# Project: Secularization and low fertility
# Purpose: Partnership data                 
# Author: Henrik-Alexander Schubert         
# Date: 16.06.2023                          
# E-Mail: schubert@demogr.mpg.de            
#############################################

rm(list = ls())

# Load the packages
library(tidyverse)
library(data.table)

# Load the functions and packages
source("functions/functions.R")
source("functions/graphics.R")

### Functions --------------------------------------------------

# Combine the data
clean_coh <- function(data) {
   # Variables to be selected
   vars <- c("shnro", "vuosi",  "spuhnro")
   data <- data[, ..vars]
   setnames(data, vars, c("id", "year", "pid"))
   return(data)
}

# Merge religious, basic and coh data
merge_dat <- function (basic = basic1, coh = coh1, religious = rel1) {
   basic <- merge(basic, coh, on = c("id", "year"), all = T) 
   basic <- merge(basic, coh, on.x = c("id", "year"), on.y = c("pid", "year"), all = T, suffixes = c("", "_par"))
   basic <- merge(basic, religious, on = c("id", "year"), all = T )
   basic <- merge(basic, religious, on.x = c("id", "year"), on.y = c("pid", "year"), all = T, suffixes = c("", "_par"))
return(basic)
}

# Combine the data
comb_data <- function(b = basic1, c = coh1, r = rel1) {
   # Merge basic and cohabitatiby by ID
   tmp <- merge(b, c, by = c("id", "year"), all = T) 
   tmp <- merge(tmp, b, by.x = c("pid", "year"), by.y = c("id", "year"), all = T, suffixes = c("", "_par"))
   tmp <- merge(tmp, r, by = c("id", "year"), all = T )
   tmp <- merge(tmp, r, by.x = c("pid", "year"), by.y = c("id", "year"), all = T, suffixes = c("", "_par"))
   tmp <- tmp[age >= 18 & age <= 55 & year <= 2019 & (ori == 11 | ori == 12), ]
   tmp$group <- factor(ifelse(is.na(tmp$pid), 1, ifelse(tmp$religious_par == 1, 2, 3)),
                       labels = c("single", "religous partner", "non_religious partner"))
   final <- tmp[, total := .N, by = .(age, year, sex, religious)]
   
   return(final)
}

### Combine with parental information --------------------------

# Load basic and taxes
load("data/basic.Rda")

# Reduce the basic data
basic <- basic[, .(year, id, age, sex, mar, ori)]

# Load the religoius data
load("data/religious.Rda")

# Path
input <- "D:/ready-made/"

# Load the data for 1987-2000
coh1 <- fread(paste0(input, "FOLK_aslii_11a/folk_20112020_tua_aslii21tot_1.csv"), fill = T)
coh1 <- clean_coh(coh1)

# Load the data for 2001-2010
coh2 <- fread(paste0(input, "FOLK_aslii_0110a/folk_20012010_tua_aslii21tot_1.csv"), fill = T)
coh2 <- clean_coh(coh2)

# Load the data for 2011-
coh3 <- fread(paste0(input, "FOLK_aslii_8800a/folk_19872000_tua_aslii21tot_1.csv"), fill = T)
coh3 <- clean_coh(coh3)

### Prepare the data ----------------------------

# Reduce the basic data
basic1 <- basic[year %in% unique(coh1$year), ]
basic2 <- basic[year %in% unique(coh2$year), ]
basic3 <- basic[year %in% unique(coh3$year), ]

# Split the religious data
rel <- religious[, .(year, id, religious)]
rel1 <- rel[year %in% unique(coh1$year), ]
rel2 <- rel[year %in% unique(coh2$year), ]
rel3 <- rel[year %in% unique(coh3$year), ]

### Combine the data --------------------------

# Create for the first 
tmp <- merge(basic1, coh1, by = c("id", "year"), all = T) 
tmp <- merge(tmp, basic1, by.x = c("pid", "year"), by.y = c("id", "year"), all = T, suffixes = c("", "_par"))
rm(basic1, coh1)
tmp <- merge(tmp, rel1, by = c("id", "year"), all = T )
tmp <- merge(tmp, rel1, by.x = c("pid", "year"), by.y = c("id", "year"), all = T, suffixes = c("", "_par"))
rm(rel1)
tmp <- tmp[age >= 18 & age <= 55 & year <= 2019 & (ori == 11 | ori == 12), ]
tmp$group <- factor(ifelse(is.na(tmp$pid), 1, ifelse(tmp$religious_par == 1, 2, 3)),
                    labels = c("Single", "Religiously affiliated partner", "Religiously unaffiliated partner"))
final1 <- tmp

# Create for the second 
tmp <- merge(basic2, coh2, by = c("id", "year"), all = T) 
tmp <- merge(tmp, basic2, by.x = c("pid", "year"), by.y = c("id", "year"), all = T, suffixes = c("", "_par"))
rm(basic2, coh2)
tmp <- merge(tmp, rel2, by = c("id", "year"), all = T )
tmp <- merge(tmp, rel2, by.x = c("pid", "year"), by.y = c("id", "year"), all = T, suffixes = c("", "_par"))
rm(rel2)
tmp <- tmp[!is.na(id) & age >= 18 & age <= 55 & year <= 2019 & (ori == 11 | ori == 12), ]
tmp$group <- factor(ifelse(is.na(tmp$pid), 1, ifelse(tmp$religious_par == 1, 2, 3)),
                    labels = c("Single", "Religiously affiliated partner", "Religiously unaffiliated partner"))
final2 <- tmp

# Create for the third 
tmp <- merge(basic3, coh3, by = c("id", "year"), all = T) 
tmp <- merge(tmp, basic3, by.x = c("pid", "year"), by.y = c("id", "year"), all = T, suffixes = c("", "_par"))
rm(basic3, coh3)
tmp <- merge(tmp, rel3, by = c("id", "year"), all = T )
tmp <- merge(tmp, rel3, by.x = c("pid", "year"), by.y = c("id", "year"), all = T, suffixes = c("", "_par"))
rm(rel3)
tmp <- tmp[age >= 18 & age <= 55 & year <= 2019 & (ori == 11 | ori == 12), ]
tmp$group <- factor(ifelse(is.na(tmp$pid), 1, ifelse(tmp$religious_par == 1, 2, 3)),
                    labels = c("Single", "Religiously affiliated partner", "Religiously unaffiliated partner"))

# Combine the data sets
d <- rbindlist(list(final1, final2, tmp))
rm(final1, final2, tmp)

# Create a factor for religious
d$religious <- factor(d$religious, labels = c("Religiously unaffiliated", "Religiously affiliated"))

# Save the data
save(d, file = "data/group_rel.Rda")

# Plot the partnership distribution -----------------------------------

# Estimate the share for the total population
d[, total := .N, by = .(year, sex, religious)]
pop_total <- d[, .(proportion = .N / unique(total), total = .N), by = .(year, sex, religious, group)]
pop_total <- pop_total[!is.na(religious), ]
pop_total <- pop_total[!is.na(group), ]


# Plot the result
ggplot(pop_total, aes(x = year, y = proportion, colour = group, group = group, shape = group)) +
   geom_point(size = 2) +
   geom_line() +
   facet_grid(sex  ~ religious) +
   scale_x_continuous("Year", expand = c(0, 0.2), breaks = seq(1995, 2020, by = 5), limits = c(1995, 2019)) +
   scale_y_continuous("Proportion (%)", expand = c(0, 0),limits = c(0, 0.65),n.breaks = 8, labels = scales::percent_format(accuracy = 1L)) +
   scale_colour_manual(name = "", values = colors) +
   scale_shape(name = "")
ggsave(last_plot(), filename = "figures/share_group_rel_par.pdf", height = 20, width = 24, unit = "cm")

# Plot the result
ggplot(pop_total, aes(x = year, y = total, colour = group, group = group, shape = group)) +
   geom_point(size = 2) +
   geom_line() +
   facet_grid(sex  ~ religious) +
   scale_x_continuous("Year", expand = c(0, 0.2), breaks = seq(1995, 2020, by = 5), limits = c(1995, 2019)) +
   scale_y_continuous("Population", expand = c(0, 0), n.breaks = 8, labels = scales::unit_format(unit = "k", scale = 1e-3)) +
   scale_colour_manual(name = "", values = colors) +
   scale_shape(name = "") +
   theme(strip.background = element_blank(),
         strip.text = element_text(size = 14, face = "bold"),
         #axis.text.x = element_text(angle = 45, hjust = 1),
         panel.spacing.y = unit(0.5, "cm"))
ggsave(last_plot(), filename = "figures/count_group_rel_par.pdf", height = 20, width = 24, unit = "cm")

### Standarization -----------------------------------------------------

# Get the weights
standard <- d[year == 2018, ]
standard <- standard[, .(.N), by = .(age, sex)]
standard[, total := sum(N), by = sex]
standard[, share := N / total]

# Plot the standard population
standard %>% 
   mutate(share = ifelse(sex == "Male", -share, share)) %>% 
   ggplot(aes(x = age, y = share, fill = sex)) + 
   geom_col() +
   coord_flip() +
   scale_fill_manual(values = colors) +
   scale_x_continuous("Age", expand = c(0, 0)) +
   scale_y_continuous("Weight (in %)", expand = c(0, 0), labels = scales::percent)
ggsave(last_plot(), filename = "figures/standard_population.pdf")
# Estimate the total count
d[, total := .N, by = .(age, year, sex, religious)]
pop_age <- d[, .(proportion = .N / unique(total)), by = .(age, year, sex, religious, group)]

# Join the standard with the age-specific weights
pop_age <- merge(pop_age, standard, by = c("age", "sex"), allow.cartesian = T)

# Standardize the estimates
pop_age <- pop_age[, .(proportion = sum(share * proportion)), by = .(year, sex, religious, group)]
pop_age <- pop_age[!is.na(religious) & !is.na(group), ]

# Plot the result
ggplot(pop_age, aes(x = year, y = proportion, colour = group, group = group, shape = group)) +
   geom_point(size = 2) +
   geom_line() +
   facet_grid(sex  ~ religious) +
   scale_x_continuous("Year", expand = c(0, 0.2), breaks = seq(1995, 2020, by = 5), limits = c(1995, 2019)) +
   scale_y_continuous("Proportion (%)", expand = c(0, 0),limits = c(0, 0.65), labels = scales::percent) +
   scale_colour_manual(name = "", values = colors) +
   scale_shape(name = "") +
   theme(strip.text = element_text(size = 12, face = "bold"),
         strip.background = element_blank(),
         axis.text.x = element_text(angle = 45, hjust = 1),
         panel.spacing.y = unit(0.5, "cm")) +
   labs(caption = "Note: proportions are age-standardized. Standard population is year 2018.")

ggsave(last_plot(), filename = "figures/standardized_group_rel_par.pdf", height = 20, width = 24, unit = "cm")

### END ###################################################