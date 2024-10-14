### 10. Spatial #####################
# Purpose: Regional exposures       #
# Author: Henrik-Alexander Schubert #
# Date: 16.06.2023                  #
# E-Mail: schubert@demogr.mpg.de    #
#####################################

rm(list = ls())

# Load the packages
library(data.table)
library(tidyverse)
library(sf)
library(broom)
library(mgcv)
library(lmtest)
library(fixest)
library(stargazer)
library(mgcv)

# Load the functions
source("functions/functions.R")
source("functions/graphics.R")
source("functions/models.R")

# Load the basic and religious data
load("data/basic.Rda")
load("data/religious.Rda")
load("data/births_parents.Rda")
load("W:/Henriks_projects/3_subnational_birth_squeezes/data/regions_maakunta.Rda")

# Select the features
religious <- religious[, .(id, year, religious)]
basic <- basic[, .(id, year, sex, age, set, ori, res)]
basic <- basic[religious, on = .(id, year)]

### Functions --------------------------------------------

# Detrending for the fixed effects individual slope models
detrend <- function(data, variables = "tfr") {
  y <- data[[variables]]
  x <- data[["year"]]
  x2 <- x^2
  mod <- lm(y  ~ x + x2)
  data[[paste(variables, "detrend", sep = "_")]] <- y-predict(mod, data.frame(x, x2))
  return(data)
}

# Estimate confidence intervals
conf_int <- function(coef, se, alpha = 0.05) {
  right <- function()  qnorm(1-alpha/2)*se
  list("lower" = coef - right(), "higher" = coef + right())
}

### Load the regional files ------------------------------

# Load and clean the regional data
reg <- read.csv("D:/metadata/classifications/region/alueet22.csv", encoding = "latin1")
reg <- dplyr::rename(reg, res = kunta, res_name = Maakunta, reg = Seutukunta) 
reg <- reg[, c("res", "res_name", "reg")]

# Merge with the regional data
births <- births[reg, on = c("res_mother" = "res")]
basic <- basic[reg, on = "res"]

# Remove 
basic[, i.religious := NULL]

### Estimate the panel model -----------------------------

# Set the minimum and maximum age
min_age <- 15
max_age <- 49

# Change the birth age
births$age_mother[births$age_mother <= min_age] <- min_age
births$age_mother[births$age_mother >= max_age] <- max_age

# Estimate the regional tfr
births_f <- births[, .(births = .N), by = .(yob_child, reg, age_mother)]
basic <- basic[age %in% min_age:max_age & sex == "Female", ]
expos_f <- basic[, .(pop = .N), by = .(reg, age, year)]
expos_f <- expos_f[order(reg, age, year), exposure := (pop + shift(pop, type = "lag"))/2, by = .(reg, age)]
asfr_f <- expos_f[births_f, on = c( "reg", "age" = "age_mother", "year" = "yob_child")]

# Create age groups
asfr_f <- create_age_groups(asfr_f)
asfr_f <- asfr_f[, .(exposure = sum(exposure, na.rm = T), births = sum(births, na.rm = T)), by = .(reg, year, age_group)]

### Smoothing the tfr data -------------------------------------

# Smooth the rates
smoothing_regions <- c("Ålands skärgård", "Joutsan", "Torniolaakson")
smoothing_data <- asfr_f[asfr_f$reg %in% smoothing_regions, ]
smoothing_data <- split(smoothing_data, smoothing_data$reg)
smoothing_data <- map_df(smoothing_data, smooth_tfr)
smoothing_data$asfr_smooth <- smoothing_data$births / smoothing_data$exposure
  

# Estimate the TFR -------------------------------------------------------

tfr_f <- asfr_f[, .(tfr = sum(5 * births / exposure, na.rm = T)), by = .(year, reg)]
tfr_f <- tfr_f[!is.na(tfr) & !is.na(year) & year %in% 1995:2018, ]
tfr_f <- tfr_f[!(reg %in% smoothing_regions)]

### Estimate the panel-all results ---------------------------------------

# Filter when the year is larger than 1990 and Finns (born in Finland and abroad)
basic <- basic[(ori == 11 | ori == 12), ]
basic <- basic[order(id, year), ]
basic[, religious := impute_religious(religious), by = id]
share_res_name <- basic[, .(share = mean(religious, na.rm = T)), by = .(year, reg)]

# Combine with the regional shares_name of religiosity
panel <- share_res_name[tfr_f, on = c("year", "reg")]
panel <- panel[order(reg, year), ]
panel <- panel[!is.na(reg) & !is.na(share)]
panel <- panel[year <= 2018, ]
change_plot <- panel[year %in% c(2000, 2009, 2018) & !is.na(reg), .(dtfr = tfr - shift(tfr, type = "lag"),
                                                     dshare = share - shift(share, type = "lag"),
                                                     label = paste0(shift(year, type = "lag"), "-", year)), by = reg]

# Plot the res_nameult
plot_period_change <- ggplot(subset(change_plot, label %in% c("2000-2009", "2009-2018")), aes(x = dshare, y = dtfr, colour = label, shape = label)) +
  geom_point() +
  geom_smooth(se = F, method = "lm", formula = "y ~ x") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  scale_x_continuous("Change in regional share of religiosity", expand = c(0, 0)) +
  scale_y_continuous("Change in TFR", n.breaks = 10, expand = c(0.01, 0)) +
  scale_colour_manual(name = "Period of change:", values = colors) +
  scale_shape(name = "Period of change:") +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 14, face = "bold")
  )
figs(plot_period_change, "reg_corr_period")

# Regres_names the change
lm(dtfr  ~ dshare, data = change_plot[label == "2009-2018"])

# Plot the trend
trend_tfr <- ggplot(panel, aes(x = year, y = tfr)) +
  geom_line(aes(colour = share * 100, group = reg)) +
  geom_smooth(method = "loess", se = F, formula = "y ~ x", colour = "black") +
  scale_colour_gradient(low = colors[1], high = colors[2], name = "Share religious (%):") +
  scale_x_continuous("Year", breaks = seq(1995, 2015, by = 5), expand = c(0, 0)) +
  scale_y_continuous("Total fertility rate") +
  theme(legend.key.width = unit(2, "cm"))
figs(trend_tfr, "trend_regional_tfr")

## Estimate the models
panel <- panel[panel$year >= 2000, ]
panel$share <- panel$share * 100

# Create the detrended data
panel_models <- split(panel, panel$reg)
panel_models <- map(panel_models, detrend)
panel_models <- map(panel_models, detrend, variables = "share")
panel <- rbindlist(panel_models, idcol = "reg")

# Estimate the first differences
panel[, share_fd := share - lag(share), by = reg]
panel[, tfr_fd := tfr - lag(tfr), by = reg]

# Estimate the models
mod1 <- lm(tfr ~ share, data = panel)
mod2 <- lm(tfr  ~ share + factor(year), data = panel)
mod3 <- lm(tfr_fd ~ -1 + share_fd, data = panel[!is.na(share_detrend), ])
mod4 <- feols(tfr ~ share | reg + year, data = panel)

# Estimate the clustered standard errors for model 1 and 2
mod1_cse <- coeftest(mod1, vcov = vcov(mod1), cluster =  ~ reg + year)
mod2_cse <- coeftest(mod2, vcov = vcov(mod2), cluster =  ~ reg)

# What is the correlation
cat("The correlation between share religious and tfr is", with(panel, cor(share, tfr)))

# Model the data
models <- list("1. OLS" = mod1, "2. OLS + year" = mod2, "3. Region FE" = mod3, "4. Twoway FE" = mod4)
coefficents <- lapply(models, tidy)
coefficents <- bind_rows(coefficents, .id = "model")
coefficents$term[coefficents$term == "share_detrend"] <- "share"
coefficents$term[coefficents$term == "share_fd"] <- "share"

# Add the clusterd confidence intervals
coefficents[coefficents$model == "1. OLS", c("conf.low", "conf.high")] <- coefci(mod1, vcov = vcov(mod1), cluster =  ~ reg + year)
coefficents[coefficents$model == "2. OLS + year", c("conf.low", "conf.high")] <- coefci(mod2, vcov = vcov(mod2), cluster =  ~ reg + year)
coefficents[coefficents$model == "3. Region FE", c("conf.low", "conf.high")] <- conf_int(coef(mod3), se(mod3))
coefficents[coefficents$model == "4. Twoway FE", c("conf.low", "conf.high")] <- conf_int(coef(mod4), se(mod4))
#coefficents[coefficents$model == "5. FEIS", c("conf.low", "conf.high")] <- conf_int(coef(mod5), se(mod5))

# Plot the coefficients
plot_panel_coeff <- ggplot(data = subset(coefficents, term == "share"), aes(x = model, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point(size = 3) +
  geom_text(data = subset(coefficents, str_detect(model, "OLS") & term == "share"), aes(label = "*"), size = 8, nudge_x = 0.1) +
  geom_hline(yintercept = 0) +
  geom_linerange(size = 1.4) +
  scale_y_continuous("Estimate for % religious in region") +
  scale_x_discrete("Regression model") +
  coord_flip() +
  labs(caption = "* Confidence intervals are based on clustered standard errors at the regional level.")
figs(plot_panel_coeff, "panel_reg_all")

# Create the regres_namesion table
stargazer(mod1, mod3, mod4,
          style = "asr", omit = "year", type = "text",
          out = "results/panel_reg_tfr_all.tex")

## Regional-religious tfr ---------------------------------

# Combine the religious, basic and births data
births <- merge(births, basic,
                by.x = c("yob_child", "value_mother"),
                by.y = c("year", "id"),
                suffixes = c("", "_mother"), 
                all.x = T)
births <- births[(ori %in% c(11, 12)), ]

# res_nameet the age
births$age[births$age <= min_age] <- min_age
births$age[births$age >= max_age] <- max_age

# Aggregate now
births_f <- births[, .(births = .N, sex = "Female"), by = .(reg, age_mother, religious, yob_child)]
setnames(births_f, c("age_mother", "religious"), c("age", "religious_mother"))
births_f <- births_f[, .(births = sum(births, na.rm = T)), by = .(age, reg, religious_mother, yob_child)]
basic <- basic[(ori == 11 | ori == 12 & sex == "Female"), ]
expos <- basic[, .(expos = .N), by = .(reg, sex, age, religious, year)]
share_religious <- expos[, .(pop = sum(expos)), by = .(reg, religious, year)]
share_religious[, total := sum(pop), by = .(reg, year)]
share_religious[order(reg, year), share := pop / total]

# Combine the data
expos_f <- expos[sex == "Female", ]
rates_r <- expos_f[births_f, on = c("age" = "age", "reg" = "reg", "religious" = "religious_mother", "year" = "yob_child")]
rates_r <- create_age_groups(rates_r)
rates_r <- rates_r[, .(asfr = sum(births) / sum(expos), expos), by = .(reg, year, religious, age_group)]


# # Estimate the tfrs
tfr_r <- rates_r[, .(tfr = sum(asfr)), by = .(reg, religious, year)]

# Smooth the rates using moving averages
tfr_r <- tfr_r[order(reg, religious, year), .(tfr = zoo::rollmean(tfr, k = 3, fill = -2, na.pad = T), year), by = .(reg, religious)]
tfr_r <- tfr_r[tfr > 0, ]

# Plot the trend
ggplot(subset(tfr_r, !is.na(religious)), 
              aes(x = year, y = tfr, group = interaction(reg, religious), colour = factor(religious))) +
  geom_line() +
  scale_colour_manual(values = colors, name = "Religious affiliation") +
  scale_y_continuous("TFR", limits = c(0, 4)) + 
  facet_wrap( ~religious)

# Estimate the trend
tfr_r <- tfr_r[order(religious, reg, year), ]
share_religious <- share_religious[order(reg, year)]
change_tfr <- tfr_r[year %in% c(2000, 2009, 2019), .(tfr_change = tfr - shift(tfr, type = "lag"), year), by = .(reg, religious)]
change_share <- share_religious[year %in% c(2000, 2009, 2019) & religious == 1, .(change_share = share - shift(share, type = "lag"), year), by = .(reg)]
change_share[change_tfr, on = c("year", "reg")] %>%
  filter(year >= 2009) %>% 
  ggplot(aes(x = change_share * 100, y = tfr_change, colour = factor(religious))) +
  geom_point() + 
  geom_smooth(method = "lm", formula = "y  ~ x", se = F) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  scale_colour_manual(name = "Religious affiliation:", labels = c("unaffiliated", "affiliated"), values = colors) +
  facet_wrap( ~ year)

# Demean the data for the fixed effects model
fe_data <- tfr_r[share_religious[religious == 1], on = c("reg", "year")]
fe_data <- fe_data[religious == 1 & i.religious == 1, ]
fe_data <- fe_data[order(reg, year), ]
fe_data[, tfr_i := tfr - lag(tfr), by = reg]
fe_data[, share_i := share - lag(share), by = reg]

# Lag the variables
fe_data[, share_i_lag1 := lag(share_i), by = reg]
fe_data[, share_i_lag2 := lag(share_i, n = 2), by = reg]
fe_data[, share_lag1 := lag(share), by = reg]
fe_data[, share_lag2 := lag(share, n = 2), by = reg]

# Estimate the models
vars <- names(fe_data)[str_detect(names(fe_data), "share")]
fe_data[, (vars) := lapply(.SD, function(x) x * 100), .SDcols = (vars)]
mod1 <- lm(tfr ~ share_lag1, data = fe_data)
mod2 <- lm(tfr  ~ share_lag1 + factor(year) , data = fe_data)
mod3 <- lm(tfr_i ~ -1 + share_i_lag1, data = fe_data)
mod4 <- lm(tfr_i ~ -1 + share_i_lag1 + factor(year), data = fe_data)

# Print the res_nameult
stargazer(mod1, mod3, mod4,
          ci = T,
          omit = "year", out = "results/panel_regional_tfr_rel.tex", 
          style = "asr", type = "text")
(mod1_confint <- coefci(mod1, vcov = vcov(mod1), cluster =  ~ res_name))


# Model the data
models <- list("OLS" = mod1, "OLS + year" = mod2, "Region FE" = mod3, "Twoway FE" = mod4)
coefficents <- lapply(models, tidy, conf.int = T)
coefficents <- bind_rows(coefficents, .id = "model")
coefficents$term[coefficents$term == "share_detrend"] <- "share"

# Add the clusterd confidence intervals
coefficents[coefficents$model == "OLS", c("conf.low", "conf.high")] <- coefci(mod1, vcov = vcov(mod1), cluster =  ~ res_name)
coefficents[coefficents$model == "OLS + year", c("conf.low", "conf.high")] <- coefci(mod2, vcov = vcov(mod2), cluster =  ~ res_name)
plot_panel_coeff_reg <- ggplot(data = subset(coefficents, str_detect(term, "share")), aes(x = model, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point(size = 3) +
  geom_text(data = subset(coefficents, str_detect(model, "OLS") & str_detect(term, "share")), aes(label = "*"), size = 8, nudge_x = 0.1) +
  geom_hline(yintercept = 0) +
  geom_linerange(size = 1.4) +
  scale_y_continuous("Coefficient for % change in religiously affiliated in region", n.breaks = 5, expand = c(0, 0)) +
  scale_x_discrete("Regression model") +
  coord_flip() +
  labs(caption = "* Confidence intervals are based on clustered standard errors at the municipality level.")
figs(plot_panel_coeff_reg, "panel_reg_rel")


### Population pyramid ----------------------------------------

# Load basic and taxes
load("data/basic.Rda")

# Aggregate the data
basic <- basic[religious, on = .(id, year)]
pop_pyramid <- basic[age %in% 18:65, .(pop = .N), by = .(age, sex, year, set, religious)]
pop_pyramid$pop <- with(pop_pyramid, ifelse(sex == "Male", -pop, pop)) 

# Plot the overall population pyramid
pop_pyramid %>% 
  filter(year %in% c(1987, 2000, 2019) & !is.na(set)) %>% 
  ggplot(aes(x = age, y = pop/1000, fill = factor(religious))) +
  geom_col() +
  geom_hline(yintercept = 0) +
  facet_grid(year  ~ set, scales = "free_x") + 
  coord_flip() +
  scale_fill_manual(values = colors, name = "", ) +
  scale_y_continuous("Population (in k)") +
  scale_x_continuous("Age", n.breaks = 10, expand = c(0, 0)) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 15, face = "bold")
  )
ggsave(last_plot(), filename = "figures/pop_pyramid_rel_set.pdf", height = 20, width = 20, unit = "cm")

# Plot the overall population pyramid
pop_pyramid[, .(pop = sum(pop)), by = .(age, sex, year, religious)] %>% 
  filter(year %in% c(1987, 2000, 2010, 2019)) %>% 
  ggplot(aes(x = age, y = pop/1000, fill = factor(religious))) +
  geom_col() +
  geom_hline(yintercept = 0) +
  facet_wrap( ~ year) + 
  coord_flip() +
  scale_fill_manual(values = colors, labels = c("non-religious", "religious"), name = "religious") +
  scale_y_continuous("Population (in k)") +
  scale_x_continuous("Age", n.breaks = 10, expand = c(0, 0), limits = c(18, 55)) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 15, face = "bold")
  )
ggsave(last_plot(), filename = "figures/pop_pyramid_rel.pdf", height = 20, width = 20, unit = "cm")

### Aggregate the data ----------------------------------------


# Merge the two data sets
basic <- basic[age >= 18 & age <= 55 & ori %in% c(11, 12), ]

# Join with religious
basic <- basic[religious, on = .(year, id)]

# Estimate the share by settlement type
share_set <- basic[!is.na(set), .(share = mean(religious, na.rm = T)), by = .(year, set)]

# Plot the distribution
ggplot(data = subset(share_set, !is.na(set)), aes(x = year, y = share, colour = set, group = set, shape = set)) +
  geom_line(size = 1.3) +
  geom_point(size = 3) +
  geom_text(data = subset(share_set, year == 1996), aes(label = set), hjust = "left", vjust = 1, size = 10, family = "serif") +
  scale_x_continuous("Year", breaks = scales::pretty_breaks(n = 10), expand = c(0, 0.3)) +
  scale_y_continuous("Share religious", expand = c(0, 0.01), breaks = scales::pretty_breaks(n = 10), labels = scales::percent_format(accuracy = 5L)) +
  scale_shape(name = "") +
  scale_colour_manual(name = "", values = colors) +
  guides(colour = "none", shape = "none")

ggsave(last_plot(), filename = "figures/spatial_religion.pdf")

# Estimate the share of religious per region and year
share_res_name <- basic[, .(share = mean(religious)), by = .(year, res_name)]

### Prepare the map data --------------------------------------

# Load the shape data
shape <- st_read("D:/metadata/classifications/shapefiles/kunta4500k_2020Polygon.shp")
shape$kunta <- as.numeric(shape$kunta)
shape <- left_join(shape, reg, by = c("kunta" = "res"))
shape <- shape %>% group_by(res_name) %>% summarise(.group = "drop")

# Create the data
shape <- subset(shape, select = c(res_name, geometry))

# Seutukunta
shape_kunta <- shape  %>% 
  group_by(res_name) %>% 
  summarize(.groups = "drop")

# Merge the data
map_data <- inner_join(shape_kunta, share_res_name, by = c("res_name" = "res_name"))

# Create the map
map_data %>% 
  filter(year %in% c(1996, 2005, 2018)) %>% 
  ggplot(aes(fill = share)) +
  geom_sf(size = 0.05) +
  facet_wrap( ~ year) +
  scale_fill_gradient(name = "Share religous:", low = colors[1], high = colors[2], label = scales::percent_format(accuracy = 5L), n.breaks = 8, direction = -1) +
  theme(
    legend.key.width = unit(2, "cm"),
    strip.background = element_blank(),
    strip.text = element_text(size = 15, face = "bold"),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

# Save
ggsave(last_plot(), filename = "figures/map_share.pdf", height = 15, width = 20, unit = "cm")


### Estimate the deviation from the mean -------------------

# Estimate the annual share
mean_annual <- basic[, .(share_mean = mean(religious, na.rm = T)), by = year]

# Join the map data with annual data
map_data <- inner_join(map_data, mean_annual)

# Estimate the deviation from the mean
map_data <- map_data %>% mutate(diff = share - share_mean)

# Create the map
map_data %>% 
  filter(year %in% c(1996, 2005, 2018)) %>% 
  ggplot(aes(fill = diff)) +
  geom_sf(size = 0.05) +
  facet_wrap( ~ year) +
  scale_fill_gradient2(name = "Deviation from country-year mean \n  % share religous:", low = "navyblue", mid = "white", high = "firebrick", midpoint = 0, label = scales::percent_format(accuracy = 5L), n.breaks = 8) +
  #scale_fill_binned(low = "navyblue", high = "red", midpoint = 0, mid = "white") +
  theme(
    legend.key.width = unit(2, "cm"),
    strip.background = element_blank(),
    strip.text = element_text(size = 15, face = "bold"),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

ggsave(last_plot(), filename = "figures/map_share_dev.pdf", height = 15, width = 20, unit = "cm")

### END #####################################################