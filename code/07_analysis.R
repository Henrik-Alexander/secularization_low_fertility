#############################################
# Project: Secularization and low fertility 
# Purpose: analysis                         
# Name: Henrik-Alexander Schubert           
# Date: 05.07.2023                          
# E-mail: schubert@demogr.mpg.de            
# Pre-requisites: functions                 
#############################################

rm(list = ls())

# Load the packages
library(tidyverse)
library(data.table)
library(stargazer)
library(mvtnorm)
library(mgcv)
library(splines)
library(marginaleffects)

# Load the functions and packages
source("functions/functions.R")
source("functions/graphics.R")
source("functions/models.R")
source("functions/bam_table.R")

size <- F

# MODELLING: ----------------------------------------

# Load the data
load("data/final_data.Rda")

## Smooth poisson models --------------------------

# Run the smooth models
poiss_mod1 <- poiss_model(d, c("religious", "act", "act_par", "edu", "edu_par", "inc_quant", "inc_quant_par", "set", "period"))
poiss_mod2 <- poiss_model(d, c("religious_par", "act", "act_par", "edu", "edu_par", "inc_quant", "inc_quant_par", "set", "period"))
poiss_mod3 <- poiss_model(d, c("religious", "religious_par", "act", "act_par", "edu", "edu_par", "inc_quant", "inc_quant_par", "set", "period"))
poiss_mod4 <- poiss_model(d, c("religious", "religious_par", "act", "act_par", "edu", "edu_par", "inc_quant", "inc_quant_par", "set", "period"), interaction = T, constant = F)

# Print the results
poiss_models <- mget(ls(pattern = "poiss_mod[1-4]"))
table_bams(poiss_models, file_directory = "results/poisson_models_cleaned.tex")
rm(poiss_mod1, poiss_mod2, poiss_mod3, poiss_mod4)

# Predict the results
pred_data <- expand.grid(age_onset = 18:50, union_duration = 0:10, rel = 0:1, rel_par = 0:1,
                         edu = most_frequent(d$edu), edu_par = most_frequent(d$edu_par), act = most_frequent(d$act), act_par = most_frequent(d$act_par),
                         set = most_frequent(d$set), period = most_frequent(d$period), inc_quant = most_frequent(d$inc_quant), inc_quant_par = most_frequent(d$inc_quant_par))
save(poiss_models, file = "data/poisson_models.Rda")

# Create the predictions for the different poisson models
for (i in seq_along(poiss_models)) {
  cat("Model:", i, "\n")
  tmp <- pred_data
  prediction <- predict(poiss_models[[i]], pred_data, type = "response", se = T)
  tmp$fit <- prediction$fit
  tmp$se <- prediction$se
  tmp$model <- i
  if (i == 1) {
    result <- tmp
  } else {
    result <- rbind(result, tmp)
  }
}

# Create a factor for the rel variables
result$rel <- factor(result$rel, labels = c("Woman: religiously unaffiliated", "Woman: religiously affiliated"))
result$rel_par <- factor(result$rel_par, labels = c("Man: religiously unaffiliated", "Man: religiously affiliated"))

# Plot the age_onset-specific first birth rate by union_duration
plot_astr_dur <- ggplot(subset(result, model == 4), aes(x = age_onset + union_duration, y = fit, colour = union_duration, group = union_duration)) +
  geom_line(size = 1.5, alpha = .5) +
  facet_grid(rel  ~ rel_par) +
  scale_colour_viridis_c("Union duration:", option = "D") +
  theme(strip.background = element_blank()) +
  scale_y_continuous("Hazard of first birth",limits = c(0, 0.2), expand = c(0, 0)) +
  scale_x_continuous("Age", expand = c(0, 0), n.breaks = 8, limits = c(18, 50)) +
  theme(legend.key.width = unit(2, "cm"),
        panel.spacing.y = unit(0.5, "cm"),
        strip.text = element_text(size = 14, face = "bold"))
figs(plot_astr_dur, "hazard_fb_age_onset_couple", height = 25)

# Plot the risk of transitioning to birth by union duration seperate for different ages at onset
age_labels <- paste("Age at union formation:", 18:55)
names(age_labels) <- 18:55
plot_risk <- ggplot(subset(result, model == 4 & age_onset %in% c(20, 25, 30, 35)), aes(x = union_duration, y = fit, colour = interaction(rel, rel_par))) +
  geom_line() +
  geom_ribbon(aes(ymin = fit - se * 1.96, ymax = fit + se * 1.96, fill = factor(interaction(rel, rel_par))), alpha = .3) +
  facet_wrap( ~ age_onset) +
  scale_colour_viridis_d(name = "Couple composition:", labels = c("Both religiously unaffiliated", "Woman religiously affiliated", "Man religiously affiliated", "Both religiously affiliated"), option = "D") +
  scale_fill_viridis_d(name = "Couple composition:", labels = c("Both religiously unaffiliated", "Woman religiously affiliated", "Man religiously affiliated", "Both religiously affiliated"), option = "D") +
  guides(colour = guide_legend(nrow = 2)) +
  scale_y_continuous("Hazard of first birth", limits = c(0, 0.2), expand = c(0, 0), n.breaks = 8) +
  scale_x_continuous("Union duration", expand = c(0, 0), n.breaks = 8) +
  theme(strip.background = element_blank(),
        panel.spacing.x = unit(0.5, "cm"),
        strip.text = element_text(face = "bold", size = 15))
figs(plot_risk, "hazard_fb_union_duration", width = 25)
rm(poiss_models)

## Event-history models -----------------------------------------------

size <- F

# Estimate the basic model
log_tv_mod1 <- logistic_model(d, c("religious", "act", "act_par", "edu", "edu_par", "inc_quant", "inc_quant_par", "set", "period"), size = size)
log_tv_mod2 <- logistic_model(d, c("religious_par", "act", "act_par", "edu", "edu_par", "inc_quant", "inc_quant_par", "set", "period"), size = size)
log_tv_mod3 <- logistic_model(d, c("religious", "religious_par", "act", "act_par", "edu", "edu_par", "inc_quant", "inc_quant_par", "set", "period"), size = size)
log_tv_mod4 <- logistic_model(d, c("religious", "religious_par", "act", "act_par", "edu", "edu_par", "inc_quant", "inc_quant_par", "set", "period"), size = size, interaction = T, constant = F)

# Save the models
log_tv_models <- mget(ls(pattern = "log_tv_mod[0-9]"))
table_bams(log_tv_models, file_directory = "results/log_tv_cleaned.tex")
save(log_tv_models, file = "results/log_tv_mod.Rda")
rm(log_tv_mod2, log_tv_mod3, log_tv_mod4)

# Estimate the basic model
log_tc_mod1 <- logistic_model(d, c("rel", "act", "act_par", "edu", "edu_par", "inc_quant", "inc_quant_par", "set", "period"))
log_tc_mod2 <- logistic_model(d, c("rel_par", "act", "act_par", "edu", "edu_par", "inc_quant", "inc_quant_par", "set", "period"))
log_tc_mod3 <- logistic_model(d, c("rel", "rel_par", "act", "act_par", "edu", "edu_par", "inc_quant", "inc_quant_par", "set", "period"))
log_tc_mod4 <- logistic_model(d, c("rel", "rel_par", "act", "act_par", "edu", "edu_par", "inc_quant", "inc_quant_par", "set", "period"), interaction = T)
models <- mget(ls(pattern = "log_tc_mod[0-9]"))
table_bams(models, file_directory = "results/log_tc_cleaned.tex")
save(models, file = "results/log_tc_mod.Rda")


### Average marginal effects ---------------------------

# Print the average marginal effects
ame <- map(models, marginaleffects)
ame <- map(ame, summary)

# Create a table with the average marginal effects
clean_ame_table <- function(ame_table) {
  ame_table[, c("estimate", "conf.low", "conf.high")] <- apply(ame_table[, c("estimate", "conf.low", "conf.high")], 2, function(x) round(x, digits = 3))
  ame_table$effect <- paste0(ame_table$estimate, " (", ame_table$conf.low, ", ", ame_table$conf.high, ")")
  ame_table$term <- paste0(ame_table$term, ": ", ame_table$contrast)
  return(ame_table[, c("term", "effect")])
}
ame_table <- lapply(ame, clean_ame_table)
tmp1 <- full_join(ame_table$log_tc_mod1, ame_table$log_tc_mod2, by = "term", suffix = c(" Model 1", " Model 2"))
tmp2 <- full_join(ame_table$log_tc_mod3, ame_table$log_tc_mod4, by = "term", suffix = c(" Model 3", " Model 4"))
ame_table <- full_join(tmp1, tmp2, by = "term"); rm(tmp1, tmp2)
stargazer(ame_table,
          summary = F,
          title = "Average marginal effect of the discrete-time survival model on the transition to first birth.",
          out = "results/ame_tc_log.tex")

# Plot the average marginal effects
ame_plot <- bind_rows(ame, .id = "model")
ame_plot$significant <- ifelse(ame_plot$p.value < 0.1, 1, 0)
names(ame_plot) <- str_to_lower(names(ame_plot))
ame_plot <- ame_plot[str_detect(ame_plot$term, "rel"), c("model", "term", "contrast", "estimate", "significant", "conf.low", "conf.high")]
ame_plot$term <- clean_labels(ame_plot$term)
ame_plot$model <- gsub("log_tc_mod", "Model ", ame_plot$model)
ggplot(ame_plot, aes(x = term, y = estimate, alpha = significant, colour = model, shape = model)) +
  geom_hline(yintercept = 0) +
  geom_point(position = position_dodge(width = 0.4), size = 4) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(width = 0.4), size = 1.5) +
  coord_flip(ylim = c(-0.005, max(ame_plot$estimate)+0.01)) +
  scale_x_discrete("") +
  scale_y_continuous("Average marginal effect") +
  scale_colour_manual(values = colors) +
  guides(alpha = "none",
         colour = guide_legend(nrow = 2, byrow = T)) 
figs(last_plot(), "ame_tc_log", height = 10, width = 20, unit = "cm")

save(ame, file = "results/marginal_effects_logmodel.Rda")


### AIC's model comparison -----------------------------

# Compare the Akaike information criterion
aics <- sapply(models, function(model) model[["aic"]])
which.min(aics)
write.csv(data.frame(model = names(aics), aic = aics), file = "results/model_aics.csv")

# Make the final regression table
stargazer(log_tv_mod1, log_tv_mod2, log_tv_mod3, log_tv_mod4,
          type = "text",
          apply.coef = exp,
          style = "asr",
          label = "tab: regression_tc",
          title = "Results from discrete-time survival analysis using logit-bionial models on the probability of childbirth. Results are displayed in log-odds. The couple composition with respect to religious affilaition is held time-constant.",
          out = "results/reg_dcts_tc.tex")


# Predict the results
pred_data <- expand.grid(age_onset = 18:50, union_duration = 0:10, rel = 0:1, rel_par = 0:1,
                         edu = most_frequent(d$edu), edu_par = most_frequent(d$edu_par), act = most_frequent(d$act), act_par = most_frequent(d$act_par),
                         set = most_frequent(d$set), period = most_frequent(d$period), inc_quant = most_frequent(d$inc_quant), inc_quant_par = most_frequent(d$inc_quant_par))
log_models <- mget(ls(pattern = "log_tc_mod\\d"))
save(log_models, file = "data/models_logistic_tc.Rda")

# Create the predictions for the different poisson models
for (i in seq_along(log_models)) {
  cat("Model:", i, "\n")
  tmp <- pred_data
  prediction <- predict(log_models[[i]], pred_data, type = "response", se = T)
  tmp$fit <- prediction$fit
  tmp$se <- prediction$se
  tmp$model <- i
  if (i == 1) { result <- tmp
  } else {
    result <- rbind(result, tmp)
  }
}

# Create a factor for the rel variables
result$rel <- factor(result$rel, labels = c("Woman: religiously unaffiliated", "Woman: religiously affiliated"))
result$rel_par <- factor(result$rel_par, labels = c("Man: religiously unaffiliated", "Man: religiously affiliated"))

# Plot the age_onset-specific first birth rate by union_duration
plot_astr_dur <- ggplot(subset(result, model == 3), aes(x = age_onset + union_duration, y = fit)) +
  geom_line(size = 1.5, aes(colour = union_duration, group = union_duration)) +
  facet_grid(rel  ~ rel_par) +
  scale_colour_steps("Union duration:", low = colors[1], high = colors[2], breaks = seq(0, 10, by = 2)) +
  theme(strip.background = element_blank()) +
  scale_y_continuous("Hazard of first birth",limits = c(0, max(result$fit)), expand = c(0, 0)) +
  scale_x_continuous("Age", expand = c(0, 0), n.breaks = 8, limits = c(18, 50)) +
  theme(panel.spacing.y = unit(0.5, "cm"),
        strip.text = element_text(size = 14, face = "bold"),
        legend.key.width = unit(2, "cm"))
figs(plot_astr_dur, "prob_fb_age_onset_couple", height = 15, width = 15)  

# Plot the risk of transitioning to birth by union duration seperate for different ages at onset
age_labels <- paste("Age at union formation:", 18:55)
names(age_labels) <- 18:55
plot_risk <- ggplot(subset(result, model == 3 & age_onset %in% seq(20, 35, by = 5)), aes(x = union_duration, y = fit, colour = interaction(rel, rel_par))) +
  geom_line(size = 2) +
  geom_ribbon(aes(ymin = fit - se * 1.96, ymax = fit + se * 1.96, fill = factor(interaction(rel, rel_par))), alpha = .2, colour = NA) +
  scale_colour_manual(name = "Couple composition:", labels = c("Both religiously unaffiliated", "Woman religiously affiliated", "Man religiously affiliated", "Both religiously affiliated"), values = colors) +
  scale_fill_manual(name = "Couple composition:", labels = c("Both religiously unaffiliated", "Woman religiously affiliated", "Man religiously affiliated", "Both religiously affiliated"), values = colors) +
  guides(colour = guide_legend(nrow = 2)) +
  facet_wrap( ~ union_duration) +
  scale_y_continuous("Hazard of first birth", limits = c(0, 0.2), expand = c(0, 0), n.breaks = 8) +
  scale_x_continuous("Union duration", expand = c(0, 0), n.breaks = 8) +
  theme(strip.background = element_blank(),
        panel.spacing.x = unit(0.5, "cm"),
        strip.text = element_text(face = "bold", size = 15))
figs(plot_risk, "prob_fb_union_duration", height = 25)

# Plot the transition risk by union duration
age_labels <- paste("Age at union formation:", 18:55)
names(age_labels) <- 18:55
plot_risk <- ggplot(subset(result, model == 3 & age_onset %in% c(24)), aes(x = union_duration, y = fit, colour = interaction(rel, rel_par))) +
  geom_line() +
  geom_ribbon(aes(ymin = fit - se * 1.96, ymax = fit + se * 1.96, fill = factor(interaction(rel, rel_par))), alpha = .2, colour = NA) +
  scale_colour_manual(name = "Couple \ncomposition:", labels = c("Both religiously unaffiliated", "Woman religiously affiliated", "Man religiously affiliated", "Both religiously affiliated"), values = colors) +
  scale_fill_manual(name = "Couple \ncomposition:", labels = c("Both religiously unaffiliated", "Woman religiously affiliated", "Man religiously affiliated", "Both religiously affiliated"), values = colors) +
  guides(colour = guide_legend(nrow = 2)) +
  scale_y_continuous("Hazard of first birth", limits = c(0, 0.2), expand = c(0, 0), n.breaks = 8) +
  scale_x_continuous("Union duration", expand = c(0, 0), n.breaks = 8) +
  theme(strip.background = element_blank(),
        panel.spacing.x = unit(0.5, "cm"),
        strip.text = element_text(face = "bold", size = 15)) +
  labs(caption = "For couples that were formed when the female partner is age 24.")
figs(plot_risk, "main_prob_firstbirth_union_duration", height = 12, width = 20)

# Plot the models
model_plots <- map(log_models, plot_gam, var_like = "rel")
map2(model_plots, figs, .y = paste0("coeff_plot_log_tc", 1:4))


### Prediction plot ----------------------

# Model get the variance covariance matrix
variance <- vcov(log_tc_mod4)

# Simulate betas
betas <- rmvnorm(n = 1000, mean = coef(log_tc_mod4), sigma = variance)

# Create the data
predictions <- expand.grid(age = 18:55, religion = 0:1, religion_par = 0:1)

# Predict the result
predictions <- predictions %>% 
  mutate(prediction = pmap(list(age, religion, religion_par), pred_logistic))

# Estimate the 90%-quantiles
predictions$lower <- map_dbl(predictions$prediction, quantile, probs = 0.05, na.rm = T)
predictions$higher <- map_dbl(predictions$prediction, quantile, probs = 0.95, na.rm = T)

# Clean the data
predictions <- predictions %>% 
  select(age, religion, religion_par, higher, lower) %>% 
  mutate(across(c(lower, higher), odd_prob))

# Plot the result
ggplot(predictions, aes(age, ymin = lower, ymax = higher, group = interaction(religion, religion_par), fill = interaction(religion, religion_par))) +
  geom_ribbon(alpha = .8)  +
  scale_fill_manual(name = "", values = colors, 
                    labels = c("Hom: non-rel", "Mixed: female rel", "Mixed: female non-rel", "Hom: rel") ) +
  scale_x_continuous("Age", expand = c(0, 0)) +
  scale_y_continuous("Probability of first birth", expand = c(0, 0))
ggsave(last_plot(), filename = "figures/age_rel_prob_firstbirth.pdf", height = 16, width = 25, unit = "cm")

# Estimate the couple first birth probability
lower  <- aggregate(higher ~ religion + religion_par, FUN = sum, data = predictions)
higher <- aggregate(lower ~ religion + religion_par, FUN = sum, data = predictions)

# Plot the result
inner_join(lower, higher) %>% 
  mutate(rate = round((higher + lower)/2, 2)) %>% 
  ggplot(aes(x = factor(religion, labels = c("rel", "not rel")), y = rate)) +
  geom_col(aes( fill = factor(religion_par)), position = position_dodge()) +
  geom_errorbar(aes(ymin = lower, ymax = higher), position = position_dodge(), width = 0.1) +
  geom_text(aes(label = rate), colour = "white", position = position_dodge()) +
  scale_colour_manual(values = colors, labels = c("rel", "not rel"), name = "rel male partner") +
  scale_x_discrete("rel female partner") +
  scale_y_continuous("Total first birth rate", expand = c(0, 0), limits = c(0, 3)) +
  scale_fill_viridis_d(name = "Male:", option = "D", 
                    labels = c("non-rel", "rel") ) 


# Remove all nonlinear models
rm(list = ls(pattern = "mod"))


### Municipality church-tax ------------------------------------

load("data/res_tax_rate.Rda")
source("functions/tax_tables.R")

# Merge the data
tax_rate <- d[tax_rate, on = c("year", "res")]

# Estimate the models
log_tv_tax_mod1 <- logistic_model(tax_rate, c("religious", "act", "act_par", "edu", "edu_par", "inc_quant", "inc_quant_par", "set", "period", "church_tax_rate"), size = size)
log_tv_tax_mod2 <- logistic_model(tax_rate, c("religious_par", "act", "act_par", "edu", "edu_par", "inc_quant", "inc_quant_par", "set", "period", "church_tax_rate"), size = size)
log_tv_tax_mod3 <- logistic_model(tax_rate, c("religious", "religious_par", "act", "act_par", "edu", "edu_par", "inc_quant", "inc_quant_par", "set", "period", "church_tax_rate"), size = size)
log_tv_tax_mod4 <- logistic_model(tax_rate, c("religious", "religious_par", "act", "act_par", "edu", "edu_par", "inc_quant", "inc_quant_par", "set", "period", "church_tax_rate"), size = size, interaction = T, constant = F)

# Create tables
mods_tax <- mget(ls(pattern = "log_tv_tax"))
table_tax(mods_tax, file_directory = "results/tax_control_models.tex")

### Linear probability models -------------------------------------

# Estimate the basic mod_lmel
mod_lm1 <- lm(birth  ~ rel + rel_par + rel:rel_par + age + age2, data = d)
mod_lm2 <- lm(birth ~ rel + rel_par + rel:rel_par + age + age2 + edu + edu_par, data = d )
mod_lm3 <- lm(birth ~ rel + rel_par  + rel:rel_par + age + age2+ inc + edu + edu_par + hhinc + act + act_par, data = d )

# Collect the model
models_linear <- mget(ls(pattern = "mod_lm[0-9]"))
lapply(models_linear, summary)

# Save the model results
save(models_linear, file = "data/models_ols.Rda")

# Counterfactual trend -----------------------------

# Load the functions
source("functions/counterfactual.R")

# Split the data into seperate chunks
d2 <- d[year >= 1996, ]
obs <- split(d2, d2$year)
baseline <- obs[[1]]

# Estimate the model
prediction_model <- logistic_model(d, c("religious", "religious_par", "act", "act_par", "edu", "edu_par", "inc_quant", "inc_quant_par", "set"), interaction = T)

# Predict the births for the observed data
obs_prediction <- map(obs, prediction, prediction_model)

# Create the counterfactual composition
obs <- obs[2:length(obs)]
count <- map(obs, estimate_counterfactual, baseline)
count_prediction <- map(count, prediction, prediction_model)

# Estimate the tfr
observed <- map_dbl(obs_prediction, get_tfr)
counterfactual <- map_dbl(count_prediction, get_tfr)
result <- data.frame("observed" = observed, "counterfactual" = counterfactual, "year" = as.numeric(names(observed)))
result <- result %>%
  mutate(across(c(observed, counterfactual),  ~ (.x - observed[1]) / observed[1], .names = "change_{.col}"))
result$difference <- with(result, ifelse(observed > counterfactual, 1, 0))

# Plot the result
count_couple_tfr <- ggplot(result, aes(x = year)) +
  geom_line(aes(y = observed, colour = "Observed")) +
  geom_line(aes(y = counterfactual, colour = "Counterfactual")) +
  geom_point(aes(y = observed, colour = "Observed"), size = 2) +
  geom_point(aes(y = counterfactual, colour = "Counterfactual"), size = 2, shape = 17) +
  scale_y_continuous("Couple TFR", expand = c(0, 0.01)) +
  scale_x_continuous("Year", n.breaks = 12, expand = c(0, 0.2)) +
  scale_colour_manual("Couple TFR:", values = colors)
figs(count_couple_tfr, "count_couple_tfr")

# Plot the result
counterfactual_tfr_trend <- ggplot(result, aes(x = year)) +
  geom_hline(yintercept = 0) +
  geom_line(aes(y = change_observed, colour = "Observed")) +
  geom_line(aes(y = change_counterfactual, colour = "Counterfactual")) +
  geom_point(aes(y = change_observed, colour = "Observed"), size = 2) +
  geom_point(aes(y = change_counterfactual, colour = "Counterfactual"), size = 2, shape = 17) +
  scale_y_continuous("Difference in couple TFR to 1996", expand = c(0, 0.0025)) +
  scale_x_continuous("Year", n.breaks = 12, expand = c(0, 0.2)) +
  scale_colour_manual("", values = colors)
figs(counterfactual_tfr_trend, "counterfactual_couple_tfr")

### Explore the shape of the hazard ---------------------------------

# Create a variable
d <- d %>% mutate(couple = case_when(religious == 0 & religious_par == 0  ~ "Both religiously unaffiliated",
                                     religious == 1 & religious_par == 0  ~ "Female religiously affiliated",
                                     religious == 0 & religious_par == 1  ~ "Male religiously affiliated",
                                     religious == 1 & religious_par == 1  ~ "Both religiously affiliated",))


# Split the data by religious and religious_partner
d_split <- split(d, d$couple)

# Transform the data
d_split <- map(d_split, prepare_data)

# Run the generalized additive model
mymodel <- map(d_split, spline_regress)

# Predict the hazard
predict <- map(mymodel, predict_spline)

# Combine the result
predict <- bind_rows(predict, .id = "model")
predict <- predict[predict$age_onset %in% seq(20, 35, by = 5), ]

# Reorder data
predict$model <- factor(predict$model, levels = c("Both religiously unaffiliated",  "Female religiously affiliated", "Male religiously affiliated", "Both religiously affiliated"), ordered = T)

# Plot the result
ggplot(predict, aes(x = union_duration, y = fit, colour = model, group = model, shape = model)) +
  geom_point() +
  geom_line() +
  facet_wrap( ~ age_onset) +
  geom_ribbon(aes(ymin = fit - 1.96*se.fit, ymax = fit + 1.96*se.fit, fill = model), alpha = .3) +
  scale_x_continuous("Union duration", expand = c(0, 0), n.breaks = 10) +
  scale_y_continuous("Couple's first birth hazard rate", expand = c(0, 0), limits = c(0, 0.25)) +
  scale_fill_manual(values = colors, name = "") +
  scale_colour_manual(values = colors, name = "") +
  scale_shape(name = "") +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 14),
        panel.spacing.x = unit(0.5, "cm")) +
  guides(colour = guide_legend(nrow = 2), fill = guide_legend(nrow = 2), shape = guide_legend(nrow = 2))
ggsave(last_plot(), filename = "figures/hazard_rel.pdf", height = 15, width = 20, unit = "cm")

# Estimate the first birth rate
first_birth_rate <- aggregate(fit  ~ model, data = predict, FUN = sum)

# stimate the mean age of first childbirth
predict %>% 
  group_by(model) %>% 
  summarize(mac = sum(union_duration * fit) / sum(fit))

### END ##########################################################