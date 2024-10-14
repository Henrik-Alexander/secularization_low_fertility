################################################
# Project: Secularization and fertility decline
# Purpse: Model functions
# Author: Henrik-Alexander Schubert              
# E-mail: schubert@demogr.mpg.de               
# Date: 23.02.2024                             
################################################


# Function to smooth the TFR using splines --------------------

# Smooth TFR through age groupings
create_age_groups <- function(data) {
  data$age_group <- cut(data$age, breaks = seq(min_age, max_age, by = 5), include.lowest = T, right = F)
  return(data)
}

# Estimate gam and predict
smooth_tfr <- function(data) {
  data <- as.data.frame(data)
  data$exposure[data$exposure == 0] <- 1
  data$year2 <- data$year^2
  data$age_group <- factor(data$age_group)
  smooth_model <- glm(births ~ year + year2 + age_group + year * age_group + year2 * age_group, data = data, offset = log(exposure), family = "poisson")
  data$births <- predict(smooth_model, data, type = "response")
  return(data)
}

# Funcitons for the analysis.R file ---------------------------

# Function to predict probability
pred_logistic <- function(age, religion, religion_par) {
  values <- c(1, religion, religion_par, age, age^2, mean(d$inc), 1, 0, mean(d$hhinc), 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, religion * religion_par)
  tmp <- sweep(betas, MARGIN = 2, values, '*')
  tmp <- rowSums(tmp)
  return(tmp)
}

# Write the logistic regression model with random sampling
logistic_model <- function(data = d, covars, size = F, interaction = F, seed = 1234, constant = T){
  set.seed(seed)
  if (is.numeric(size)) {
   # Sample the ids
   ids <- sample(unique(data$id), size = size, replace = F)
   tmp <- data[id %in% ids, ]
  }else{
    tmp <- data
  }
  
  # Run the model
  covars <- paste(covars, collapse = " + ")
  if (interaction & constant) covars <- paste0("rel:rel_par + ", covars)
  if (interaction & !constant) covars <- paste0("religious:religious_par + ", covars)
  bam(as.formula(paste("birth ~ s(age_onset) + s(union_duration) + ti(age_onset, union_duration) +", covars)), data = tmp, family = "binomial")
  # + ti(age_onset, union_duration)
  #gam(as.formula(paste("birth ~ s(age_onset) + s(union_duration) + ", covars)), data = tmp, family = "binomial")
}

# Write the poisson model as a twostep procedure in which 1) the data is aggregated and 2) the model is estimated
poiss_model <- function(data, covariates, interaction = F, constant = F) {
  # Aggregate the data
  poiss_data <- data[, .(exposure = .N, births = sum(birth)), by = c("age_onset", "union_duration", covariates)]
  
  # Run the model
  covars <- paste(covariates, collapse = " + ")
  if (interaction & constant) covars <- paste0("rel:rel_par + ", covars)
  if (interaction & !constant) covars <- paste0("religious:religious_par + ", covars)
  bam(as.formula(paste("births ~ s(age_onset) + s(union_duration) + ti(age_onset, union_duration) + ", covars)), data = poiss_data, offset = log(exposure), family = "poisson")
  # + ti(age_onset, union_duration)
  
}


### Roland's model --------------------------------------

prepare_data <- function (d) {
  
  # Aggregate the data
  events <- aggregate(birth ~ union_duration + age_onset, FUN = sum, data = d)
  d$expos <- 1
  expos <- aggregate(expos ~ union_duration + age_onset, FUN = sum, data = d)
  
  # Combine the data
  rol_data <- inner_join(events, expos)
  return(rol_data)
}

# The spline regression
spline_regress <- function(df) {
  bam(birth ~ s(union_duration) + s(age_onset) + ti(union_duration, age_onset), data = df, family = "poisson", offset = log(expos))
}

# Predict the result
predict_spline <- function(model) {
  pred <- expand.grid(union_duration = 0:10, age_onset = seq(20, 40, by = 5))
  prediction <- predict(model, newdata = pred, type = "response", se = T)
  pred <- bind_cols(pred, prediction)
  return(pred)
}


# Plot mcgv-gam models ---------------------

# Make an odds plot
plot_gam <- function(model, var_like = "") {
  summary_model <- summary(model)
  output <- data.frame(summary_model$p.table)
  # Clean the variable names
  output$predictor <- rownames(output)
  output$predictor[output$predictor == "rel"] <- "Female religously affiliated"
  output$predictor[output$predictor == "rel_par"] <- "Male religously affiliated"
  output$predictor[output$predictor == "rel:rel_par"] <- "Both religously affiliated"
  
  # Prepare the dataset
  times <- summary_model$pTerms.table[, 1]
  variable <- row.names(summary_model$pTerms.table)
  variable <- c("Intercept", rep(variable, times = times))
  output$variable <- variable; row.names(output) <- NULL
  output$lower <- exp(conf_int(output$Estimate, output$Std..Error))
  output$higher <- exp(conf_int(output$Estimate, output$Std..Error, lower = F))
  ggplot(data = subset(output, predictor != "(Intercept)" & str_detect(predictor, var_like)), aes(x = predictor, y = exp(Estimate))) +
    geom_point(stat = "identity", size = 3) +
    geom_linerange(aes(ymin = lower, ymax = higher), size = 1.3) +
    geom_hline(yintercept = 1) +
    geom_text(aes(label = round(exp(Estimate), 2)), nudge_x = 0.2) +
    coord_flip() +
    scale_y_continuous("Odds")
}

conf_int <- function(coef, se, alpha = 0.1, lower = TRUE) {
  if (lower) {
    coef - qnorm(1-alpha/2) * se
  } else {
    coef + qnorm(1-alpha/2) * se
  }
} 
