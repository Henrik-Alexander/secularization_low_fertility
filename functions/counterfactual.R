#################################################
# Project: Secularization and fertility decline #
# Purpose: counterfactual trend in couple-tfr   #
# Author: Henrik-Alexander Schubert             #
# E-mail: schubert@demogr.mpg.de                #
# Date: 19.02.2024                              #
#################################################

# Functions --------------------------------------------

# Get the couple composition measured in percentage-shares
show_prop <- function(x, y) prop.table(table(x, y)) * 100

# Estimate the convergence of the baseline and the counterfactual couple distribution
estimate_convergence <- function (table_baseline, new_table) sum(abs(table_baseline - new_table))

# Sample randomly from the subsetting vector a number that is related to the size of the differences
sample_corr <- function (subsetting_vector, diff) sample(which(subsetting_vector), size = ifelse(sum(subsetting_vector) > differ_sample(diff), differ_sample(diff), sum(subsetting_vector) / 2))

# Transform the difference into a resampling number
differ_sample <- function(diff) round((diff + 1)^12)

get_tfr <- function(data) {
  tmp <- data[, .(asfr =  sum(prediction, na.rm = T)/.N), by = age]
  return(sum(tmp$asfr, na.rm = T))
}

prediction <- function(data, model){
  data$prediction <- predict(model, data, type = "response")
  return(data)
}

# Change the distribution of couple composition so that it becomre more like the baseline
make_counter <- function (rel_new, rel_par_new, difference, repetition = repetition) {
  
  # Locate the difference
  data <- reshape2::melt(difference)
  if (repetition) {
  red <- order(data$value)[2]  
  add <- order(data$value)[3]
  } else {
  red <- which.min(data$value)
  add <- which.max(data$value)
  }
  # Estimate the adjustment factor
  adj_fact <- sqrt(abs(data$value[red] * data$value[add]))
  
  # Create the new vectors
  rel <- rel_new
  rel_par <- rel_par_new
  
  # Make the changes
  if (red == 1 & add == 2) {
    rel[sample_corr(rel == 0 & rel_par == 0, adj_fact)] <- 1
  } else if (red == 1 & add == 3) {
    rel_par[sample_corr(rel == 0 & rel_par == 0, adj_fact)] <- 1
  } else if (red == 1 & add == 4) {
    rel[sample_corr(rel == 0 & rel_par == 0, adj_fact)] <- 1
    rel_par[sample_corr(rel == 0 & rel_par == 0, adj_fact)] <- 1
  } else if (red == 2 & add == 1) {
    rel[sample_corr(rel == 1 & rel_par == 0, adj_fact)] <- 0
  } else if (red == 2 & add == 3) {
    rel[sample_corr(rel == 1 & rel_par == 0, adj_fact)] <- 0
    rel_par[sample_corr(rel == 1 & rel_par == 0, adj_fact)] <- 1
  } else if (red == 2 & add == 4) {
    rel_par[sample_corr(rel == 1 & rel_par == 0, adj_fact)] <- 1
  } else if (red == 3 & add == 1) {
    rel_par[sample_corr(rel == 0 & rel_par == 1, adj_fact)] <- 0
  } else if (red == 3 & add == 2) {
    rel[sample_corr(rel == 1 & rel_par == 0, adj_fact)] <- 0
    rel_par[sample_corr(rel == 1 & rel_par == 0, adj_fact)] <- 1
  } else if (red == 3 & add == 4) {
    rel[sample_corr(rel == 0 & rel_par == 1, adj_fact)] <- 1
  } else if (red == 4 & add == 1) {
    rel[sample_corr(rel == 1 & rel_par == 1, adj_fact)] <- 0
    rel_par[sample_corr(rel == 1 & rel_par == 1, adj_fact)] <- 0
  } else if (red == 4 & add == 2) {
    rel_par[sample_corr(rel == 1 & rel_par == 1, adj_fact)] <- 0
  } else if (red == 4 & add == 3) {
    rel[sample_corr(rel == 1 & rel_par == 1, adj_fact)] <- 0
  }
  
  return(cbind(rel, rel_par))
}


estimate_counterfactual <- function (d2, d, threshhold = 0.25, max_it = 100) {
  
  cat("Year:", unique(d2$year))
  
  # Extract the vectors
  rel_base <- d[["religious"]]
  rel_par_base <- d[["religious_par"]]
  rel_new <- d2[["religious"]]
  rel_par_new <- d2[["religious_par"]]
  
  # Estimate the differences and the convergence factor
  table_baseline <- show_prop(rel_base, rel_par_base)
  new_table <- show_prop(rel_new, rel_par_new)
  difference <- table_baseline - new_table
  convergences <- convergence <- estimate_convergence(table_baseline, new_table)
  
  # Basic parameters
  it <- 1
  repetition <- F
  
  while (convergence > threshhold & it < max_it) {
    cat("-------------------------------------------- \n")
    cat("Convergence:", convergence, "\n Iteration:", it, "\n")
    
    # Transform to counterfactual values
    new <- make_counter(rel_new, rel_par_new, difference, repetition = repetition)
    rel_new <- new[, 1]
    rel_par_new <- new[, 2]
    
    # Estimate the new convergence
    new_table <- show_prop(rel_new, rel_par_new)
    difference <- table_baseline - new_table
    convergence <- estimate_convergence(table_baseline, new_table)
    
    it <- it + 1
    convergences <- c(convergences, convergence)
    repetition <- any(duplicated(convergences))
  }
  
  # Append the data to the original data frame
  d2$rel <- rel_new
  d2$rel_par <- rel_par_new
  
  return(d2)
}


### END #######################################################