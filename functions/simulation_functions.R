#############################################
# Project: Secularization and low fertility #
# Purpose: Functions for IPF and simulation #
# Author: Henrik-Alexander Schubert         #
# E-mail: schubert@demogr.mpg.de            #
# Date: 15.04.2024                          #
#############################################


# Combine religious categories
combine_cat <- function (rel1, rel2) {
  rel3 <- rel1 + rel2
  rel3 <- factor(rel3, labels = groups)
  return(rel3)
}

# Estimate the proportion of categories
estimate_prop <- function (categories) {
  tmp <- 100 * table(categories) / length(categories)
  return(tmp)
}

##  For the deming-stephan algorithmus --------------------

# Estimate the error
est_error <- function(counterfact, target_row, target_column) {
  rowcheck <- sum(abs(rowSums(counterfact) - target_row)) / sum(target_row)
  colcheck <- sum(abs(colSums(counterfact) - target_column)) / sum(target_row)
  return(rowcheck + colcheck)
}

# Create a matrix
observed <- matrix(c(100, 200, 300, 400), 2, 2)
reference <- matrix(c(500, 200, 100, 800), 2, 2)

# Function to estimate the share
est_share <- function(cont_table) {
  tmp <- melt(cont_table)
  tmp$share <- tmp$value / sum(tmp$value)
  return(tmp)
}


count_exposures <- function(ref_table, obs_table, female = T, male = T) {
  # Purpose: Creates a counterfactual pop distribution
  # with the same total number and distribution but changed marginal distribution
  # Input: Reference table from year 2010 and an observed table
  # Output: A new table for the observed year
  if (female & !male){
    ## Female share remains constant
    count_dist <- (rowSums(ref_table) / sum(rowSums(ref_table))) * sum(rowSums(obs_table))
    result <- deming_stephan2(row_tar = count_dist, col_tar = colSums(obs_table), reference = obs_table)
  } else if (!female & male){ 
    ## Male share remains constant
    count_dist <- (colSums(ref_table) / sum(colSums(ref_table))) * sum(colSums(obs_table))
    result <- deming_stephan2(row_tar = rowSums(obs_table), col_tar = count_dist, reference = obs_table)
  } else if (female & male){ 
    ## Male and female share constant
    count_col <- (colSums(ref_table) / sum(colSums(ref_table))) * sum(colSums(obs_table))
    count_row <- (rowSums(ref_table) / sum(rowSums(ref_table))) * sum(rowSums(obs_table))
    result <- deming_stephan2(row_tar = count_row, col_tar = count_col, reference = obs_table)
  } else {
    stop("You need to change a column")
  }
  return(result)
}

# IPF: Deming-Stephan algorithm 
deming_stephan <- function(row_tar = rowSums(observed), col_tar = colSums(observed), reference, maxit = 1000, closure = 0.00001) {
  
  # Start the first iteration
  it <- 0
  improv <- 1
  
  # Start changing
  counterfactual <- reference
  
  # Estimate the baseline error
  error <- est_error(counterfactual, row_tar, col_tar)
  
  # Start transforming the data
  while(error > closure & maxit > it & improv != 0) {
    # Estimate the error
    prev_error <- error
    
    # Transform the data
    counterfactual <- counterfactual * row_tar/ rowSums(counterfactual)
    counterfactual <- t(t(counterfactual) * col_tar / colSums(counterfactual))
    
    # Estimate the error
    error <- est_error(counterfactual, row_tar, col_tar)
    cat("------------------- \n Error = ", error, "\n Iteration: ", it, "\n")
    it <- it + 1
    imprv <- error - prev_error
  }
  
  return(counterfactual)
  
}
