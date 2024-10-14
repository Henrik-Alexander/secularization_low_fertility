#############################################
# Project: Secularization and low fertility #
# Purpose: Regression table for eha-model   #
# Author: Henrik-Alexander Schubert,  #
# E-mail: schubert@demogr.mpg.de,     #
# Date: 15.04.2024, , ,   #
#############################################

# Variable labels
new_labels <- c("Intercept", "Religiously affiliated", "Religiously affiliated male",
                "Activity: unemployed",   "Activity: education", "Activity: other",
                "Activity male: unemployed", "Activity male: education", "Activity male: other",
"Education: medium", "Education: high", "Education male: medium", "Education male: high",
 paste("Income quantile:", 2:4), paste("Income quantile male:", 2:4),
"Settlement: intermediate", "Settlement: rural", 
paste("Period:", paste(seq(2000, 2015, by = 5), seq(2004, 2019, by = 5), sep = "-")),
"Both religiously affiliated")

# Sort variable labels
clean_vector <- function(vector) {
  k <- length(new_labels)
  labels <- clean_variable_labels(names(vector))
  if (length(vector) == k) {
    tmp <- vector
  } else if (length(vector) < k) {
    # ADD something
    tmp <- as.numeric(rep(NA, length = k))
    names(tmp) <- new_labels
    tmp[new_labels %in% labels] <- vector
  } else {
    # Something went wrong
    return(NULL)
  }
  # Rorder vector
  names(tmp) <- new_labels
  result <- c(tmp[2:3], tmp[length(tmp)], tmp[4:(length(tmp)-1)], tmp[1])
  return(result)
}

# Create significance
create_stars <- function(p_values) {
  stars <- character(length = length(p_values))
  stars[p_values < 0.05] <- "$^{*}$"
  stars[p_values < 0.05] <- "$^{**}$"
  stars[p_values < 0.001] <- "$^{***}$"
  stars[is.na(p_values)] <- ""
  return(stars)
}


### Function create regression table
table_bam <- function(model_output, decimals = 3) {
  if(class(model_output)[1] != "bam") {
    warning("Model is not bam!")
    return(NULL)
  } else {
    res <- summary(model_output)
    coefficients <- round(exp(clean_vector(res$p.coeff)), decimals)
    p_values <- clean_vector(res$p.pv)
    labels <- names(coefficients)
    coefficients <- paste0(ifelse(is.na(coefficients), "", coefficients), create_stars(p_values))
    result <- c(coefficients, res$n, round(res$r.sq, 3))
    names(result) <- c(labels, "N", "R$^{2}$")
    return(result)
  }
}

# Helpfer function
str_detect_any <- function(string_vector, pattern){
  any(str_detect(string_vector, pattern))
}

# Clean the variable labels
clean_variable_labels <- function(var_labels) {
  if (str_detect_any(var_labels, "religious_par")) var_labels <- str_replace(var_labels, "^religious", "^rel")
  if (str_detect_any(var_labels, "rel") & str_detect_any(var_labels, "rel_par") & str_detect_any(var_labels, "rel:rel_par")) {
    print("mod4")
  } else if (!str_detect_any(var_labels, "rel$") & str_detect_any(var_labels, "rel_par") & !str_detect_any(var_labels, "rel:rel_par")) {
    print("mod2")
    new_labels <- new_labels[new_labels != "Religiously affiliated" & new_labels != "Both religiously affiliated"]
  } else if (str_detect_any(var_labels, "rel") & !str_detect_any(var_labels, "rel_par") & !str_detect_any(var_labels, "rel:rel_par")) {
    print("mod1")
    new_labels <- new_labels[new_labels != "Religiously affiliated male" & new_labels != "Both religiously affiliated"]
  } else if(str_detect_any(var_labels, "rel$") & str_detect_any(var_labels, "rel_par") & !str_detect_any(var_labels, "rel:rel_par"))  {
    print("mod3")
    new_labels <- new_labels[new_labels != "Both religiously affiliated"]
  } else {
    warning("Shit!")
  }
  return(new_labels)
}


# Create header
create_header <- function(ncol, file) {
  composition <- c("time-varying.", "held constant.")
  caption_1 <- "The couple composition with respect to religious affiliation is"
  caption_2 <- "The age-splines (at couple formation), couple duration-splines and age-duration-splines were omitted for readability."
  if (str_detect(file, "poisson")) {
    caption <- paste("Results from discrete-time survival analysis using poisson models on the probability of childbirth. Results are displayed as hazard ratios.", caption_1, composition[1], caption_2)
  } else if (str_detect(file, "log_tc_")) {
    caption <- paste("Results from discrete-time survival analysis using logit-binomial models on the probability of childbirth. Results are displayed as odds ratios. ", caption_1, composition[2], caption_2)
  } else if (str_detect(file, "log_tv_")) {
    caption <- paste("Results from discrete-time survival analysis using logit-binomial models on the probability of childbirth. Results are displayed as odds ratios.", caption_1, composition[1], caption_2)
  }
  
  
  cat("\\begin{table}[]\n", file = file)
  cat("\\caption{", caption, "}\n", 
      file = file, append = T)
  cat("\\label{tab:}\n", file = file, append = T)
  cat("\\begin{tabular}{@{\\extracolsep{5pt}}l", rep("c", ncol) ,"}\n", file = file, append = T)
  cat("\\\\[-1.8ex]\\hline \\hline \\\\[-1.8ex]\n", file = file, append = T)
  cat(" & \\multicolumn{", ncol, "}{c}{First childbirth}\\\\\ \n", file = file, append = T)
  cat("\\cline{", 2, "-", ncol+1, "}\\\\ [-1.8ex] \n", file = file, append = T)
  cat(paste(c("", paste0("(", 1:ncol, ")")), sep = "", collapse = " & "), "\\\\ \n", file = file, append = T)
  cat("\\hline \\\\[-1.8ex] \n", file = file, append = T)
}



# Create footer
create_footer <- function(ncol, file) {
  cat("\\\\[-1.8ex]\\hline \\hline \\\\[-1.8ex]\n", file = file, append = T)
  cat("\\multicolumn{", ncol+1, "}{l}{$^{*}$p $<$ .05; $^{**}$p $<$ .01; $^{***}$p $<$ .001} \n", file = file, append = T)
  cat("\\end{tabular}\n", file = file, append = T)
  cat("\\end{table}\n", file = file, append = T)
}

# Function 
table_bams <- function(list_models, file_directory = "results/poiss_models.txt") {
  aic <- sapply(list_models, function(x) x[["aic"]])
  list_models <- lapply(list_models, table_bam)
  list_models <- do.call(cbind, list_models)
  list_models <- rbind(list_models, "AIC" = round(aic))
  is <- nrow(list_models); js <- ncol(list_models)
  create_header(js, file_directory)
  for (i in 1:is) {
    for (j in 1:js){
      row <- paste(c(rownames(list_models)[i], list_models[i, ]), sep = "", collapse = " & " )
    }
    cat(row, "\\\\ \n", file = file_directory, append = T)
    if (rownames(list_models)[i] == "Intercept") cat("\\hline \\\\[-1.8ex] \n", file = file_directory, append = T)
  }
  create_footer(js, file = file_directory)
}


### Table for tax data

old_labels <- c("(Intercept)", "religious", "religious_par", "actunemployed", "acteducation", "actother", "act_parunemployed", 
                "act_pareducation", "act_parother","eduEducation: medium", "eduEducation: high", "edu_parEducation: medium", 
                "edu_parEducation: high", "inc_quant2", "inc_quant3", "inc_quant4", "inc_quant_par2", "inc_quant_par3", 
                "inc_quant_par4", "setintermediate", "setrural", "period[2000,2005)", "period[2005,2010)", "period[2010,2015)", 
                "period[2015,2020]", "church_tax_rate", "religious:religious_par")
new_labels <- c(new_labels[1:25], "State church tax", "Both religiously affiliated")

# Create a dictionary
tax_dict <- data.frame(old = old_labels, new = new_labels)


# Create the table for the tax model
#table_tax <- function(list_models, file_directory) {
  list_models <- lapply(mods_tax, summary)
  p_values <- sapply(list_models, function(x) x$p.pv)
  coefficients <- sapply(list_models, function(x) x$p.coeff)
  aics <- sapply(mods_tax, function(x) x$aic)
  coefficients <- lapply(coefficients, function(x) round(x, 3))
  p_values <- do.call(cbind, p_values)
  is <- length(list_models); js <- length(coefficients)
  create_header(js, file_directory)
  for (i in 1:is) {
    for (j in 1:js){
      row <- paste(c(rownames(coefficients)[[j]][i], list_models[[j]][i]), sep = "", collapse = " & " )
    }
    cat(row, "\\\\ \n", append = T)
    if (rownames(list_models)[i] == "church_tax_rate") cat("\\hline \\\\[-1.8ex] \n", file = file_directory, append = T)
  }
  create_footer(js, file = file_directory)
  
#}

### END ###############################