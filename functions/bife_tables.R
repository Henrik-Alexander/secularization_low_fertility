

# Load the data
load("data/sibling_effects_1.Rda")


# Run an example model
indicator <- "ar_near_9.2km"

# New labels
new_labels <- c("Availability ratio", "Education: high",  "Education: medium", 
                "Income quantile: 2",  "Income quantile: 3", "Income quantile: 4",
                "Activity: Unemployed", "Oldest son",
                "\\% tertiary in region", "Unemployment rate", 
                "Availability:Income Q2", "Availability:Income Q3", "Availability:Income Q4")

### Functions ------------------------------------------


# Create significance
create_stars <- function(p_values) {
  stars <- character(length = length(p_values))
  stars[p_values < 0.05] <- "$^{*}$"
  stars[p_values < 0.05] <- "$^{**}$"
  stars[p_values < 0.001] <- "$^{***}$"
  stars[is.na(p_values)] <- ""
  return(stars)
}

### Function
create_table <- function(model) {
  if (!(class(model)[1] %in% c("bife", "glm"))) {
    stop("Must be bife or glm!")
  } else if (class(model)[1] == "bife") {
    table_bif(model)
  } else {
    table_glm(model)
  }
}


### Function create regression table
table_glm <- function(model_output, decimals = 3) {
  if(class(model_output)[1] != "glm") {
    warning("Model is not glm!")
    return(NULL)
  } else {
    res <- summary(model_output)
    coefficients <- round(res$coefficients[-1, 1], decimals)
    p_values <- res$coefficients[-1, 4]
    names(coefficients) <- new_labels[1:length(coefficients)]
    coefficients <- paste0(coefficients, create_stars(p_values))
    result <- list(coefficients, "No", nrow(between$model), length(unique(d3$sib)), res$null.deviance, res$deviance)
    names(result) <- c("Coefficients", "Sibling FE", "N", "Sibling groups", "Null deviance", "Deviance")
    return(result)
  }
}

### Function create regression table
table_bif <- function(model_output, decimals = 3) {
  if(class(model_output)[1] != "bife") {
    warning("Model is not bife!")
    return(NULL)
  } else {
    res <- summary(model_output)
    coefficients <- round(res$cm[, 1], decimals)
    p_values <- res$cm[, 4]
    names(coefficients) <- new_labels[1:length(coefficients)]
    coefficients <- paste0(coefficients, create_stars(p_values))
    result <- list(coefficients, "Yes", res$nobs[[1]], res$levels, round(res$null_deviance, 2), round(res$deviance, 2))
    names(result) <- c("Coefficients", "Sibling FE", "N", "Sibling groups", "Null deviance", "Deviance")
    return(result)
  }
}


# Function to transalte an R-matrix to a latex-table
matrix_to_tex <- function(table, file_directory) {
  dim <- dim(table)
  for (i in 1:dim[1]) {
    for (j in 1:dim[2]) {
      row <- paste(c(rownames(table)[i], table[i, ]), sep="", collapse=" & ")
    }
    cat(row, "\\\\ \n", file = file_directory, append = TRUE)
  }
} 

# Create header
create_header <- function(ncol, file) {
  caption <- "Logit-binomial regression of being childless at age 45 on life-time partner market exposure measured by the availability ratio for a subset of male sibling groups. Model 1 presents the between-effects, while model 2 presents the sibling fixed-effect result that harnesses only variation within sibling groups. This table is displaying logit coefficients, which represent the average response in the log-odds of childlessness to a value change of 1."
  
  cat("\\begin{table}[]\n", file = file)
  cat("\\caption{", caption, "}\n", 
      file = file, append = T)
  cat("\\label{tab:}\n", file = file, append = T)
  cat("\\begin{tabular}{@{\\extracolsep{5pt}}l", rep("c", ncol) ,"}\n", file = file, append = T)
  cat("\\\\[-1.8ex]\\hline \\hline \\\\[-1.8ex]\n", file = file, append = T)
  cat(" & \\multicolumn{", ncol, "}{c}{Childless at age 45}\\\\\ \n", file = file, append = T)
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
table_feos <- function(list_models, file_directory = "results/sibling_fe.tex") {
  list_models <- lapply(list_models, create_table)
  coefficients <- sapply(list_models, function(x) x[["Coefficients"]])
  rownames(coefficients) <- new_labels[1:nrow(coefficients)]
  model_summary <- sapply(list_models, function(x) x[-1])
  js <- ncol(coefficients)
  create_header(js, file = file_directory)
  matrix_to_tex(coefficients, file_directory)
  cat("\\hline \n", file = file_directory, append = T)
  matrix_to_tex(model_summary, file_directory)
  create_footer(js, file = file_directory)
}