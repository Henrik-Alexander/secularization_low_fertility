
# New labels
new_labels <- c("Intercept", "Religiously affiliated", "Religiously affiliated male",
                "Activity: unemployed",   "Activity: education", "Activity: other",
                "Activity male: unemployed", "Activity male: education", "Activity male: other",
                "Education: medium", "Education: high", "Education male: medium", "Education male: high",
                paste("Income quantile:", 2:4), paste("Income quantile male:", 2:4),
                "Settlement: intermediate", "Settlement: rural", 
                paste("Period:", paste(seq(2000, 2015, by = 5), seq(2004, 2019, by = 5), sep = "-")),
                "Church tax", "Both religiously affiliated")

old_labels <- c("(Intercept)", "religious", "religious_par", "actunemployed", "acteducation", "actother", "act_parunemployed", 
                "act_pareducation", "act_parother","eduEducation: medium", "eduEducation: high", "edu_parEducation: medium", 
                "edu_parEducation: high", "inc_quant2", "inc_quant3", "inc_quant4", "inc_quant_par2", "inc_quant_par3", 
                "inc_quant_par4", "setintermediate", "setrural", "period[2000,2005)", "period[2005,2010)", "period[2010,2015)", 
                "period[2015,2020]", "church_tax_rate", "religious:religious_par")

# label data
label_dict <- data.frame(new_labels, old_labels)

# Function to translate labels
translate_labels <- function(old_labels, dict) {
  dict$new_labels[match(old_labels, dict$old_labels)]
}

# Create not in function


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
  if (!(class(model)[1] %in% c("bam"))) {
    stop("Must be bife or glm!")
  } else if (class(model)[1] == "bam") {
    table_bam(model)
  }
}



### Function create regression table
table_bam <- function(model_output, decimals = 3) {
  if(class(model_output)[1] != "bam") {
    warning("Model is not bife!")
    return(NULL)
  } else {
    res <- summary(model_output)
    coefficients <- round(res$p.coeff, decimals)
    labels <- names(coefficients)
    p_values <- res$p.pv
    coefficients <- paste0(coefficients, create_stars(p_values))
    names(coefficients) <- translate_labels(labels, label_dict)
    result <- list(coefficients, length(log_tv_tax_mod1$fitted.values),  round(res$r.sq, 3),  round(model_output$aic, 3))
    names(result) <- c("Coefficients", "N", "R$^2$", "AIC")
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
  caption <- "Logit-binomial regression of being childless at age 45 on life-time partner market exposure measured by the availability ratio. The models control for the regional church tax rate. This table is displaying logit coefficients, which represent the average response in the log-odds of childlessness to a value change of 1."
  
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
table_tax <- function(list_models, file_directory = "results/log_tax_model.tex") {
  list_models <- lapply(list_models, create_table)
  coefficients <- sapply(list_models, function(x) x[["Coefficients"]])
  coefficients <- sapply(coefficients, function(x) ifelse(label_dict$new_labels %notin% names(x), "", x))
  rownames(coefficients) <- label_dict$new_labels
  model_summary <- sapply(list_models, function(x) x[-1])
  js <- ncol(coefficients)
  create_header(js, file = file_directory)
  matrix_to_tex(coefficients, file_directory)
  cat("\\hline \n", file = file_directory, append = T)
  matrix_to_tex(model_summary, file_directory)
  create_footer(js, file = file_directory)
}
