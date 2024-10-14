### Clean the latex tables

# Set the path
files <- list.files(path = "results", pattern = "tex$", full.names = T)

# Load the files
tables <- lapply(files, read.delim)

## Clean the tables --------------------

clean_tex_table <- function(datafile) {

  # Remove the first numbers
  tmp <- apply(datafile, 1, function(x) gsub("^[0-9 ]+&", "", x))
  
  # Remove the underscores
  tmp <- gsub("\\\\_", " ", tmp)
  
  # Replace partner with male
  tmp <- gsub(" par", " male", tmp)
  
  # AGe at union onset
  tmp <- gsub("age onset", "Age at union formation", tmp)
  
  # Replace education
  tmp <- gsub("edu", "& Education", tmp)
  
  # Replace activity
  tmp <- gsub("act", "Activity ", tmp)
  
  # Replace settlement
  tmp <- gsub(" set", " Settlement", tmp)
  
  # Replace Income quantile
  tmp <- gsub("inc", "Income", tmp)
  tmp <- gsub("quant", "quantile", tmp)
  
  # Replace religious affiliation
  tmp <- gsub("rel", "Religiously affiliated", tmp)
  
  # Union duration
  tmp <- gsub("union", "Union", tmp)
  
  # Replace long empty sequence
  tmp <- gsub("    ", " ", tmp)
  
  #
  tmp <- gsub("^[0-9]+", "", tmp)
  
  # Period
  tmp <- gsub("period", "Period", tmp)
  
  # Remove the quotation marks
  tmp <- gsub("\"", "", tmp)
  
  # make it a data.frame
  tmp <- as.data.frame(tmp)
  
  return(tmp)
}

# Clean the tables
tables <- lapply(tables, clean_tex_table)
figures <- gsub(".tex", "_cleaned.tex", files)

# Save the files
for(i in seq_along(figures)){
  write.table(tables[[i]], figures[i], row.names = FALSE, col.names = FALSE, quote = FALSE)
}

### END #####################################