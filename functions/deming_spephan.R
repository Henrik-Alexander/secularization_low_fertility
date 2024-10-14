

# Transformation
step1.1 <- function(table, target_row) table * target_row / rowSums(table)
step1.2 <- function(table, target_column) table * target_column / rowCols(table)

# Estimate the error
