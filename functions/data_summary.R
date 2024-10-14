summarise_numeric <- function(x) {
  c("N" = length(x),
    "Mean" = round(mean(x), 3),
    "St. Dev." = round(sd(x), 3),
    "Min" = min(x),
    "Max"  = max(x))
}

summarise_factor <- function(x) {
  levels <- unique(x)
  count <- table(x)
  share <- round(prop.table(count) * 100, 3)
  c("levels" = levels,
    "Count" = count,
    "Share" = share)
}

summarise_data <- function(x) {
  if (is.numeric(x)){
    summarise_numeric(x)
  } else {
    summarise_factor(x)
  }
}
