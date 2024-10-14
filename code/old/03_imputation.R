### Impute information

# Create a sequence
life <- rep(c(rep(0, 10), rep(NA, 3), 1, 1, rep(NA, 3), 1), 2)
life <- ifelse(life == 1, "religious", "non-religious")
x <- life

# Imputation function
impute_info <- function (x) {
  missing <- ifelse(is.na(x), 1, 0)
  positions <- which(missing == 1)
  # Identify clusters
  clust_starts <- c(TRUE, ifelse(diff(positions) >= 2, 1, 0))
  cluster <- numeric(length(positions))
  for (i in seq_along(cluster))   cluster[i] <- ifelse(i == 1, 0, cluster[i-1]) + clust_starts[i]

  # Impute the information for the different clusters
  for (i in unique(cluster)) {
    
  # Create the boundaries
  pos <- positions[cluster == i]
  first <- pos[1]
  last <- pos[length(pos)]
  len <- last - first + 1
  
    # Do the proper imputations
    if (x[first-1] == x[last+1]) {
      x[pos] <- x[first-1]
    } else {
      # Impute half of the length with the first and the other half with the last
     x[pos[1:round(len/2)-1]] <- x[first-1]
     x[pos[round(len/2):len]] <- x[last+1]
    }
  }
  return(x)
}

impute_info(life)
