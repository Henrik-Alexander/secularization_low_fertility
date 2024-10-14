### Functions #######################
# Purpose: Write functions          #
# Author: Henrik-Alexander Schubert #
# Date: 16.06.2023                  #
# E-Mail: schubert@demogr.mpg.de    #
#####################################


### Key parameters ---------------------------------------------

# Define the periods
periodT <- 1994:1999
periodA <- 2000:2009
periodB <- 2010:2020

### Create directory structure ---------------------------------

gen_folder <- function(foldername = "Raw"){
  if(file.exists(foldername)){
    cat("The", foldername, "folder exists already! \n")
  }else{
    dir.create(foldername)
  }
}

# Create the folders
folders <- c("code", "raw", "functions", "data", "results", "figures", "notebooks")
lapply(folders, gen_folder)

#### Is not in -------------------------------------------------

`%notin%` <- Negate("%in%")

### Create a tabulate function ----------------------------------

tab <- function(...){
  tmp <- table((...), useNA = "always")
  return(tmp)
}

### Impute religiosity ------------------------------------------

# Create the data
impute_religious <- function (x) {
  
  if (sum(is.na(x)) == 0 | mean(is.na(x)) == 0) {
    
  } else {
  
  # Create vector of missings
  missings <- ifelse(is.na(x), 1, 0)
  position <- which(missings == 1)
  
  # Identify clusters
  clust_starts <- c(T, ifelse(diff(position) >= 2, 1, 0))
  cluster <- numeric(length(position))
  for (i in seq_along(cluster)) cluster[i] <- ifelse(i == 1, 1, cluster[i-1] + clust_starts[i])

  # Impute information across clusters
  for (i in unique(cluster)) {
    # Create the boundaries
    pos <- position[cluster == i]
    if (position[i] == 1) {
      # Impute the first observed value
      x[pos] <- x[which(missings == 0)[1]]
    } else {
    if (all(i == max(cluster) & length(x) == position[length(position)])) {
       x[pos] <- x[pos[1] - 1]
    } else if (x[pos[1] - 1] == x[pos[length(pos)] + 1]) {
        x[pos] <- x[pos[1] - 1]
      } else {
        # Impute half of the lenght with the first info and the second half with the other
        x[pos[1:round(length(pos)/2)-1]] <- x[pos[1]-1]
        x[pos[round(length(pos)/2):length(pos)]] <- x[ pos[length(pos)] + 1]
      }
    }
  }
  }
  return(x)
}


### File names --------------------------------------------------

# Set the paths
input <- "D:/ready-made/"

# Create a data frame with all the paths and names
datasets <- data.frame(
  paths = paste0(input, c("FOLK_perus_8800a/folk_19872000_tua_perus21tot_1.csv",
                          "FOLK_perus_0110a/folk_20012010_tua_perus21tot_1.csv", 
                          "FOLK_perus_11a/folk_20112020_tua_perus22tot_1.csv")),
  names = c("raw/folk_1987_2000.Rda", "raw/folk_2001_2010.Rda", "raw/folk_2011_2020.Rda"),
  clean = c("data/cleaned_folk_1987_2000.Rda", "data/cleaned_folk_2001_2010.Rda", "data/cleaned_folk_2011_2020.Rda"))


# Function to create a local copy of the data --------------------

load_data <- function(dataset = 1){
  
  if(dataset %notin% 1:3) stop("Dataset has to be a number betweeen 1 and 3!")
  
  if(file.exists(datasets[dataset, ]$names)){
    cat("File has been saved already!")
  }else{
    # Load the basic data
    d <- fread(datasets[dataset, ]$paths)
    
    # Save a copy of the data
    save(d, file = datasets[dataset, ]$names)
    
    # Delete the file
    rm(d)
  }
  
  # Load the dataset
  load(datasets[dataset, ]$names)
  
  return(d)
}

# Estimate the probability from the log-odds
odd_prob <- function (logodds) exp(logodds) / (1 + exp(logodds))

# Return the most frequent value
most_frequent <- function(x) x[which.max(table(x))]

# Save the plots
figs <- function(plot, plot_name, height = 15, width = 25){
  ggsave(plot, filename = paste0("figures/", plot_name, ".pdf"), height = height, width = width, unit = "cm")
}

### Function to clean the religion label ---------------------

# Function to clean the labels
clean_labels <- function(label) {
  label <- str_replace(label, "_", " ")
  label <- str_replace(label, "rel", "Religiously affiliated")
  label <- str_replace(label, "par", "(male)")
  return(label)
}


### Save figures in the figure folder ------------------------

figs <- function(plot_name, file_name, height = 15, width = 25, unit = "cm") ggsave(plot_name, filename = paste0("figures/", file_name, ".pdf"), height = height, width = width, unit = "cm")

### Create census data ---------------------------------------


merge_parent_census <- function(year_census = 1970) {
  
  # Get the census data
  dta <- cen[[paste(year_census)]]
  
  # Make parent_child data.table
  parent_child <- as.data.table(parent_child)
  
  # Get the children data
  d <- parent_child[year(parent_child$date_birth) %in% (year_census - 10:15), ]
  
  # Filter the census data for parents that gave birth to 
  # the cohort born in 1955-1960
  dta <- dta[!is.na(id), ]
  
  # Merge the census with the child id
  dta <- dta[d[, .(id, cid)], on = "id"]
  
  years_t <- 1995:1999
  years_a <- 2000:2009
  years_b <- 2010:2022
  
  # Get the children's income (Train set)
  children <- basic[id %in% d$cid & year >= 1995, .(age, inc, sex, id, year, yob)]
  names(children) <- paste0(names(children), "_child")
  
  # Create suffix for the parents names
  dta <- dta[, .(id, cid, year, yob, occ, act, une_dur, emp_dur, sec, inc, occ_label, edu_field, edu, nro, mar, sex, ses)]
  names(dta) <- paste0(names(dta), "_parent")
  
  # Merge the parent's information and children information
  tmp <- dta[children, on = c("cid_parent" = "id_child"), allow.cartesian = TRUE]
  
  # Filter data that hos no na on the cid
  tmp <- tmp[!is.na(cid_parent), ]
  
  ### Pivot wider to create mother and father data
  
  # Rename child variable
  setnames(tmp, "cid_parent", "cid")
  
  # Remove the parent name
  names(tmp) <- str_replace(names(tmp), "_parent", "")
  
  # Create mother
  mot <- tmp[sex == "Female", ]
  fat <- tmp[sex == "Male", ]
  unk <- tmp[is.na(sex), ]
  
  # Combine the datasets
  par <- merge(mot, fat, by = c("cid", "inc_child", "sex_child", "year_child", "age_child", "yob_child"), suffixes = c("_mot", "_fat"), all = T)
  
  # Remove sex of the parent variable
  par[, sex_mot := NULL]
  par[, sex_fat := NULL]
  
  # Estimate age at socialisation (parent interview)
  par[, age_soc := year_mot - yob_child]
  
  return(par)
}

### Create dummy data ------------------------------

dummy.data.frame <- function(data) {
  # Make data table
  data <- as.data.table(data)
  # Make only factor variables to dummies
  factors <- sapply(data, is.factor)
  characters <- sapply(data, is.character)
  factors <- ifelse(factors == TRUE | characters == TRUE, TRUE, FALSE)
  factors[names(data) == "id"] <- FALSE
  nm_fact <- names(data)[factors]
  nm_non <- names(data)[!factors]
  if (sum(factors) == 0) {
    df <- data
  } else {
    # Split the data into permanent and temporary
    df <- data[, ..nm_non]
    # Create the dummies
    for (nm in nm_fact) {
      tmp <- data %>% pull(nm)
      values <- tmp %>% unique()
      if (length(values) <= 2) {
        labs <- names(df)
        df <- bind_cols(df, ifelse(tmp == values[1], 1, 0))
        names(df) <- c(labs, values[1])
      } else {
        labels <- paste0(nm, "_", values)
        dummies <- vector("list", length = length(values))
        names(dummies) <- labels
        # Fill the values
        for (i in seq_along(values)) dummies[[i]] <- ifelse(tmp == values[i], 1, 0)
        df <- bind_cols(df, bind_cols(dummies))
      }
    } 
  }
  # Clean the data
  df <- as_tibble(df)
  df <- df %>% janitor::clean_names() %>%
    select(-id) %>% 
    mutate(across(everything(), as.numeric))
  return(df)
}


### END ##############################################3