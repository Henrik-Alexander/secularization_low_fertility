#########################################
# Project: Secularization and fertility #
# Purpose: Creating the basic data      #
# Name: Henrik-Alexander Schubert       #
# Date: 05.07.2023                      #
# E-mail: schubert@demogr.mpg.de        #
# Pre-requisites: functions             #
#########################################

# Load the packages
library(tidyverse)
library(data.table)

# Load the functions and packages
source("functions/functions.R")
source("functions/graphics.R")

# Functions ---------------------------------------

# Function to create a local copy of the data --------------------

load_data <- function(dataset = 1){
  
  if(dataset %notin% 1:3) stop("Dataset has to be a number betweeen 1 and 3!")
  
  if(file.exists(datasets[dataset, ]$names)){
    cat("File has been saved already!\n")
    
    # Load the dataset
    load(datasets[dataset, ]$names)
    
  }else{
    # Load the basic data
    d <- fread(datasets[dataset, ]$paths)
    
    # Save a copy of the data
    save(d, file = datasets[dataset, ]$names)
  }
  return(d)
}


### Load the data sets -----------------------

# Create the variable vector
# Select the important variables
vars <- c("vuosi", "shnro", "ika", "syntyv", "kuolv", "sukup", # Generel
        "kturaha_k", "ptoim1", "ammattikoodi_k", "sose",  # Socio-economic status and employment
        "tyke", "tyokk", # Unemployment and employment
        "ututku_aste", "suorv",  # Education
        "kunta", "maka",  "skunta", # Region
        "sivs", "lkm_k",
        "syntyp2", "kansa1_k", "svaltio_k") 

# Create a contianer
data <- vector("list", length = 3)

for(i in 1:3){

cat("Round", i, "out of ", 3, "!")

# First group of years
d <- load_data(i)

# Select the variables
d <- d[, ..vars]

# Rename the variables
d <- d[ , .(year = vuosi,
            id = shnro,
            age = ika,
            yob = syntyv,
            yod = kuolv,
            bpl = skunta,
            res = kunta,
            mar = sivs,
            inc = kturaha_k,
            nch = lkm_k,
            sex = sukup,
            act = ptoim1,
            cob = svaltio_k,
            edu = ututku_aste,
            ses = sose,
            occ_code = ammattikoodi_k,
            edu_end = suorv,
            set = maka,
            ori = syntyp2,
            nat = kansa1_k)]

# Filter only Finnish born
d <- d[ori %in% c(11, 12), ]

# sex: Create a variable for sex
d[ , sex := (ifelse(sex == 1, "Male", "Female"))] 

# Create a variable for being born in Finland
d[ , bif := (ifelse(cob == 1, 1, 0))] 

# Recode the SES variable
d[, ses := factor(fcase(
  ses %in% 10:20, 1,
  ses %in% 31:34, 2,
  ses %in% 41:44, 3,
  ses %in% 51:59, 4,
  ses ==  70, 5,
  ses == 60, 6,
  ses >= 80, 7
),
labels = c("Self-employed", "Senior clerical staff", "Junior clerical staff", 
           "Workers", "Pensioners", "Students", "other")) ]

# Create activity status
d[ , act := factor(fcase(act == 11, 1,
                         act == 12, 2,
                         act == 21, 3,
                         act == 22, 4,
                         act %in% c(24, 25, 29, 99), 5),
                   labels = c("employed", "unemployed", "age 0-14", "education", "other"))]

# Recode the settlement structure
d[, set := factor(fcase(
                        set == "K1", 1,
                        set %in% c("K2", "K3", "M4", "M5"), 2,
                        set %in% c("M6", "M7"), 3),
                  labels = c("urban", "intermediate", "rural"), ordered = T)]

# Create missings
d[, edu := factor(fcase(
  is.na(edu), 1,
  edu %in% c(3, 4), 2,
  edu %in% c(5, 6, 7, 8), 3),
  labels = c( "Education: basic", "Education: medium", "Education: high"), ordered = T)]

# Clean the marriage variable
d[, rel := factor(fcase(
  mar == 1, "unmarried",
  mar == 2, "married",
  mar == 4, "divorced",
  mar == 5, "widowed"
))]

# Estimate the income quantile
d[, inc_quant := factor(fcase(inc < quantile(inc, probs = 0.25, na.rm = T), 1,
                           inc < quantile(inc, probs = 0.5, na.rm = T)  & inc >= quantile(inc, probs = 0.25, na.rm = T), 2,
                           inc < quantile(inc, probs = 0.75, na.rm = T) & inc >= quantile(inc, probs = 0.5, na.rm = T), 3,
                           inc >= quantile(inc, probs = 0.75, na.rm = T), 4)), by = year]

### Education field ----------------------------------

# Create missings
d$edu_field <- ifelse(d$edu_field == "9", NA, d$edu_field)

data[[i]] <- d 
rm(d)

}

# Combine the data
basic <- rbindlist(data)

# Save the basic data
save(basic, file = "data/basic.Rda")

### END ################################################