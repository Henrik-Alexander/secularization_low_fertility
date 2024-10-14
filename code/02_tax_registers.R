#############################################
# Project: Secularization and low fertility
# Purpose: Tax registers and church membersh.               
# Author: Henrik-Alexander Schubert         
# Date: 16.09.2023                          
# E-Mail: schubert@demogr.mpg.de            
#############################################

rm(list = ls())

# Load the packages
library(data.table)
library(tidyverse)

# Load the functions
source("functions/functions.R")
source("functions/graphics.R")

# Load the basic data
"data/basic.Rda"

#### Load the data ----------------------------------------

# Datasets
data_path <- "D:/ready-made/"
data1 <- "FOLK_tulo_11a/folk_20112019_tua_tulo21tot_1.csv"
data2 <- "FOLK_tulo_0110a/folk_20012010_tua_tulo21tot_1.csv"
data3 <- "FOLK_tulo_8800a/folk_19872000_tua_tulo21tot_1.csv"
datasets <- c(data1, data2, data3)

# Create a vector with the important variables
oldnames <- c("vuosi", "shnro", "palk", "lkuve", "lkive")
newnames <- c("year", "id", "income", "mun_tax", "church_tax")

# Create a result container
data <- vector("list", length = length(datasets))

# Load the data for the different years
for (i in seq_along(datasets)) {
    
  cat("Iteration", i, "out of", length(datasets), "\n")
  
  # Load the data
  d <- fread(paste0(data_path, datasets[i]))
  
  # Select variables
  d <- d[, ..oldnames]
  
  # Rename variables
  setnames(d, oldnames, newnames)
  
  # Create an indicator if a person pays church tax
  d[, religious := fcase(
        church_tax == 0 & mun_tax > 0, 0,
        church_tax > 0, 1)]
                          
  # Assign the result
  data[[i]] <- d
  rm(d)

}

# Combine the result
religious <- rbindlist(data)
rm(data)

### Estimate the tax rate per region -----------------------

# Load the data
load("data/basic.Rda")
basic <- basic[, .(year, id, res, act)]

# Filter the data
tax_rate <- basic[religious, on = c("year", "id")]
tax_rate <- tax_rate[!is.na(mun_tax) & mun_tax > 0 & church_tax > 0 & !is.na(income) & church_tax < income, ]

# Estimate the proportion of tax_rate per region
tax_rate[, church_tax_rate := church_tax / income]
tax_rate <- tax_rate[, .(church_tax_rate = 100 * mean(church_tax_rate)), by = .(res, year)]
tax_rate <- tax_rate[order(res, year), ]

# Save the data
save(tax_rate, file = "data/res_tax_rate.Rda")


### Impute the religiosity -----------------------------------------

# Number of missings
miss_or <- sum(is.na(religious$religious))
missing_trend <- religious[, .(share = mean(is.na(religious))), by = year] %>% 
  ggplot(aes(x = year, y = share)) +
  geom_line() +
  scale_x_continuous("Year", expand = c(0, 0), breaks = scales::pretty_breaks()) +
  scale_y_continuous("Share missing religious", expand = c(0, 0),labels = scales::percent)
figs(missing_trend, "missing_rel")

### Impute the religious data ------------------------------

# Order the data by id and year
religious <- religious[order(id, year), ]

# Split by id
chunks <- 100
ids <- unique(religious$id)
positions <- round(seq(1, length(ids), length.out = chunks))
imputed <- vector("list", length = chunks-1)

# Impute in chunks - using harddrive to reduce impact on memory
for (i in 2:chunks) {
  tmp <- religious[id %in% ids[positions[i-1]:positions[i]], ]
  tmp[, religious := impute_religious(religious), by = id]
  save(tmp, file = paste0("data/imp_", i-1, ".Rda"))
  rm(tmp)
}

# Remove the long dataset
rm(religious)

# Combine the data
for (i in 1:(chunks-1)) {
  load(paste0("data/imp_", i, ".Rda"))
  imputed[[i]] <- tmp
  rm(tmp)
}

# Change the format
religious <- rbindlist(imputed)

# Delete the temporary files
file.remove(list.files(path = "data", pattern = "imp_", full.names = T))

# Save the imputed data
save(religious, file = "data/religious.Rda")

# Display the result
miss_imp <- sum(is.na(religious$religious))
cat("Imputed values:", (miss_or - miss_imp) / miss_or)


### Explorative analysis ---------------------------------

# Plot the share of state church tax payers over time
religious[, .(share = mean(religious)), by = year] %>% 
  ggplot(aes(x = year, y = share)) +
    geom_point(size = 2) +
    geom_line() + 
    ylab("Share religious people") +
    xlab("Year") 


# Create income quantiles data
inc_quant <- religious[, .(
                  quant_1 = quantile(income, probs = 0.25, na.rm = T),
                  quant_2 = quantile(income, probs = 0.5, na.rm = T),
                  quant_3 = quantile(income, probs = 0.75, na.rm = T),
                  quant_4 = quantile(income, probs = 1, na.rm = T)), by = year]

# Save the income quantiles
save(inc_quant, file = "data/income_quantiles.Rda")

# Plot the share of religious people by income group over time
religious[, .(share = mean(religious, na.rm = T)), by = .(year)] %>% 
  ggplot(aes(x = year, y = share)) +
    geom_col() +
    scale_x_continuous(expand = c(0, 0), n.breaks = 10) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1), labels = scales::percent_format(accuracy = 1L)) +
    ylab("Proportion religiously affiliated (%)") +
    xlab("Year")

# Save the plot
ggsave(last_plot(), filename = "figures/share_rel_pop.pdf")

### State church tax ============================

# Filter the data
d <- religious[church_tax > 0 & income > 0]

# Estimate the quantiles
qu <- quantile(d$income)

# Estimate quantile threshholds
d[, quantile := fcase(income < qu[2], 1, income >= qu[2] & income < qu[3], 2,
                      income >= qu[3] & income < qu[4], 3, income >= qu[4], 4) ]

# Estimate the church tax
d <- d[, .(churchtax = mean(church_tax), income = round(mean(income))), by = .(year, quantile)]

# Plot the result
ggplot(subset(d, year == 2019), aes(x = as.factor(quantile), y = churchtax, fill = quantile)) +
  geom_col()

### END ###############################################