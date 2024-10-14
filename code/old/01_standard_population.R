##########################################
# Purpose: Estimate standard population  #
# Author: Henrik-Alexander Schubert      #
# Date: 09.11.2023                       #
# E-Mail: schubert@demogr.mpg.de         #
##########################################

# Load the packages
library(tidyverse)
library(data.table)

# Load the data
sta <- fread("data/001_11rd_2022_20231109-172703.csv", skip = 3)

### Transform the data ---------------------------

# Create lables
names(sta) <- c("id", "cat", 1972:2022)
sta$sex <- str_extract(sta$id, pattern = "[A-Z][a-z]+")
sta$age <- as.numeric(str_extract(sta$id, pattern = "[0-9]+"))
sta$`2022` <- as.integer(str_extract(sta$`2022`, "[0-9]+"))

# Pivot the data
sta <- sta %>% 
  pivot_longer(cols = matches("[0-9]"), values_to = "Pop", names_to = "Year")

# Plot the standard
sta %>% 
  filter(Year == 2018) %>% 
  mutate(Pop = ifelse(sex == "Males", -Pop, Pop)) %>% 
  ggplot(aes(x = age, y = Pop, fill = sex)) +
  geom_col() +
  coord_flip() +
  annotate(geom = "text", x = 90, y = 22000, label = "Females", colour = "#E41A1C", family = "serif", size = 10) +
  annotate(geom = "text", x = 90, y = -22000, label = "Males", colour = "#377EB8", family = "serif", size = 10) +
  scale_x_continuous("Age", expand = c(0, 0), n.breaks = 10) +
  scale_y_continuous('Population', expand = c(0, 0), n.breaks = 10, labels = abs) +
  scale_fill_brewer(palette = "Set1") +
  theme_bw(base_size = 16, base_family = "serif") +
  guides(fill = "none") +
  theme(panel.spacing.x = unit(1, "cm"))
# Save the last_plot
ggsave(last_plot(), filename = "figures/pop_full_2018.pdf")

# Plot the standard
sta %>% 
  filter(Year == 2018 & age >= 18 & age < 55) %>% 
  group_by(Year, sex) %>% 
  mutate(total = sum(Pop)) %>% 
  group_by(Year, sex, age) %>%
  summarise(share = Pop / total) %>% 
  ggplot(aes(x = age, y = share, fill = sex)) +
  geom_col() +
  facet_wrap(~ sex) +
  coord_flip() +
  scale_x_continuous("Age", expand = c(0, 0), n.breaks = 10) +
  scale_y_continuous('Weight (x)', expand = c(0, 0)) +
  scale_fill_brewer(palette = "Set1") +
  theme_bw(base_size = 16, base_family = "serif") +
  guides(fill = "none") +
  theme(panel.spacing.x = unit(1, "cm"))

# Save the last_plot
ggsave(last_plot(), filename = "figures/standard_pop_2018.pdf")

### END ############################################