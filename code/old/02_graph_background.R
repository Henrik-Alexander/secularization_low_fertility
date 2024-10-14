#########################################
# Purpose: Create graph for background  #
# Author: Henrik-Alexander Schubert     #
# Date: 02.12.2023                      # 
# E-mail: schubert@demogr.mpg.de        #
#########################################


# Load the packages
library(tidyverse)

### Functions ----------------------------------------

# set theme
theme_set(theme_test(base_size = 14, base_family = "serif"))
theme_update(plot.margin = margin(0.2, 0.4, 0.1, 0.1, "cm"),
             panel.grid.major.y = element_line(colour = "grey80", linewidth = 0.5, linetype = "dotted"),
             panel.grid.major.x = element_line(colour = "grey80", linewidth = 0.5, linetype = "dotted"),
             panel.grid.minor.x = element_blank(),
             panel.grid.minor.y = element_blank(),
             legend.background = element_rect(fill = "white", colour = "white"),
             legend.title = element_text(face = "bold"),
             axis.title.x = element_text(face = "bold", size = 14),
             axis.title.y = element_text(face = "bold", size = 14),
             legend.position = "bottom"
)


### Data preperation ---------------------------------

# Cateogorise the groups
christians <- c("CHRISTIANITY", "Christian Community of Finland", "Liberal Catholic Church", "Evangelical Lutheran free congregations", "Other Christian", "Evangelical Lutheran Church of Finland")

# Get the files
date <- "20231202"
files <- list.files("data", pattern = date, full.names = T)

# Load the data
tfr <- read.csv(files[2], skip = 3, sep = ",", header = F)

# Clean the fertility data
names(tfr) <- c("Year", "TFR")

### Plot the TFR
ggplot(tfr, aes(x = Year, y = TFR)) +
  geom_line() +
  scale_y_continuous("Total Fertility Rate",expand = c(0, 0), limits = c(1, 6), breaks = scales::breaks_pretty(n = 12)) +
  scale_x_continuous(expand = c(0, 0.2), breaks = scales::breaks_pretty(n = 10))
ggsave(last_plot(), filename = "figures/tfr_fin_ts.pdf", height = )

### Prepare the religion data ----------------------------------

# Load the pop data
pop <- read.csv(files[1], skip = 3, header = F, quote = "")

# Clean the population data
names(pop) <- c("Religion", "Age", "Year", "Males", "Females")
pop <- pop %>% 
  mutate(across(everything(), ~ str_remove_all(.x, pattern = "\"")),
         across(c(Year, Males, Females), as.numeric))

# Regroup religions
pop <- pop %>% 
  mutate(Religion = ifelse(Religion %in% c("CHRISTIANITY", "PERSONS NOT MEMBERS OF ANY RELIGIOUS COMMUNITY", "Evangelical Lutheran Church of Finland"), Religion, "Other religion"),
         Religion = case_when(Religion == "CHRISTIANITY" ~ "Other christians",
                              Religion == "PERSONS NOT MEMBERS OF ANY RELIGIOUS COMMUNITY" ~ "Not religious",
                              Religion == "Other religion" ~ "Other religions",
                              Religion == "Evangelical Lutheran Church of Finland" ~ "Church of Finland")) %>% 
  group_by(Religion, Year) %>% 
  summarise(across(c(Males, Females), sum), .groups = "drop")

  
# Estimate things
pop <- pop %>% 
  mutate(total = Males + Females,
         sex_ratio = Males / Females) %>% 
  group_by(Year) %>% 
  mutate(share = total / sum(total))


# Plot the results
ggplot(pop, aes(x = Year, y = share, colour = Religion, group = Religion, shape = Religion)) +
  geom_line() +
  geom_point(size = 3) +
  scale_colour_brewer(palette = "Set1") +
  scale_y_continuous("Populatution Share", labels = scales::percent, expand = c(0, 0), limits = c(0, 0.5)) +
  scale_x_continuous(expand = c(0, 0.2), breaks = scales::pretty_breaks()) +
  scale_shape_manual(values = c(15, 17, 18, 19))
ggsave(last_plot(), filename = "figures/share_rel_statfin.pdf", height = 12, width = 20, unit = "cm")


# Plot the sex ratio
ggplot(pop, aes(x = Year, y = sex_ratio, colour = Religion, group = Religion, shape = Religion)) +
  geom_line() +
  geom_point(size = 3) +
  geom_hline(yintercept = 1) +
  scale_colour_brewer(palette = "Set1") +
  scale_y_continuous("Sex Ratio (log-scale)", n.breaks = 10,expand = c(0, 0.02), trans = "log10") +
  scale_x_continuous(expand = c(0, 0.2), breaks = scales::pretty_breaks()) +
  scale_shape_manual(values = c(15, 17, 18, 19)) +
  guides(colour = guide_legend(nrow = 2))
ggsave(last_plot(), filename = "figures/sexratio_rel.pdf", height = 12, width = 20, unit = "cm")


### Combine the data --------------------------------------------

# Select the data
pop_nr <- pop %>% filter(Religion == "Not religious")
pop_nr$share <- 1 - pop_nr$share
tfr <- tfr %>% filter(Year >= 1990)

# Colours
fertilitycolour = "#5e3c99"
popcolour = "#fdb863"

# Coeff
coeff <- .53

# Plot together
ggplot(tfr, aes(x = Year, y = TFR)) +
  geom_line(colour = fertilitycolour, size = 2) +
  geom_line(data = pop_nr, aes(y = share/coeff), colour = popcolour, size = 2) +
  #geom_point(colour = fertilitycolour) +
  #geom_point(data = pop_nr, aes(y = share), colour = popcolour) +
  scale_y_continuous("Total Fertility Rate", expand = c(0.05, 0), 
                     breaks = scales::pretty_breaks(n = 5), 
                     sec.axis = sec_axis(~.*coeff, name="Share religiously affiliated", labels = scales::percent, breaks = scales::pretty_breaks(n = 5))) +
  scale_x_continuous(expand = c(0, 0), breaks = scales::pretty_breaks()) +
  theme(axis.title.y = element_text(color = fertilitycolour, size=13),
        axis.title.y.right = element_text(color = popcolour, size=13)) +
  labs(caption = "Source: Data from Statistics Finland (2024).") +
  annotate(geom = "text", x = 2011, y = 1.85,
           label = "Total Fertility Rate", hjust = 0, family = "serif", colour = fertilitycolour, fontface = "bold", size = 6) +
  annotate(geom = "text", x = 2003, y = 1.65,
           label = "Share\n religiously affiliated", hjust = 0, family = "serif", colour = popcolour, fontface = "bold", size = 6)

ggsave(last_plot(), filename = "figures/tfr_pop.pdf", height = 12, width = 20, unit = "cm")

### END #####################################################