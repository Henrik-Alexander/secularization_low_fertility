### Graphics ########################
# Purpose: Create the graphic style #
# Author: Henrik-Alexander Schubert #
# Date: 16.06.2023                  #
# E-Mail: schubert@demogr.mpg.de    #
#####################################

# Colors
colors <- c( "#e66101", "#fdb863", "#b2abd2", "#5e3c99")
shapes <- c(15, 16, 17, 18)

# Set theme
theme_set(theme_test(base_size = 16, base_family = "serif"))
theme_update(plot.margin = ggplot2::margin(0.2, 0.4, 0.2, 0.2, "cm"),
             panel.grid.major.y =  element_line(linetype = "dotted", size = 0.5, colour = "grey"),
             panel.grid.major.x =  element_line(linetype = "dotted", size = 0.5, colour = "grey"),
             panel.grid.minor.y =  element_blank(),
             panel.grid.minor.x =  element_blank(),
             legend.background = element_rect(fill = "white"),
             legend.title =  element_text(face =  "bold"),
             axis.title.y =  element_text(size =  14, face =  "bold"),
             axis.title.x =  element_text(size =  14, face =  "bold"),
             plot.title.position = "plot",
             title = element_text(face = "bold"),
             legend.position = "bottom",
             strip.background = element_blank(),
             strip.text = element_text(face = "bold", size = 15),
             panel.spacing = unit(0.8, "cm"),
             legend.margin = margin(0, 0, 0, 0)
)

### END ##########################################################