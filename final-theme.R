# 36-462: Data Mining Final
# 05/06/17
# theme for ggplot graphs

library(ggplot2)

final_theme <- theme_grey() + 
  theme(axis.text = element_text(),
        panel.grid.major = element_line(colour = "#DCDCDC"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title.y = element_text(face = "bold", family = "Times", size = 14),
        axis.title.x = element_text(face = "bold", family = "Times", size = 14),
        axis.text.x = element_text(face = "bold", family = "Times", size = 12),
        axis.text.y = element_text(face = "bold", family = "Times", size = 12),
        plot.title = element_text(color = "black", face = "bold", family = "Times", size = 16),
        legend.text = element_text(face = "bold", family = "Times"),
        legend.title = element_text(face = "bold", family = "Times"))



color_palette <- c("")