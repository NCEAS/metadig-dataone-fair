#This theme is a modified version of the the Arctic Data Center's "theme_ADC" housed at:
#https://github.nceas.ucsb.edu/KNB/arctic-data/blob/master/reporting/R/theme_ADC.R

theme_ADC_modified <-
  theme_bw(base_size = 12, base_family = "Helvetica") +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold",
      margin = margin(10, 0, 10, 0),
      color = "#1D244F"
    ),
    plot.subtitle = element_text(
      size = 14,
      margin = margin(0, 0, 10, 0),
      color = "#1D244F"
    ),
    axis.text.x = element_text(
      angle = 50,
      size = 16,
      vjust = 0.5,
      color = "#1D244F"
    ),
    axis.text.y = element_text(size = 18, color = "#1D244F"),
    axis.title.x = element_text(
      color = "#1D244F",
      vjust = -.5,
      size = 20
    ),
    axis.title.y = element_text(
      color = "#1D244F",
      angle = 90,
      vjust = .5,
      size = 20
    ),
    panel.background = element_rect(fill = "white"),
    axis.line = element_line(color = "#1D244F"),
    panel.grid.major = element_line(colour = "gray", size = 0.01),
    panel.grid.minor = element_line(colour = "gray", size = 0.04),
  ) +
  theme(legend.position='top', 
        legend.justification='left',
        legend.direction='horizontal',
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.title = element_text(size=15),
        legend.text = element_text(size=14))