vera_theme <- function() {
  theme(
    # add border 1)
    panel.border = element_rect(colour = "darkgrey", fill = NA, linetype = 1),
    # color background 2)
    panel.background = element_rect(fill = "ivory"),
    # modify grid 3)
    panel.grid.major.x = element_line(colour = "darkgrey", linetype = 3, size = 0.5),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y =  element_line(colour = "darkgrey", linetype = 3, size = 0.5),
    panel.grid.minor.y = element_blank(),
    # modify text, axis and colour 4) and 5)
    axis.text = element_text(colour = "#484537", face = "italic"),
    axis.title = element_text(colour = "#484537", face="bold"),
    axis.ticks = element_line(colour = "#484537"),
    # legend at the bottom 6)
    legend.position = "right", 
    # titl e
    plot.title = element_text(colour="#484537", face="bold")
  )
}

