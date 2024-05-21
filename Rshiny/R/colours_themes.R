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
    axis.title = element_text(colour = "#484537", 
                              face="bold"),
    axis.title.y = element_text(vjust = +3), 
    axis.title.x = element_text(vjust = -2.5),
    axis.ticks = element_line(colour = "#484537"),
    # legend at the bottom 6)
    legend.position = "right", 
    legend.title = element_text(face="bold", colour="#484537"),
    legend.text = element_text( colour="#484537"),
    # title
    plot.title = element_text(colour="#484537", face="bold"),
    # plot margins 
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
  )
}


# 09/04/2024: changed legend title to bold + colour same as rest 
# 09/04/2024: added plot margins and margins between axis label & axis text 


#### Setting bird colours 
    
    # These are for the bto data 
          
          # Set bird colours
          # in alphaetical order the birds are: blue tit, coal tit, crested tit, great tit, marsh tit, willow tit 
          colours_all_bto<-c("azure3", "bisque4", "black", '#ffcc00', 'burlywood3', "darkolivegreen3" )
          # exlcuding the crested tit, which has often not got enough data 
          colours_exCrest_bto<-c("azure3", "bisque4", '#ffcc00', 'burlywood3', "darkolivegreen3" )
          
          
          
    #### MOnths for plottin g
    
      months_aug_start<-c("Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul")
      
      

  