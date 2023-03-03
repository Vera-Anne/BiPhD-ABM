# functions for the hoarding model 
# Vera Vinken 
# March 2023 


##############################
#      load packages         #
##############################
load_packages<-function(){
  library(usethis)
  library(devtools)
  library(truncnorm)
  library(pracma)
  library(ggplot2)
  library(plotly) # for 3D surface plot 
  library(rgl)
  library(plot3D)
  library(htmlwidgets)
  library(webshot)
  library(withr)
  library('plyr')
  library('gridExtra')
  library(grid)
  library(lattice)
  library(dplyr)
  library(data.table)
  library(tidyverse)
  library(viridis)
  library(hrbrthemes)
}