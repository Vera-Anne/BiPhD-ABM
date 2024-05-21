
############### DATA EXTRACTION ABM - SHINY APP ###################

## For: Workshop: Shiny futures. 

## By: Vera Vinken

## Date: 20/05/2022

## See completed app (but not code) at: 

###### SCRIPT ##############

# LIBRARIES
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)


#### HOUSEKEEPING ###

# Clear workspace
rm( list = ls())

# Data

# Load the default data from the .rda file
# I've put this in a folder named "data", as I think this is what Shiny wants for uploading
load("data/results.rda") 


###############################################
# UI - USER INTERFACE
###############################################

ui <- fluidPage(
  checkboxGroupInput("variable", "Variables to show:",
                     c("Cylinders" = "cyl",
                       "Transmission" = "am",
                       "Gears" = "gear")),
  tableOutput("data")
)

ui <- fluidPage(
  h1("Title of my app"),
  sidebarLayout(
    # Select what you want in the slide panel 
    sidebarPanel (
      checkboxGroupInput( "models" , "Pick which models to show:",
                          c("1.1" = 11, 
                            "1.2" = 12,
                            "1.3.1" = 131, 
                            "1.3.2" = 132))),
    # Print what needs to be shown 
    mainPanel(
      textOutput("models_chosen"), 
 
    )
    
  )
)





##################################################
# SERVER 
##################################################

server <- function(input, output) {
  
  # vv: dummies to run with 
  # 
  #   input = list(
  #   xax1 = "Rear axle ratio",
  #   secs = 16
  #   )
  
  
  output$models_chosen = renderText( {
    print(paste0("the models chosen are: "), 
                    input$models)
  })
  
  
  
}


shinyApp(ui, server)
