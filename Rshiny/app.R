
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
library(plotly)


#### HOUSEKEEPING ###

# Clear workspace
rm( list = ls())




###############################################
# UI - USER INTERFACE
###############################################


ui <- fluidPage(
  h1("Title of my app"),
  sidebarLayout(
    # Select what you want in the slide panel 
    sidebarPanel (
      checkboxGroupInput( "models_to_show" , "Pick which models to show:",
                          c("1.1 - Non hoarding - SC" = 11, 
                            "1.2 - Leftover hoarding - SC" = 12,
                            "1.3.1 - Direct hoarding (H top) - SC" = 131, 
                            "1.3.2 - Direct hoarding (R top) - SC" = 132,
                            "2.1 - Non hoarding - Stomach content" = 21, 
                            "2.2 - Leftover hoarding - FR" = 22,
                            "2.3.1 - Direct hoarding (H top) - FR" = 231, 
                            "2.3.2 - Direct hoarding (R top) - FR" = 232,
                            "3.1 - Non hoarding - FCR" = 31, 
                            "3.2 - Leftover hoarding - FCR" = 32,
                            "3.3.1 - Direct hoarding (H top) - FCR" = 331, 
                            "3.3.2 - Direct hoarding (R top) - FCR" = 332,
                            "4.1 - Non hoarding - FR & FCR" = 41, 
                            "4.2 - Leftover hoarding - FR & FCR" = 42,
                            "4.3.1 - Direct hoarding (H top) - FR & FCR" = 431, 
                            "4.3.2 - Direct hoarding (R top) - FR & FCR" = 432,
                            "5.1 - Non hoarding - Stomach content" = 51, 
                            "5.2 - Leftover hoarding - SC & FR" = 52,
                            "5.3.1 - Direct hoarding (H top) - SC & FR" = 531, 
                            "5.3.2 - Direct hoarding (R top) - SC & FR" = 532,
                            "6.1 - Non hoarding - FCR" = 61, 
                            "6.2 - Leftover hoarding - SC & FCR" = 62,
                            "6.3.1 - Direct hoarding (H top) - SC & FCR" = 631, 
                            "6.3.2 - Direct hoarding (R top) - SC & FCR" = 632)),
    checkboxGroupInput("environments_to_show", "Pick which environments to show (CURRENTLY ONLY PICK 1):", 
                       c("Average over all environments" = 1, 
                         "12 Environments seperate" = 12)),
    checkboxGroupInput("phys_var_to_show", "Pick which physiological variable to show (CURRENTLY ONLY PICK 1):", 
                       c("Stomach content" = "sc", 
                         "Fat reserve" = "fr", 
                         "Fat change rate" = "fcr"))),
    # Print what needs to be shown 
    mainPanel(
      textOutput("models_chosen_text"), 
      textOutput("env_chosen_text"), 
      plotOutput("survplot")
    )
  )
)



##################################################
# SERVER 
##################################################

server <- function(input, output) {
  
  # vv: dummies to run with 

    # input = list(
    # environments_to_show = 1,
    # models_to_show = c(11, 232, 41)
    # )
    # 
  
  # Data
  
  # Load the default data from the .rda file
  # I've put this in a folder named "data", as I think this is what Shiny wants for uploading
  load("data/results.rda") 
  load("data/Esum_results.rda")
  

  
    # survival plot 
    output$survplot = renderPlot({
      
      # Filter out the data for the survival plot 
      
      # Filter for the correct environment setting 
      
      if(input$environments_to_show == 1){
        # what needs to happen for only 1 average survival 
        
        # Set the current survival data to means only 
        surv_dat <- df_out_Esum
        
      } else if (input$environments_to_show ==12){
        # What needs to happen for all 12 environments 
        
        # Set the current survival data to all 12 environments seperate 
        surv_dat<-df_out
      }
      
      # Filter for the correct models as given by input & select survival only 
      surv_dat<-surv_dat%>%
        filter(model %in% input$models_to_show)%>%
        filter(id == "alive")
 
    # Prepare the survival plot 
    if(input$environments_to_show == 1){
      # plot 1 graph with the survival means. 
      p<-ggplot(surv_dat, aes(x=timestep, y=mean_val_env))+
        geom_line(aes(col=model))+
        #facet_wrap(.~env_id,  nrow=6)+
        #ggtitle(label=paste(int_var, "- Variable per environment type" ))+
        theme(plot.title=element_text(face='bold'), 
              axis.title.x = element_text( face='bold' ), 
              axis.title.y=element_text(face='bold'), 
              strip.text=element_text(), 
              legend.text=element_text(), 
              legend.title=element_blank())+
        ggtitle("Survivalplot")+
        #scale_color_manual(values=colours)+
        labs(fill=" ")
    } else if(input$environments_to_show == 12){
      # plot 12 graphs iwth the survivals 
      p<-ggplot(surv_dat, aes(x=timestep, y=value))+
        geom_line(aes( col=model))+
        facet_wrap(.~env,  nrow=6)+
        #ggtitle(label=paste(int_var, "- Variable per environment type" ))+
        theme(plot.title=element_text(face='bold'), 
              axis.title.x = element_text( face='bold' ), 
              axis.title.y=element_text(face='bold'), 
              strip.text=element_text(), 
              legend.text=element_text(), 
              legend.title=element_blank())+
        ggtitle("Survivalplot")+
        #scale_color_manual(values=colours)+
        labs(fill=" ")
    }
    
    #p<-ggplotly(p)
    #p%>% layout(legend = list(title=list(text='<b>Model ID<b>')))
    
    # print plot to output
    p
    
    })
  
    # TIME FOR OUTPUT 
        # Just a temp testing text to make sure modesl are chosen correctly 
        output$models_chosen_text = renderText( {
          print(paste0("the models chosen are: ", 
                         paste(input$models_to_show, collapse = ";")))
        })
        # And for the environments 
        output$env_chosen_text = renderText( {
          print(paste0("the environments chosen are: ", 
                       paste(input$environments_to_show, collapse = ";")))
        })
        
}


shinyApp(ui, server)
