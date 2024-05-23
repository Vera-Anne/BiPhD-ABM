# Tit simulation app
# Vera Vinken 
# 20 May 2014 


#### TO DO LIST #### 

# make the check boxes for variable and environment drop downs 
# fix that graphics box that turns up 
# figure sizes
# Model selection should go by "all hoarders", "all direct hoarders", "all stomach content" 
# Split up subset 1 and subset 2? 
# Fix colours so that each model has its own color that it will always get


# Packages 
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(pals)

#### HOUSEKEEPING ###
# Clear workspace
rm(list = ls())

###############################################
# UI - USER INTERFACE
###############################################

ui <- fluidPage(
  h1("Tit Simulation App"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("models_to_show", "Pick which models to show:",
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
                           "4.3.1 - Direct hoarding (H top) - FR & FCR (DOESNT WORK YET)" = 431, 
                           "4.3.2 - Direct hoarding (R top) - FR & FCR (DOESNT WORK YET)" = 432,
                           "5.1 - Non hoarding - Stomach content" = 51, 
                           "5.2 - Leftover hoarding - SC & FR" = 52,
                           "5.3.1 - Direct hoarding (H top) - SC & FR (DOESNT WORK YET)" = 531, 
                           "5.3.2 - Direct hoarding (R top) - SC & FR (DOESNT WORK YET)" = 532,
                           "6.1 - Non hoarding - FCR" = 61, 
                           "6.2 - Leftover hoarding - SC & FCR" = 62,
                           "6.3.1 - Direct hoarding (H top) - SC & FCR (DOESNT WORK YET)" = 631, 
                           "6.3.2 - Direct hoarding (R top) - SC & FCR (DOESNT WORK YET)" = 632), 
                         select = c(11)),
      checkboxGroupInput("environments_to_show", "Pick which environments to show (CURRENTLY ONLY PICK 1):", 
                         c("Average over all environments" = 1, 
                           "12 Environments separate" = 12)),
      checkboxGroupInput("phys_var_to_show", "Pick which physiological variable to show (CURRENTLY ONLY PICK 1):", 
                         c("Stomach content" = "stom_con", 
                           "Fat reserve" = "fat_res", 
                           "Fat change rate" = "fat_loss_r"))
    ),
    mainPanel(
      textOutput("models_chosen_text"), 
      textOutput("env_chosen_text"), 
      plotlyOutput("survplot", height = "1000px", width = "100%"),
      plotlyOutput("physplot", height = "1100px", width = "100%")
    )
  )
)

##################################################
# SERVER 
##################################################

server <- function(input, output) {
  # Load the default data from the .rda file
  load("data/results.rda") 
  load("data/Esum_results.rda")
  load("data/Dsum_results.rda")
  
  # Load the colors and themes
  source("R/colours_themes.R")
  
  
  # Create the colour scale 
  #Create a custom color scale
  myColors <- stepped()
  names(myColors) <- levels(dat$grp)
  colScale <- scale_colour_manual(name = "grp",values = myColors)
  
  # Render the survival plot
  output$survplot <- renderPlotly({
    # Filter out the data for the survival plot 
    surv_dat <- if(input$environments_to_show == 1) {
      df_out_Esum
    } else {
      df_out
    }
    
    # Filter for the correct models and survival status
    surv_dat <- surv_dat %>%
      filter(model %in% input$models_to_show) %>%
      filter(id == "alive")
    
    # Prepare the survival plot
    p <- if(input$environments_to_show == 1) {
      ggplot(surv_dat, aes(x = timestep, y = mean_val_env)) +
        geom_line(aes(col = model)) +
        theme(plot.title = element_text(face = 'bold', size = 25), 
              axis.title.x = element_text(face = 'bold', size = 15), 
              axis.title.y = element_text(face = 'bold', size = 15), 
              strip.text = element_text(), 
              legend.text = element_text(size = 10), 
              legend.title = element_blank()) +
        ggtitle("Survival") +
        xlab(label = "Timestep (20 min)")+
        ylab(label = "Proportion  of birds alive")+
        vera_theme()+
        scale_color_manual(values=as.vector(kelly(24))) +
        labs(fill = " ")
    } else {
      ggplot(surv_dat, aes(x = timestep, y = value)) +
        geom_line(aes(col = model)) +
        facet_wrap(.~env, nrow = 6) +
        theme(plot.title = element_text(face = 'bold', size = 25), 
              axis.title.x = element_text(face = 'bold', size = 15), 
              axis.title.y = element_text(face = 'bold', size = 15), 
              strip.text = element_text(), 
              legend.text = element_text(size = 10), 
              legend.title = element_blank()) +
        ggtitle("Survival") +
        xlab(label = "Timestep (20 min)")+
        ylab(label = "Proportion  of birds alive")+
        vera_theme()+
        scale_color_manual(values=as.vector(kelly(24))) +
        labs(fill = " ")
    }
    
    ggplotly(p)
  })
  
  
  # Render the physiological variable plot 
  output$physplot <- renderPlotly({
    
    # Filter for the correct models and survival status
    phys_dat <- df_out_Dsum %>%
      filter(model %in% input$models_to_show) %>%
      filter(id == paste(input$phys_var_to_show))
    
    # Prepare the physiological variable plot
    p <- ggplot(phys_dat, aes(x = timestep_within_day, y = mean_val_day)) +
      geom_line(aes(col = model)) +
      facet_wrap(.~env, nrow = 6) +
      theme(plot.title = element_text(face = 'bold', size = 25), 
            axis.title.x = element_text(face = 'bold', size = 15), 
            axis.title.y = element_text(face = 'bold', size = 15), 
            strip.text = element_text(), 
            legend.text = element_text(size = 10), 
            legend.title = element_blank()) +
      ggtitle("Physiological variable") +
      xlab(label = "Timestep (20 min)")+
      ylab(label = paste(input$phys_var_to_show)) +
      vera_theme()+
      scale_color_manual(values=as.vector(kelly(24))) +
      labs(fill = " ")
    
    ggplotly(p)
  })
  
  
  # Output the selected models
  output$models_chosen_text <- renderText({
    paste0("The models chosen are: ", paste(input$models_to_show, collapse = ";"))
  })
  
  # Output the selected environments
  output$env_chosen_text <- renderText({
    paste0("The environments chosen are: ", paste(input$environments_to_show, collapse = ";"))
  })
}

shinyApp(ui, server)
