# Tit simulation app
# Vera Vinken 
# 20 May 2014 

# Packages 
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(plotly)
library(pals)
library(bslib)
library(profvis)

#### HOUSEKEEPING ###
# Clear workspace
rm(list = ls())



###############################################
# UI - USER INTERFACE                         #
###############################################



ui <- fluidPage(
  # set background
  setBackgroundColor("#e3e2db"),
  # Header with logo 
  titlePanel(title=div(img(src="logo.png", height = "3%", width= "3%"), "Tit Simulator (Test version May '24)", 
                       style = "background-color: #484537; padding: 12px; color:#e3e2db; font-size:35px; font-style:bold;")),
  
  
  # Create the sidebar for variable input 
  sidebarLayout(
    # panel to the left
    sidebarPanel(
      style = "background-color: #c8c4b7; padding: 15px; font-size:15px;",
      tags$head(
        tags$style(type='text/css', 
                   ".nav-tabs {font-size: 20px;} 
              /* Set the height of plot containers */
              .plot-container {
                height: 80vh; /* 80% of the viewport height */
              }"
        )
      ),
      
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
                           "4.3.1 - Direct hoarding (H top) - FR & FCR" = 431, 
                           "4.3.2 - Direct hoarding (R top) - FR & FCR " = 432,
                           "5.1 - Non hoarding - Stomach content" = 51, 
                           "5.2 - Leftover hoarding - SC & FR" = 52,
                           "5.3.1 - Direct hoarding (H top) - SC & FR" = 531, 
                           "5.3.2 - Direct hoarding (R top) - SC & FR " = 532,
                           "6.1 - Non hoarding - FCR" = 61, 
                           "6.2 - Leftover hoarding - SC & FCR" = 62,
                           "6.3.1 - Direct hoarding (H top) - SC & FCR " = 631, 
                           "6.3.2 - Direct hoarding (R top) - SC & FCR " = 632),
                         #selected = c(11)
      ),
      selectInput("environments_to_show",
                  label = "Pick which environment selection to show",
                  choices = 
                    c(
                      "Mean across all" = "mean", 
                      "All 12 separate" = "all"
                    ),
                  selected = "all"),
      selectInput("phys_var_to_show", 
                  label = "Select the physiological variable", 
                  choices = c("Stomach content" = "stom_con", 
                              "Fat reserve" = "fat_res", 
                              "Fat change rate" = "fat_loss_r"), 
                  selected = "stom_con"),
      selectInput("beh_var_to_show", 
                  label = "Select the behaviour variable", 
                  choices = c("Eat" = "eat", 
                              "Eat & hoard" = "eat_hoard", 
                              "Hoard direct" = "dir_hoard", 
                              "Rest" = "rest", 
                              "Retrieve cache" = "retrieve", 
                              "Sleep" = "sleep"), 
                  selected = "eat"),
      # sliderInput("height", "Plot height", min = 100, max = 1800, value = 800),
      # sliderInput("width", "Plot width", min = 100, max = 1800, value = 1100),
      actionButton(inputId = "go", label = "Update")
    ),
    # Print the output in the main panel 
    mainPanel(
      style = "height: 90vh; overflow-y: auto;",
      tags$head(
        tags$style(type='text/css', 
                   ".nav-tabs {font-size: 20px;} 
              # /* Set the height of plot containers */
              # .plot-container {
              #   height: 80vh; /* 80% of the viewport height */
              # }"
        )
      ),
      tabsetPanel(
        #style = h
        id = "tabset",
        tabPanel("Survival", plotlyOutput("survplot", height = "100%", width = "100%"),style = "background-color:#ffffff; padding: 15px;height:80vh;"),
        tabPanel("Physiological variable", plotlyOutput("physplot", height = "100%", width = "100%"), style = "background-color:#ffffff; padding: 15px;height:80vh;"),
        tabPanel("Behavioural variable", plotlyOutput("behplot", height = "100%", width = "100%"), style = "background-color:#ffffff; padding: 15px;height:80vh;")
      ) # close tabset


      
      
    ) # close main panel
  ), # close sidebar layout
  
  
  
  
) # close fluidpage


server <- function(input, output, session) {
  
  # Load the default data from the .rda file
  load("Data/results.Rda") 
  load("Data/Dsum_results.Rda")
  
  # Load the colors and themes
  source("R/colours_themes.R")
  
  # Create the colour scale 
  myColors <- stepped()
  names(myColors) <- levels(df_out$model)
  colScale <- scale_colour_manual(name = "model", values = myColors)
  
  # Reactive statement for the data selection 
  data <- eventReactive(input$go, {
    # In all cases, select the models needed
    df <- df_out %>%
      filter(model %in% input$models_to_show) %>%
      filter(id == "alive")
    # Check if we need to summarise
    if(input$environments_to_show == "all") {
      # Nothing needs to happen
    } else if(input$environments_to_show == "mean") {
      # We need to summarise across all 12 environments 
      df <- df %>%
        group_by(timestep, id, model) %>%
        summarise(mean_val_env = mean(value, na.rm = TRUE), nObs = n()) %>%
        ungroup()
    }
    df
  })
  
  # Reactive data for the behavioral and physiological data 
  data_Dsum <- eventReactive(input$go, {
    # Take out models that we don't use 
    df <- df_out_Dsum %>%
      filter(model %in% input$models_to_show)
    # This data is in principle ready to be used across days
    if(input$environments_to_show == "all") {
      # nothing needs to happen
    } else if(input$environments_to_show == "mean") {
      # Take the mean across all environments 
      df <- df %>%
        group_by(timestep_within_day, id, model) %>%
        summarise(mean_val_env = mean(mean_val_day, na.rm = TRUE), nObs = n()) %>%
        ungroup()
    }
    df
  })
  
  # phys specific 
  data_phys <- eventReactive(input$go, {
    data_Dsum() %>%
      filter(id == paste(input$phys_var_to_show))
  })
  
  # beh specific 
  data_beh <- eventReactive(input$go, {
    data_Dsum() %>%
      filter(id == paste(input$beh_var_to_show))
  })
  
  # Render the survival plot
  output$survplot <- renderPlotly({
    if(input$environments_to_show == "mean") {
      survplot <-ggplot(data(), aes(x = timestep, y = mean_val_env)) +
        geom_line(aes(col = model)) +
        theme(plot.title = element_text(face = 'bold', size = 25), 
              axis.title.x = element_text(face = 'bold', size = 15), 
              axis.title.y = element_text(face = 'bold', size = 15), 
              strip.text = element_text(), 
              legend.text = element_text(size = 10), 
              legend.title = element_blank()) +
        ggtitle("Survival") +
        xlab("Timestep (20 min)") +
        ylab("Proportion of birds alive") +
        vera_theme() +
        colScale +
        labs(fill = " ")
    } else {
      survplot <-ggplot(data(), aes(x = timestep, y = value)) +
        geom_line(aes(col = model)) +
        facet_wrap(. ~ env, nrow = 6) +
        theme(plot.title = element_text(face = 'bold', size = 25),
              axis.title.x = element_text(face = 'bold', size = 15),
              axis.title.y = element_text(face = 'bold', size = 15),
              strip.text = element_text(),
              legend.text = element_text(size = 10),
              legend.title = element_blank()) +
        ggtitle("Survival") +
        xlab("Timestep (20 min)") +
        ylab("Proportion of birds alive") +
        vera_theme() +
        colScale +
        labs(fill = " ")
    }
    ggplotly(survplot)
    #survplot
    #ggplotly(survplot)#, width = input$width, height = input$height)#, height = input$height, width = input$width)
    #plotlyOutput
  })
  
  # Render the physiological variable plot 
  output$physplot <- renderPlotly({
    if(input$environments_to_show == "mean") {
      physplot <- ggplot(data_phys(), aes(x = timestep_within_day, y = mean_val_env)) +
        geom_line(aes(col = model)) +
        theme(plot.title = element_text(face = 'bold', size = 20), 
              axis.title.x = element_text(face = 'bold', size = 15), 
              axis.title.y = element_text(face = 'bold', size = 15), 
              strip.text = element_text(), 
              legend.text = element_text(size = 10), 
              legend.title = element_blank()) +
        ggtitle(paste("Physiological variable = ", input$phys_var_to_show)) +
        xlab("Timestep (20 min) within Day") +
        ylab(input$phys_var_to_show) +
        vera_theme() +
        colScale +
        labs(fill = " ")
    } else {
      physplot <- ggplot(data_phys(), aes(x = timestep_within_day, y = mean_val_day)) +
        geom_line(aes(col = model)) +
        facet_wrap(. ~ env, nrow = 6) +
        theme(plot.title = element_text(face = 'bold', size = 20), 
              axis.title.x = element_text(face = 'bold', size = 15), 
              axis.title.y = element_text(face = 'bold', size = 15), 
              strip.text = element_text(), 
              legend.text = element_text(size = 10), 
              legend.title = element_blank()) +
        ggtitle(paste("Physiological variable = ", input$phys_var_to_show)) +
        xlab("Timestep (20 min) within Day") +
        ylab(input$phys_var_to_show) +
        vera_theme() +
        colScale +
        labs(fill = " ")
    }
    #
    ggplotly(physplot)#, width = input$width, height = input$height)
    #plotlyOutput
  })
  
  # Render the behaviour variable plot 
  output$behplot <- renderPlotly({
     if(input$environments_to_show == "mean") {
       behplot  <-ggplot(data_beh(), aes(x = timestep_within_day, y = mean_val_env)) +
        geom_line(aes(col = model)) +
        theme(plot.title = element_text(face = 'bold', size = 20), 
              axis.title.x = element_text(face = 'bold', size = 15), 
              axis.title.y = element_text(face = 'bold', size = 15), 
              strip.text = element_text(), 
              legend.text = element_text(size = 10), 
              legend.title = element_blank()) +
        ggtitle(paste("Behaviour variable = ", input$beh_var_to_show)) +
        xlab("Timestep (20 min) within Day") +
        ylab(input$beh_var_to_show) +
        vera_theme() +
        colScale +
        labs(fill = " ")
    } else {
      behplot  <-ggplot(data_beh(), aes(x = timestep_within_day, y = mean_val_day)) +
        geom_line(aes(col = model)) +
        facet_wrap(. ~ env, nrow = 6) +
        theme(plot.title = element_text(face = 'bold', size = 20), 
              axis.title.x = element_text(face = 'bold', size = 15), 
              axis.title.y = element_text(face = 'bold', size = 15), 
              strip.text = element_text(), 
              legend.text = element_text(size = 10), 
              legend.title = element_blank()) +
        ggtitle(paste("Behavioural variable = ", input$beh_var_to_show)) +
        xlab("Timestep (20 min) within Day") +
        ylab(input$beh_var_to_show) +
        vera_theme() +
        colScale +
        labs(fill = " ")
    }
    ggplotly(behplot)#, width = input$width, height = input$height)
    #plotlyOutput
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


#profvis::profvis(runApp(shinyApp(ui, server)), prof_output = "C:/Local_R/BiPhD-ABM/Rshiny/")

shinyApp(ui, server)