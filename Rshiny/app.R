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
library(bslib)

#### HOUSEKEEPING ###
# Clear workspace
rm(list = ls())

###############################################
# UI - USER INTERFACE
###############################################

ui <- fluidPage(
  # Header with logo 
  titlePanel(title=div(img(src="logo.png", height = "5%", width= "5%"), "Tit Simulator")),
  
  # Create the sidebar for variable input 
  
  sidebarLayout(
    # panel to the left
    sidebarPanel(
      # Question 1: which models? 
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
                           "6.3.2 - Direct hoarding (R top) - SC & FCR" = 632)
                         ,select = c(11)
                         ),
      # Question 2: which environments for survival? 
        selectInput("environments_to_show",
        label = "Pick which environment selection to show",
        choices = 
          c(
            "Mean across all" = "mean", 
            "All 12 seperate" = "all"
          ),
        selected = "Mean across all"),
      # Question 3: which physiological variable? 
      selectInput("phys_var_to_show", 
                  label = "Select the physiological variable", 
                  choices = c("Stomach content" = "stom_con", 
                              "Fat reserve" = "fat_res", 
                              "Fat change rate" = "fat_loss_r"), 
                  selected = "stom_con"),
      # Question 4: which behaviour variable? 
      selectInput("beh_var_to_show", 
                  label = "Select the behaviour variable", 
                  choices = c("Eat" = "eat", 
                              "Eat & hoard" = "eat_hoard", 
                              #"Foraging attempt" = "forage", 
                              "Hoard direct" = "dir_hoard", 
                              #"Successful foraging attempt" = "find_food", 
                              "Rest" = "rest", 
                              "Retrieve cache" = "retrieve", 
                              "Sleep" = "sleep"), 
                  selected = "eat")
    ),
    # Print the output in the main panel 
    mainPanel(
      height = "95%", 
      
      # Tabs with information
      navset_card_underline(
        # For the survival plots 
        nav_panel("Survival", plotlyOutput("survplot", height = "100%", width = "100%")),
        
        nav_panel("Physiological variable", plotlyOutput("physplot", height = 800, width = "100%")),
        
        nav_panel("Behaviour variable", plotlyOutput("behplot", height = 800, width = "100%")),
        
        nav_panel("Text 2", textOutput("env_chosen_text"))
      )
      
    )
  ) 
  
 
  
  # Start the sidebar (panel on the left)
  
)

##################################################
# SERVER 
##################################################

# input<-list(
#   environments_to_show = 1,
#   models_to_show = c(11, 21),
#   phys_var_to_how = "fat_res"
# )


server <- function(input, output) {
  # Load the default data from the .rda file
  load("data/results.rda") 
  #load("data/Esum_results.rda")
  load("data/Dsum_results.rda")
  
  # Load the colors and themes
  source("R/colours_themes.R")
  
  
  # Create the colour scale 
  #Create a custom color scale
  myColors <- stepped()
  names(myColors) <- levels(df_out$model)
  colScale <- scale_colour_manual(name = "model",values = myColors)
  
  
  # Reactive statement for the data selection 
  data<-reactive({
    # In all cases, select the models needed
    df<-df_out%>%
      filter(model %in% input$models_to_show)%>%
      filter(id == "alive")
    # Check if we need to summarise because 
    if(input$environments_to_show == "all"){
      # Nothing needs to happen
    }else if(input$environments_to_show == "mean"){
      # We need to summarise across all 12 environments 
      df<-df%>%
        group_by(timestep, id, model)%>%
        summarise(mean_val_env = mean(value, na.rm=T), nObs =n())%>%
        ungroup()
    }
    # This is where the other options (across temp, across food) will be. 
    
    # Feed back the data frame 
    df
    })
  
  # Reactive data for the behavioral and physiological data 
  # They need to be summarized across the days
  # And then according to the environment selection
  
  data_Dsum<-reactive({
    # TAke out models that we don't use 
    df<-df_out_Dsum%>%
      filter(model %in% input$models_to_show)
    # This data is in principle ready to be used across days
    # But if any of the "means" is selected, it will need to be changed
    if(input$environments_to_show=="all"){
      # nothing needs to happen
    }else if(input$environments_to_show == "mean"){
      # I need to take the mean across all environments 
      df<-df%>%
        group_by(timestep_within_day, id, model)%>%
        summarise(mean_val_env = mean(mean_val_day, na.rm=T), nObs=n())%>%
        ungroup()
    }
    # This is where I@ll put the other environment grouping options 
    
    # return the data fram 
    df
  })

data_phys<-reactive(
  data_Dsum()%>%
    filter(id == paste(input$phys_var_to_show))
)

data_beh<-reactive(
  data_Dsum()%>%
    filter(id == paste(input$beh_var_to_show))
)
  

# Render the survival plot
  output$survplot <- renderPlotly({
    # Prepare the survival plot
    p <- if(input$environments_to_show == "mean") {
      ggplot(data(), aes(x = timestep, y = mean_val_env)) +
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
        #scale_color_manual(values=as.vector(kelly(24))) +
        colScale+
        labs(fill = " ")
    } else {
      ggplot(data(), aes(x = timestep, y = value)) +
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
        #scale_color_manual(values=as.vector(kelly(24))) +
        colScale+
        labs(fill = " ")
    }
    
    ggplotly(p)
  })
  
  
  # Render the physiological variable plot 
  output$physplot <- renderPlotly({
    # Prepare the phys plot
    p <- if(input$environments_to_show == "mean") {
      ggplot(data_phys(), aes(x = timestep_within_day, y = mean_val_env)) +
        geom_line(aes(col = model)) +
        theme(plot.title = element_text(face = 'bold', size = 25), 
              axis.title.x = element_text(face = 'bold', size = 15), 
              axis.title.y = element_text(face = 'bold', size = 15), 
              strip.text = element_text(), 
              legend.text = element_text(size = 10), 
              legend.title = element_blank()) +
        ggtitle(paste("Physiological variable = ", input$phys_var_to_show)) +
        xlab(label = "Timestep (20 min) within Day")+
        ylab(label = paste(input$phys_var_to_show))+
        vera_theme()+
        #scale_color_manual(values=as.vector(kelly(24))) +
        colScale+
        labs(fill = " ")
    } else {
      p <- ggplot(data_phys(), aes(x = timestep_within_day, y = mean_val_day)) +
        geom_line(aes(col = model)) +
        facet_wrap(.~env, nrow = 6) +
        theme(plot.title = element_text(face = 'bold', size = 25), 
              axis.title.x = element_text(face = 'bold', size = 15), 
              axis.title.y = element_text(face = 'bold', size = 15), 
              strip.text = element_text(), 
              legend.text = element_text(size = 10), 
              legend.title = element_blank()) +
        ggtitle(paste("Physiological variable = ", input$phys_var_to_show)) +
        xlab(label = "Timestep (20 min) within Day")+
        ylab(label = paste(input$phys_var_to_show)) +
        vera_theme()+
        #scale_color_manual(values=as.vector(kelly(24))) +
        colScale+
        labs(fill = " ")
    }
    
    ggplotly(p)
  })
  
  
  # Render the behaviour variable plot 
  output$behplot <- renderPlotly({
    # Prepare the beh plot
    p <- if(input$environments_to_show == "mean") {
      ggplot(data_beh(), aes(x = timestep_within_day, y = mean_val_env)) +
        geom_line(aes(col = model)) +
        theme(plot.title = element_text(face = 'bold', size = 25), 
              axis.title.x = element_text(face = 'bold', size = 15), 
              axis.title.y = element_text(face = 'bold', size = 15), 
              strip.text = element_text(), 
              legend.text = element_text(size = 10), 
              legend.title = element_blank()) +
        ggtitle(paste("Behaviour variable = ", input$beh_var_to_show)) +
        xlab(label = "Timestep (20 min) within Day")+
        ylab(label = paste(input$beh_var_to_show))+
        vera_theme()+
        #scale_color_manual(values=as.vector(kelly(24))) +
        colScale+
        labs(fill = " ")
    } else {
      p <- ggplot(data_beh(), aes(x = timestep_within_day, y = mean_val_day)) +
        geom_line(aes(col = model)) +
        facet_wrap(.~env, nrow = 6) +
        theme(plot.title = element_text(face = 'bold', size = 25), 
              axis.title.x = element_text(face = 'bold', size = 15), 
              axis.title.y = element_text(face = 'bold', size = 15), 
              strip.text = element_text(), 
              legend.text = element_text(size = 10), 
              legend.title = element_blank()) +
        ggtitle(paste("Behavioural variable = ", input$beh_var_to_show)) +
        xlab(label = "Timestep (20 min) within Day")+
        ylab(label = paste(input$beh_var_to_show)) +
        vera_theme()+
        #scale_color_manual(values=as.vector(kelly(24))) +
        colScale+
        labs(fill = " ")
    }
    
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
