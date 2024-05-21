
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
data = mtcars
i=1
namdat= names(data)
for( i in 1:ncol (data)){
  assign( namdat[i],  unique(data[,i]))
  print ( class(data[,i]))
}

infoTable = rbind(
  c( "mpg"	,"Miles/(US) gallon"),
  c( "cyl"	,"Number of cylinders"),
  c( "disp","Displacement (cu.in.)"),
  c( "hp"	,"Gross horsepower"),
  c( "drat","Rear axle ratio"),
  c( "wt"	,"Weight (* 1000 lbs)"),
  c( "qsec","1/4 mile time (s)"),
  c( "vs"	,"Engine (0 = V-shaped, 1 = straight)"),
  c( "am"	,"Transmission (0 = automatic, 1 = manual)"),
  c( "gear", "Number of forward gears"))


###############################################
# UI - USER INTERFACE
###############################################

ui <- fluidPage(
  h1("My mtcars exploration app!"),
  sidebarLayout(
    sidebarPanel (
      radioButtons( "xax1" , "Pick your predictor variable",choices = c(
        "Rear axle ratio",
        "Weight (* 1000 lbs)",
        "Gross horsepower"
      ) ),
      sliderInput("secs", "1/4 mile in less than x seconds" ,min= 10 , max = 25,value = 16), 
      # vv - add a slider where you can input your miles per gallon 
      sliderInput("mpg", "Miles per Gallon" ,min= 10 , max = 40, value = 20)
    ),
    mainPanel(
      plotOutput("qsecplot"),
      textOutput("fasterthan"), 
      plotOutput("mpgplot"), 
      textOutput("betterthan"),
      plotOutput("combineplot"),
      textOutput("best")
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
  
  
  
  # QSEC plot  (number of SEConds for Quarter mile)
  output$qsecplot = renderPlot({
    
    # find the column
    # vv - selects the variable in infoTable (c1 = variable name, c2 = written name), based on the input variable selected
    whi =  which( infoTable[,2] == input$xax1)
    # vv - adds a column to the data with information abou thte selected variable (mpg in the current example)
    data$xvar = data[,whi]
    
    # run a lm 
    m_qsec<-lm(data$qsec ~ data$xvar)
    m_qsec_sum<-summary(m_qsec)
    p<-as.numeric(m_qsec_sum$coefficients[,4][2])
    
    if(p<=0.01){
      printed_p<-paste("p < 0.01")
      col_set<-"seagreen"
    }else if (p >= 0.05){
      printed_p<-paste("p = ", round(p, 3))
      col_set<-"red"
    }else{
      printed_p<-paste("p = ", round(p, 3))
      col_set<-"chocolate"
    }
    
    
    # Plot
    # vv - plots the data by the selected variable and the number of secons cutoff that are given in 
    p = ggplot ( data, aes ( x = xvar , y = qsec))+
      xlab( input$xax1)+
      ylab( "1/4 mile time (s)")+
      geom_smooth(col = "chocolate", fill = "chocolate", alpha = 0.3, method = "lm")+
      theme_classic(base_size =22)+
      geom_hline(yintercept = input$secs,lty = 2, col = "grey")+
      annotate(geom = "text", label= paste(printed_p), x = 3, y = 20, col = paste(col_set), size = 8)
    
    
    # type of plot will depend on input variable
    if( class(data$xvar)== "numeric"){
      p = p+geom_point()  
    }
    
    # print plot to output
    p
    
  })
  
  # vv - now the text output: pastes all the cars that are faster than this cutoff point 
  
  output$fasterthan = renderText( {
    print( paste0 ( paste( rownames( data)[ data$qsec < input$secs], collapse = "; ") ,
                    "; all drive a quarter mile in less than ",
                    input$secs, 
                    " seconds from standstill."))
  })
  
  
  #### now for MPG 
  
  # MPG plot 
  output$mpgplot = renderPlot({
    
    # find the column
    # vv - selects the variable in infoTable (c1 = variable name, c2 = written name), based on the input variable selected
    whi =  which( infoTable[,2] == input$xax1)
    # vv - adds a column to the data with information abou thte selected variable (rear axel ratio)
    data$xvar = data[,whi]
    
    
    # run a lm 
    m_mpg<-lm(data$mpg ~ data$xvar)
    m_mpg_sum<-summary(m_mpg)
    p<-as.numeric(m_mpg_sum$coefficients[,4][2])
    
    if(p<=0.01){
      printed_p<-paste("p < 0.01")
      col_set<-"seagreen"
    }else if (p >= 0.05){
      printed_p<-paste("p = ", round(p, 3))
      col_set<-"red"
    }else{
      printed_p<-paste("p = ", round(p, 3))
      col_set<-"chocolate"
    }
    
    # Plot
    # vv - plots the data by the selected variable and the number of secons cutoff that are given in 
    p = ggplot ( data, aes ( x = xvar , y = mpg ))+
      xlab( input$xax1)+
      ylab( "Mile per Gallon")+
      geom_smooth(col= "goldenrod", fill = "goldenrod", alpha =0.3, method = "lm")+
      theme_classic(base_size =22)+
      geom_hline(yintercept = input$mpg,lty = 2, col = "grey")+
      annotate(geom = "text", label= paste(printed_p), x = 3, y = 30, col = paste(col_set), size = 8)
    
    
    # type of plot will depend on input variable
    if( class(data$xvar)== "numeric"){
      p = p+geom_point()  
    }
    
    # print plot to output
    p
    
  })
  
  # vv - now the text output: pastes all the cars that are faster than this cutoff point 
  
  output$betterthan = renderText( {
    print( paste0 ( paste( rownames( data)[ data$mpg > input$mpg], collapse = "; ") ,
                    "; all drive more than  ",
                    input$mpg, 
                    " per gallon."))
  })
  
  #### now for combining 
  
  # MPG plot 
  output$combineplot = renderPlot({
    
    # find the column
    # vv - selects the variable in infoTable (c1 = variable name, c2 = written name), based on the input variable selected
    whi =  which( infoTable[,2] == input$xax1)
    # vv - adds a column to the data with information abou thte selected variable (rear axel ratio)
    data$xvar = data[,whi]
    
    
    # run a lm 
    m_com<-lm(data$mpg ~ data$qsec)
    m_com_sum<-summary(m_com)
    p<-as.numeric(m_com_sum$coefficients[,4][2])
    
    if(p<=0.01){
      printed_p<-paste("p < 0.01")
      col_set<-"seagreen"
    }else if (p >= 0.05){
      printed_p<-paste("p = ", round(p, 3))
      col_set<-"red"
    }else{
      printed_p<-paste("p = ", round(p, 3))
      col_set<-"chocolate"
    }
    
    # Plot
    # vv - plots the data by the selected variable and the number of secons cutoff that are given in 
    p = ggplot ( data, aes ( x = qsec , y = mpg ))+
      xlab( " 1/4 mile time (s)")+
      ylab( "Mile per Gallon")+
      geom_smooth(col= "seagreen", fill = "seagreen", alpha =0.3, method = "lm")+
      theme_classic(base_size =22)+
      geom_hline(yintercept = input$mpg,lty = 2, col = "grey")+
      geom_vline(xintercept = input$secs,lty = 2, col = "grey")+
      annotate(geom = "text", label= paste(printed_p), x = 16, y = 30, col = paste(col_set), size = 8)
    
    
    # type of plot will depend on input variable
    if( class(data$xvar)== "numeric"){
      p = p+geom_point()  
    }
    
    # print plot to output
    p
    
  })
  
  # vv - now the text output: pastes all the cars that are faster than this cutoff point 
  
  # subset<-data%>%
  #   filter(mpg > input$mpg)%>%
  #   filter(qsec < input$qsec)
  
  output$best = renderText( {
    print( paste0 ( paste( rownames(data)[((data$mpg > input$mpg & data$qsec < input$secs))], collapse = "; ") ,
                    "; all drive more than  ",
                    input$mpg, 
                    " per gallon.", 
                    " AND, drive a quarter mile in less than ", 
                    input$secs))
  })
  
  
  
}


shinyApp(ui, server)
