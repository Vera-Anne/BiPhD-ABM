#################################
# Small bird in winter - ABM 
# Start date: 16/05/2023
# Vera Vinken 
# all models - for running them (singular)
#################################

##############################
#      load packages         #
##############################
# library(usethis)
# library(devtools)
# library(truncnorm)
# library(pracma)
# library(ggplot2)
# library(plotly) # for 3D surface plot 
# library(rgl)
# library(plot3D)
# library(htmlwidgets)
# library(webshot)
# library(withr)
# library('plyr')
# library('gridExtra')
# library(grid)
# library(lattice)
# library(dplyr)
# library(data.table)
# library(tidyverse)
# library(viridis)
# library(foreach)
# library(doParallel)
# library(purrr)
# library(beepr)
# library(tidyr)

# link to the function file 
# This contains all the general, smaller funcitons needed for the models 
setwd("C:/Local_R/BiPhD-ABM/May23")
source('MOD_1_FuncSource.R')
source('ModelSource.R')

#################################################################
##   Model 1.1: Non-hoarding bird, Access to Stomach Content   ##
#################################################################

# Run model 1.1 
    mod_1_1(10, 100, 8, 0.2, 8)


    #  CONCATENATE THE DATAFRAMES 
    #create_df_func(outputFile = outcome_1_1_env8, modelType = '11')
    # create plots 
    plots_12_func(inputdata=total_vars_df11, modelType='11')
    # if needed 
    plot_12_11

    
# rUN IT FOR THE 18 ENVIRONMENTS 
    env_func_1_1(days = 3, N= 5, th_forage_sc = 0.2, daylight_h = 8, modelType = 11)

######################################################################
##   Model 1.2: Leftover-hoarding bird, Access to Stomach Content   ##
######################################################################
    
    # Run model 1.1 
    mod_1_2(10, 100, 8, 0.1, 0.3, 8)
    
    
    #  CONCATENATE THE DATAFRAMES 
    #create_df_func(outputFile = outcome_1_1_env8, modelType = '11')
    # create plots 
    plots_12_func(inputdata=total_vars_df12, modelType='12')
    # if needed 
    plot_12_12
    
    
    # rUN IT FOR THE 18 ENVIRONMENTS 
    env_func_1_2(days = 3, N= 5, th_forage_sc1 = 0.1, th_forage_sc2 = 0.3, daylight_h = 8, modelType = 12)

######################################################################
##   Model 1.2: Leftover-hoarding bird, Access to Stomach Content   ##
######################################################################
    
    # Run model 1.1 
    mod_1_3(10, 100, 8, 0.1, 0.2, 0.3, 8)
    
    
    #  CONCATENATE THE DATAFRAMES 
    #create_df_func(outputFile = outcome_1_1_env8, modelType = '11')
    # create plots 
    plots_12_func(inputdata=total_vars_df13, modelType='13')
    # if needed 
    plot_12_12
    
    
    # rUN IT FOR THE 18 ENVIRONMENTS 
    env_func_1_2(days = 3, N= 5, th_forage_sc1 = 0.1, th_forage_sc2 = 0.2, th_forage_sc3 = 0.3, daylight_h = 8, modelType = 12)
    
    
    
