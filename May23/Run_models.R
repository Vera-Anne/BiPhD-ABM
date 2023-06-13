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
library(truncnorm)
# library(pracma)
library(ggplot2)
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
 library(data.table)
# library(tidyverse)
# library(viridis)
library(foreach)
library(doParallel)
 library(purrr)             # for making lists into dataframes 
library(beepr)
# library(tidyr)
library(doParallel)         # For runing code parallel with dopar function 
library(foreach)            # For running code parallel 
library(ggpubr)             # To arrange plots 

# link to the function file 
# This contains all the general, smaller funcitons needed for the models 
#setwd("C:/Local_R/BiPhD-ABM/May23") # for hp elitebook 
setwd("C:/Local_R/BiPhD-ABM/May23") # for hp elitebook 
source('MOD_1_FuncSource.R')
source('ModelSource.R')

#################################################################
##   Model 1.1: Non-hoarding bird, Access to Stomach Content   ##
#################################################################

# Run model 1.1 
    mod_1_1(30, 1000, 8, 0.2, 8)
    mod_1_1(days=30, N=100, env_type = 8, th_forage_sc = 0.2, daylight_h = 8)


    #  CONCATENATE THE DATAFRAMES 
    #create_df_func(outputFile = outcome_1_1_env8, modelType = '11')
    # create plots 
    plots_12_func(inputdata=total_vars_df11, modelType='11')
    # if needed 
    plot_12_11

    
# rUN IT FOR THE 18 ENVIRONMENTS 
    system.time({
    env_func_1_1(days = 30, N= 100, th_forage_sc = 0.2, daylight_h = 8, modelType = 11)
    })
    
  # and then parallel 
    system.time({
    env_func_1_1_par(days = 30, N= 100, th_forage_sc = 0.2, daylight_h = 8, modelType = 11)
    })
    
    # Now do an overview image 
    plot_env_18_surv(output_env_func)
    
    
######################################################################
##   Model 1.2: Leftover-hoarding bird, Access to Stomach Content   ##
######################################################################
    
    # Run model 1.2
    mod_1_2(30, 100, 15, 0.1, 0.3, 8)
    
    
    #  CONCATENATE THE DATAFRAMES 
    #create_df_func(outputFile = outcome_1_1_env8, modelType = '11')
    # create plots 
    plots_12_func(inputdata=total_vars_df12, modelType='12')
    # if needed 
    plot_12_12
    
    
    # rUN IT FOR THE 18 ENVIRONMENTS 
    system.time({
    env_func_1_2(days = 30, N= 100, th_forage_sc1 = 0.1, th_forage_sc2 = 0.3, daylight_h = 8, modelType = 12)
    })
    
    # the same but in parallel 
    system.time({
    env_func_1_2_par(days = 30, N= 100, th_forage_sc1 = 0.1, th_forage_sc2 = 0.3, daylight_h = 8, modelType = 12)
    })
    
    # Now do an overview image 
    plot_env_18_surv(output_env_func)
    

######################################################################
##   Model 1.3: Direct hoarding bird, Access to Stomach Content   ##
######################################################################
    
# Run model 1.3.1
    mod_1_3_1(30, 1000, 15, 0.1, 0.2, 0.3, 8)
    
    
    #  CONCATENATE THE DATAFRAMES 
    #create_df_func(outputFile = outcome_1_1_env8, modelType = '11')
    # create plots 
    plots_12_func(inputdata=total_vars_df131, modelType='131')
    # if needed 
    plot_12_131
    
    
    # rUN IT FOR THE 18 ENVIRONMENTS 
    env_func_1_3_1(days = 3, N= 5, th_forage_sc1 = 0.1, th_forage_sc2 = 0.2, th_forage_sc3 = 0.3, daylight_h = 8, modelType = 131)
    # rUN IT FOR THE 18 ENVIRONMENTS 
    env_func_1_3_1_par(days = 3, N= 5, th_forage_sc1 = 0.1, th_forage_sc2 = 0.2, th_forage_sc3 = 0.3, daylight_h = 8, modelType = 131)
    
    # Now do an overview image 
    plot_env_18_surv(output_env_func)
    
# Run model 1.3.2 
    mod_1_3_2(30, 1000, 15, 0.1, 0.2, 0.3, 8)
    # now plot
    plots_12_func(inputdata = total_vars_df132, modelType='132')
    # output
    plot_12_132
    
    # rUN IT FOR THE 18 ENVIRONMENTS 
    env_func_1_3_2(days = 3, N= 5, th_forage_sc1 = 0.1, th_forage_sc2 = 0.2, th_forage_sc3 = 0.3, daylight_h = 8, modelType = 132)
    
    # Now do an overview image 
    plot_env_18_surv(output_env_func)
    
  #################################################################
  ##   Model 2.1: Non-hoarding bird, Access to Fat Reserves      ##
  #################################################################
    
    # Run model 2.1 
      mod_2_1(days=30, N=1000, env_type = 15, th_forage_fr = 2, daylight_h = 8)
  
    
    # create plots 
      plots_12_func(inputdata=total_vars_df21, modelType='21')
      # if needed 
      plot_12_21
      
    
    # Run IT FOR THE 18 ENVIRONMENTS 
    # parallel 
      system.time({
        env_func_2_1_par(days = 30, N= 100, th_forage_fr = 2, daylight_h = 8, modelType = 21)
      })

      
  ######################################################################
  ##   Model 2.2: Leftover-hoarding bird, Access to Fat - Reserves    ##
  ######################################################################
      
      # Run model 2.2
      mod_2_2(30, 1000, 15, 1, 3, 8)
      
      # create plots 
      plots_12_func(inputdata=total_vars_df22, modelType='22')
      # if needed 
      plot_12_22
      
      
      # rUN IT FOR THE 18 ENVIRONMENTS 
      # parallel 
      system.time({
        env_func_2_2_par(days = 30, N= 100, th_forage_fr1 = 0.1, th_forage_fr2 = 0.3, daylight_h = 8, modelType = 22)
      })
      
      
  ######################################################################
  ##    Model 2.3: Direct hoarding bird, Access to Fat - Reserves     ##
  ######################################################################
      
      # Run model 2.3.1
      mod_2_3_1(30, 1000, 15, 1, 2, 3, 8)
      
      # create plots 
      plots_12_func(inputdata=total_vars_df231, modelType='231')
      # if needed 
      plot_12_231
      
      
      # rUN IT FOR THE 18 ENVIRONMENTS 
        # env_func_1_3_1(days = 3, N= 5, th_forage_sc1 = 0.1, th_forage_sc2 = 0.2, th_forage_sc3 = 0.3, daylight_h = 8, modelType = 131)
      # rUN IT FOR THE 18 ENVIRONMENTS 
        # env_func_1_3_1_par(days = 3, N= 5, th_forage_sc1 = 0.1, th_forage_sc2 = 0.2, th_forage_sc3 = 0.3, daylight_h = 8, modelType = 131)
      
      
      # Run model 1.3.2 
      mod_2_3_2(30, 1000, 15, 1, 2, 3, 8)
      # now plot
      plots_12_func(inputdata = total_vars_df232, modelType='232')
      # output
      plot_12_232
      
      # rUN IT FOR THE 18 ENVIRONMENTS 
        # env_func_1_3_2(days = 3, N= 5, th_forage_sc1 = 0.1, th_forage_sc2 = 0.2, th_forage_sc3 = 0.3, daylight_h = 8, modelType = 132)
      
      
