##########################################
# Small bird in winter - ABM 
# Start date: 15/05/2023
# Vera Vinken 
# Import normal model runs & visualize 
#########################################

# packages 
library(tidyverse)
library(purrr)
library(ggplot2)
library(data.table)

# link to the function file 
setwd("C:/Local_R/BiPhD-ABM/")
source('MOD_1_FuncSource.R')

# Retrieve the output you want to analyse 
# The filename can be copied from the folder manually 
# define the file you want to read
  retrieve_output_func(modelType = 'MOD_1_1')
  fileName11<-'MOD_1_1_out_D30_N1000_eType8_th_sc0.2_DL8_2023-05-16_16_10_33'
  load(paste0(fileName11))
  
  retrieve_output_func(modelType = 'MOD_1_2')
  fileName12<-'MOD_1_2_out_D30_N1000_eType8_th_sc10.1_th_sc20.3_DL8_2023-05-16_15_28_48'
  load(paste0(fileName12))
  
  retrieve_output_func(modelType = 'MOD_1_3')
  fileName13<-'MOD_1_3_out_D30_N1000_eType8_th_sc10.1_th_sc20.2_th_sc30.3_DL8_2023-05-16_15_35_54'
  load(paste0(fileName13))

 

#################################
#  CONCATENATE THE DATAFRAMES   # 
#################################

# For each of teh 12 variables that we want the matrices off 
# And calcualte the column means of each of those matrices (average per timestep)
  create_df_func(outputFile = outcome_1_1, modelType = '11')
  create_df_func(outputFile = outcome_1_2, modelType = '12')
  create_df_func(outputFile = outcome_1_3, modelType = '13')

# input different paratmers to get to output for the different models 


#################################
#   SHOW SOME VISUALISATION     # 
#################################

# run the plots 
  plots_12_func(inputdata=total_vars_df11, modelType='11')
  plots_12_func(inputdata=total_vars_df12, modelType='12')
  plots_12_func(inputdata=total_vars_df13, modelType='13')

# if needed 
  plot_12_11
  plot_12_12
  plot_12_13



  
  
  
  
  