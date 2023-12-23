######################################################################################
# 21/12/2023
# debugging script for testing hpc performance 
# Vera Vinken 
# Last changes: 21/12/2023
######################################################################################

# Goal is to create a script that can run per level
# This should reduce memory requirement and hopefully incrase speed for running. 

##############################
#      load packages         #
##############################
library(usethis)
library(devtools)
library(truncnorm)
library(pracma)
library(ggplot2)
#library(rgl)
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
library(foreach)
library(doParallel)
library(utils)

# link to the function file 
source('MOD_1_FuncSource.R')
source('ModelSource_HPC.R')
#source("ModelSource.R")


#############################
#  COMMAND LINE ARGUMENTS   #
#############################
args<-commandArgs(trailingOnly=TRUE)

# Input variables 
# Number of days in the simulation 
days<-as.numeric(args[1])

# Number of agents in the simulation
N<-as.numeric(args[2])

# Number of hours of daylight 
daylight_h<- as.numeric(args[3])

# model type 
modelType<-as.numeric(args[4])

# Output directory 
out_dir<-args[5]


# Run the model 
    if(modelType==11){
      env_func_1_1_par_hpc(days = days, N= N, th_forage_sc = cur_th, daylight_h = daylight_h, modelType = 11)
    }else if(modelType==21){
      env_func_2_1_par_hpc(days = days, N= N, th_forage_fr = cur_th, daylight_h = daylight_h, modelType = 21)
    }else if(modelType==31){
      env_func_3_1_par_hpc(days = days, N= N, th_forage_flr = cur_th, daylight_h = daylight_h, modelType = 31)
    }else{
      print('problem with model_type settings')
    }


#####################
#   SAVE RESULTS    # 
#####################


# set directory 
setwd(out_dir)
# make sure to attach the threshold to the dataframe 
save(output_env_func, file=paste0('single_env_run_testing_', modelType, "_", format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'.Rda'))






