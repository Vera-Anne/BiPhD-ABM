######################################################################################
# 21/12/2023
# Rscript that runs the binary branching for models x.1 - Version 2, as the first isnt working 
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
source("ModelSource.R")


#############################
#  COMMAND LINE ARGUMENTS   #
#############################
args<-commandArgs(trailingOnly=TRUE)

# Input variables 
  # Number of days in the simulation 
  days<-as.numeric(args[1])
  
  # Number of agents in the simulation
  N<-as.numeric(args[2])
  
  # set the ranges 
  range_min_start <- as.numeric(args[3])
  range_max_start <- as.numeric(args[4])
  
  # set the level 
  level<-as.numeric(args[5])

  # Number of hours of daylight 
  daylight_h<- as.numeric(args[6])
  
  # model type 
  modelType<-as.numeric(args[7])
  
  # Output directory 
  out_dir<-args[8]


##########################
##    BRANCHING         ##
##########################

# Run the branch function 
vals<-branch_func(range_min = range_min_start, range_max = range_max_start, branch_num = 4)
  
  
# Now run the model 
  for (i in 1:4){
    if (i==1){
      # Create empty list for results 
      level_results<-list()
    }
    # Set current th 
    cur_th<-vals[i]
    
    # Run the model 25 times in the 12 environments and take the average
    for (j in 1:25){
      if (j==1){
        out_per_run_list<-list()
      }
      # Run the model 
      if(modelType==11){
        env_func_1_1_par(days = days, N= N, th_forage_sc = cur_th, daylight_h = daylight_h, modelType = 11)
      }else if(modelType==21){
        env_func_2_1_par_hpc(days = days, N= N, th_forage_fr = cur_th, daylight_h = daylight_h, modelType = 21)
      }else if(modelType==31){
        env_func_3_1_par_hpc(days = days, N= N, th_forage_flr = cur_th, daylight_h = daylight_h, modelType = 31)
      }else{
        print('problem with model_type settings')
      }
      out_per_run_list[j]<-cbind(output_env_func[[1]][1], cur_th)
      print(paste(j))
    } # end of loop for 25 run s
    
    # bind together
    out_per_run_df<-data.frame(matrix(unlist(out_per_run_list), nrow=25, byrow=TRUE),stringsAsFactors=FALSE)
    colnames(out_per_run_df)<-"HL"
    # Now take the average for the threshold 
    mean_perf<-c(mean(out_per_run_df$HL), cur_th)
    # add to list
    level_results[[i]]<-mean_perf
    # mark that things are working
    print(paste("Done with level ", i))
  } # end of 1-4 branching loop
  
  halflife_out<-data.frame(matrix(unlist(level_results), nrow=4, byrow=TRUE),stringsAsFactors=FALSE)
  names<-c("HL",  "th")
  colnames(halflife_out)<-names
  
  # select highest outcome
  max_HL<-max(halflife_out$HL)
  best<-halflife_out[halflife_out$HL==max_HL, ]
  best_th<-best[1,2]
  
  # Now calculate the ranges for the next level 
  # Calculate the new ranges for level 2 (this uses the stepsize that comes out of the branching function in level 1 )
  range_min_next<-(best_th-(step_size/2))
  range_max_next<-(best_th+(step_size/2))
  
  next_range<-cbind(range_min_next, range_max_next)
  
  # Now connect all the results together 
  level_output_list<-list(halflife_out, best, next_range)
  
#####################
#   SAVE RESULTS    # 
#####################
  
  
# set directory 
  setwd(out_dir)
# make sure to attach the threshold to the dataframe 
  save(level_output_list, file=paste0('out_4branch_level_x1_', modelType, '_level', level, "_", format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'.Rda'))
  
  




