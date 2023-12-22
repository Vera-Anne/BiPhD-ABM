######################################################################################
# 21/12/2023
# Rscript that runs the binary branching for models x.2 - Version 2, as the first isnt working 
# Vera Vinken 
# Last changes: 21/12/2023
######################################################################################

# Goal is to create a script that can run per level
# This should reduce memory requirement and hopefully incrase speed for running. 
# Using code from the x.1 version 2 script and from the function in the funcSource file. 

##############################
#      load packages         #
##############################
library(usethis)
library(devtools)
library(truncnorm)
library(pracma)
library(ggplot2)
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
source('ModelSource.R')


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
  range_min_start_1 <- as.numeric(args[3])
  range_max_start_1 <- as.numeric(args[4])
  
  range_min_start_2<-as.numeric(args[5])
  range_max_start_2<-as.numeric(args[6])
  
  # set the level 
  level<-as.numeric(args[7])

  # Number of hours of daylight 
  daylight_h<- as.numeric(args[8])
  
  # model type 
  modelType<-as.numeric(args[9])
  
  # Output directory 
  out_dir<-args[10]


##########################
##    BRANCHING         ##
##########################

# Run the branch function 
  vals_th1<-branch_func(range_min = range_min_start_1, range_max = range_max_start_1, branch_num = 4)
  # save the stepsize used for this
  step_size_th1<-step_size
  # run for second threshold 
  vals_th2<-branch_func(range_min = range_min_start_2, range_max = range_max_start_2, branch_num = 4)
  # save the stepsize for this 
  step_size_th2<-step_size
  
  # Generate the possible combinations 
  # Take out the stepsize 
  vals_th1<-vals_th1[1:4]
  vals_th2<-vals_th2[1:4]
  
  # Generate a grid with the possible combinations 
  grid<-expand.grid(vals_th1, vals_th2)
  names<-c("th1", "th2")
  colnames(grid)<-names
  
  # For models x.1 of the first subset, th2 needs to be higher than th1 
  grid<-grid[grid$th1<grid$th2, ]
  
  # Now run the model 
  for (i in 1:nrow(grid)){
    if (i==1){
      # Create empty list for results 
      level_results<-list()
    }
    # Set current thresholds 
    cur_th1<-grid[i,1]
    cur_th2<-grid[i,2]
    
    # Run the model 25 times in the 12 environments and take the average
    for (j in 1:25){
      if (j==1){
        out_per_run_list<-list()
      }
      # Run the model 
      if(modelType==12){
        env_func_1_2_par_hpc(days = days, N=N , th_forage_sc1 = cur_th1 , th_forage_sc2 = cur_th2 , daylight_h = daylight_h, modelType = 12)
      }else if(modelType==22){
        env_func_2_2_par_hpc(days = days, N= N, th_forage_fr1 = cur_th1 , th_forage_fr2 = cur_th2 , daylight_h = daylight_h, modelType = 22)
      }else if(modelType==32){
        env_func_3_2_par_hpc(days = days, N= N, th_forage_flr1 = cur_th1 , th_forage_flr2 = cur_th2 , daylight_h = daylight_h, modelType = 32)
      }else{
        print('problem with model_type settings')
      }
      out_per_run_list[j]<-cbind(output_env_func[[1]][1], cur_th1, cur_th2)
      print(paste(j))
    } # end of loop for 25 run s
    
    # bind together
    out_per_run_df<-data.frame(matrix(unlist(out_per_run_list), nrow=25, byrow=TRUE),stringsAsFactors=FALSE)
    colnames(out_per_run_df)<-"HL"
    # Now take the average for the threshold 
    mean_perf<-c(mean(out_per_run_df$HL), cur_th1, cur_th2)
    # add to list
    level_results[[i]]<-mean_perf
    # mark that things are working
    print(paste("Done with level 1 ", i))
  } # end of 1-4 branching loop
  
  halflife_out<-data.frame(matrix(unlist(level_results), nrow=nrow(grid), byrow=TRUE),stringsAsFactors=FALSE)
  names<-c("HL",  "th1", "th2")
  colnames(halflife_out)<-names
  
  # select highest outcome
  max_HL<-max(halflife_out$HL)
  best<-halflife_out[halflife_out$HL==max_HL, ]
  #best_ths_l1<-best_l1[1,2]
  
  # Calcualte teh ranges for the next level 

  # For threshold 1 
  range_min_next_th1<-(best$th1-(step_size_th1/2))
  range_max_next_th1<-(best$th1+(step_size_th1/2))
  # Same for threshold 2
  range_min_next_th2<-(best$th2-(step_size_th2/2))
  range_max_next_th2<-(best$th2+(step_size_th2/2))
  
  # Bind together 

  next_range<-cbind(range_min_next_th1, range_max_next_th1, range_min_next_th2, range_max_next_th2)
  
  # Now connect all the results together 
  level_output_list<-list(halflife_out, best, next_range)
  
#####################
#   SAVE RESULTS    # 
#####################
  
# set directory 
  setwd(out_dir)
# make sure to attach the threshold to the dataframe 
  save(level_output_list, file=paste0('out_4branch_level_x2_', modelType, '_level', level, "_", format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'.Rda'))
  
  


