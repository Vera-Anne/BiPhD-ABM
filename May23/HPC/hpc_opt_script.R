#################################
# Small bird in winter - ABM 
# Start date: 25/04/2023
# Vera Vinken 
# Model 1_1 
#################################

##############################
#      load packages         #
##############################
library(usethis)
library(devtools)
library(truncnorm)
library(pracma)
library(ggplot2)
library(rgl)
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
source('ModelSource.R')

# checking memory usage 
#Rprof(tf<-"rprof.log", memory.profiling=TRUE)

###############################
#    USE WHEN RUNNING LOCAL   # 
###############################
# args<-c(30, 100, 8, 5, 11, NA)


#############################
#  COMMAND LINE ARGUMENTS   #
#############################
args<-commandArgs(trailingOnly=TRUE)

# Input variables 
# Number of days in the simulation 
days <- as.numeric(args[1])
# Number of agents in the simulation 
N <- as.numeric(args[2])
# Number of hours of daylight 
daylight_h <- as.numeric(args[3])
# number of thresholds 
num_th<-as.numeric(args[4])
# model type 
modelType<-as.numeric(args[5])
# OUTPUT DIR
out_dir<-args[6]

###################################
#  RUN THE ENVIRONMENT FUNCTION   # 
###################################

# Set the number of cores that you will be using (10-20 was recommended by Jaume Barcadit 04/07/2023)
numCores<-20
registerDoParallel(numCores)

# Start the if-statement and determine which model you are running 

# set up the values for which you want to optimise 
# This needs to be different for the different models 
if (modelType==00){
  # for testing 
  
}else if (modelType==11){
  
  # Set the number of thresholds you want to test for
  num_th<-num_th
  # The minimum 
  min_th_val<-0
  # And the maximum 
  max_th_val<-0.4
  # create the vector that has the actual threshold values in it 
  th_vec<-linspace( x1=min_th_val, x2=max_th_val, num_th)
  
  # Start the parallel function for all the th values   
  outcome_opt_1_1_hpc<-foreach(i=1:length(th_vec), .packages=c("foreach", "doParallel")) %dopar%{
    # Set current vector 
    cur_th<-th_vec[i]
    #print(cur_th)
    
    # put into the environment fnction 
    env_results<-env_func_1_1_hpc(days = days, N= N, th_forage_sc = cur_th, daylight_h = daylight_h, modelType=modelType)
    # Give it all a name 
    names(env_results)<-paste(c("Max_surv_"), i, sep="") # , "All_vars_perEnv_"
    
    env_results
    
  } # end parallel 
  
  # clean up cluster 
  stopImplicitCluster()
  
  # save the data 
  setwd(out_dir)
  save(outcome_opt_1_1_hpc, file='outcome_1_1_HPC.Rda')
  
} else if (modelType==12){
  
  # Set the number of options for which each threshold needs to be tested 
  num_th<-num_th
  # set the minima 
  min_th_sc1<-0
  min_th_sc2<-0
  # set the maxima
  max_th_sc1<-0.4
  max_th_sc2<-0.4
  # create the vectors
  th1_vec<-linspace(x1=min_th_sc1, x2=max_th_sc1, n=num_th)
  th2_vec<-linspace(x1=min_th_sc2, x2=max_th_sc2, n=num_th)
  # create a matrix that contains all possible combinations 
  # var 1 = th 1
  # var 2 = th 2 
  th1_th2_comb<-as.matrix(expand.grid(th1_vec, th2_vec))
  
  
  # Start the parallel function for all the th values   
  outcome_opt_1_2_hpc<-foreach(i=1:nrow(th1_th2_comb), .packages=c("foreach", "doParallel")) %dopar%{
    # Set the current thresholds 
    cur_th1<-th1_th2_comb[i,1]
    cur_th2<-th1_th2_comb[i,2]
    
    # First check if this is a sensbile threshold combination 
    if (cur_th1<cur_th2){
      # If so, run the enviornment function 
      # put into the environment fnction 
      env_results<-env_func_1_2_hpc(days = days, N= N, th_forage_sc1 = cur_th1, th_forage_sc2=cur_th2, daylight_h = daylight_h, modelType=modelType)
      # Give it all a name 
      names(env_results)<-paste(c("Max_surv_", "All_vars_perEnv_"), i, sep="")
      # Return it, so it will be included in the endresult of the optimization list 
      env_results
    } else{
      # Otherwise, fill in the lists so they contain 'NA' (this is easiest to 'ignore' when analyzing)
      # make an empty list
      env_results<-list()
      # fill in the first list (mean and sd value across all environments in above situation)
      env_results[[1]]<-c(NA, NA)
      # Fill in the second list (contains the 18 seperate lists in above situation)
      env_results[[2]]<-NA 
      # Return it to make it save 
      env_results 
    }
    
  } # end parallel 
  
  # clean up cluster 
  stopImplicitCluster()
  
  # save the data 
  setwd(out_dir)
  save(outcome_opt_1_2_hpc, file='outcome_1_2_HPC.Rda')
  
} else if (modelType==131){
  
  #Set the number of options for which each trheshold needs to be tested
  num_th<-num_th
  # set the minima
  min_th_sc1<-0
  min_th_sc2<-0
  min_th_sc3<-0
  # set the maxima
  max_th_sc1<-0.4
  max_th_sc2<-0.4
  max_th_sc3<-0.4
  # create the vectors
  th1_vec<-linspace(x1=min_th_sc1, x2=max_th_sc1, n=num_th)
  th2_vec<-linspace(x1=min_th_sc2, x2=max_th_sc2, n=num_th)
  th3_vec<-linspace(x1=min_th_sc3, x2=max_th_sc3, n=num_th)
  # create a matrix that contains all possible combinations
  # var 1 = th 1
  # var 2 = th 2
  # var 3 = th 3
  th1_th2_th3_comb<-as.matrix(expand.grid(th1_vec, th2_vec, th3_vec))
  
  # Start the parallel function for all the th values   
  outcome_opt_1_3_1_hpc<-foreach(i=1:nrow(th1_th2_th3_comb), .packages=c("foreach", "doParallel")) %dopar%{
    
    # Set the current thresholds 
    cur_th1<-th1_th2_th3_comb[i,1]
    cur_th2<-th1_th2_th3_comb[i,2]
    cur_th3<-th1_th2_th3_comb[i,3]
    
    # First check if this is a sensbile threshold combination 
    if (cur_th1<cur_th2 && cur_th2<cur_th3){
      # If so, run the enviornment function 
      # put into the environment fnction 
      env_results<-env_func_1_3_1_hpc(days = days, N= N, th_forage_sc1 = cur_th1, th_forage_sc2=cur_th2, th_forage_sc3=cur_th3, daylight_h = daylight_h, modelType=modelType)
      # Give it all a name 
      names(env_results)<-paste(c("Max_surv_", "All_vars_perEnv_"), i, sep="")
      # Return it, so it will be included in the endresult of the optimization list 
      env_results
    } else{
      # Otherwise, fill in the lists so they contain 'NA' (this is easiest to 'ignore' when analyzing)
      # make an empty list
      env_results<-list()
      # fill in the first list (mean and sd value across all environments in above situation)
      env_results[[1]]<-c(NA, NA)
      # Fill in the second list (contains the 18 seperate lists in above situation)
      env_results[[2]]<-NA 
      # Return it to make it save 
      env_results 
    }
    
  } # end parallel 
  
  # clean up cluster 
  stopImplicitCluster()
  
  # save the data 
  setwd(out_dir)
  save(outcome_opt_1_3_1_hpc, file='outcome_1_3_1_HPC.Rda')
  
  
} else if (modelType==132){
  #Set the number of options for which each trheshold needs to be tested
  num_th<-num_th
  # set the minima
  min_th_sc1<-0
  min_th_sc2<-0
  min_th_sc3<-0
  # set the maxima
  max_th_sc1<-0.4
  max_th_sc2<-0.4
  max_th_sc3<-0.4
  # create the vectors
  th1_vec<-linspace(x1=min_th_sc1, x2=max_th_sc1, n=num_th)
  th2_vec<-linspace(x1=min_th_sc2, x2=max_th_sc2, n=num_th)
  th3_vec<-linspace(x1=min_th_sc3, x2=max_th_sc3, n=num_th)
  # create a matrix that contains all possible combinations
  # var 1 = th 1
  # var 2 = th 2
  # var 3 = th 3
  th1_th2_th3_comb<-as.matrix(expand.grid(th1_vec, th2_vec, th3_vec))
  
  # Start the parallel function for all the th values   
  outcome_opt_1_3_2_hpc<-foreach(i=1:nrow(th1_th2_th3_comb), .packages=c("foreach", "doParallel")) %dopar%{
    
    # Set the current thresholds 
    cur_th1<-th1_th2_th3_comb[i,1]
    cur_th2<-th1_th2_th3_comb[i,2]
    cur_th3<-th1_th2_th3_comb[i,3]
    
    # First check if this is a sensbile threshold combination 
    if (cur_th1<cur_th2 && cur_th2<cur_th3){
      # If so, run the enviornment function 
      # put into the environment fnction 
      env_results<-env_func_1_3_2_hpc(days = days, N= N, th_forage_sc1 = cur_th1, th_forage_sc2=cur_th2, th_forage_sc3=cur_th3, daylight_h = daylight_h, modelType=modelType)
      # Give it all a name 
      names(env_results)<-paste(c("Max_surv_"), i, sep="") # , "All_vars_perEnv_"
      # Return it, so it will be included in the endresult of the optimization list 
      env_results
    } else{
      # Otherwise, fill in the lists so they contain 'NA' (this is easiest to 'ignore' when analyzing)
      # make an empty list
      env_results<-list()
      # fill in the first list (mean and sd value across all environments in above situation)
      env_results[[1]]<-c(NA, NA)
      # Fill in the second list (contains the 18 seperate lists in above situation)
      env_results[[2]]<-NA 
      # Return it to make it save 
      env_results 
    }
    
  } # end parallel 
  
  # clean up cluster 
  stopImplicitCluster()
  
  # save the data 
  setwd(out_dir)
  save(outcome_opt_1_3_2_hpc, file='outcome_1_3_2_HPC.Rda')
  
  
} else if (modelType==21){
  
  
  
} else if (modelType==22){
  
} else if(modelType==231) {
  
} else if(modelType==232) {
  
  
  
} else if(modelType=='31'){
  
}  else if(modelType=='32'){
  
  
} else if(modelType=='331'){
  
  
}else if(modelType=='332'){
  
}else if (modelType=='41'){
  
}else if(modelType=='42'){
  
  
}else if (modelType=='431'){
  
  
} else if(modelType=='432'){
  
  
}else {
  print('help stop, something is wrong with the modeltype ')
  
}

# End memory profiling 
# Rprof(NULL)
# summaryRprof(tf, memory='both')
