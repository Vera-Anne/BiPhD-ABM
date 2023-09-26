######################################################################################
# 22/08/2023
# Rscript that can run job arrays where each seperate job runs for 1 threshold combo 
# Vera Vinken 
# Last changes: 18/09/2023
######################################################################################

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
source('ModelSource.R')
#("C:/Local_R/BiPhD-ABM/May23")
# checking memory usage 
#Rprof(tf<-"rprof.log", memory.profiling=TRUE)

###############################
#    USE WHEN RUNNING LOCAL   # 
###############################
#args<-c(30, 100, 8, 5, 11, NA)


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
th_comb_numb<-as.numeric(args[4])
# model type 
modelType<-as.numeric(args[5])
# OUTPUT DIR
out_dir<-args[6]
# repeat number
rep_num<-args[7]
###################################
#  RUN THE ENVIRONMENT FUNCTION   # 
###################################


# Start the if-statement and determine which model you are running 

# set up the values for which you want to optimise 
# This needs to be different for the different models 
if (modelType==0){
  # for testing 
  
}else if (modelType==11){
  
  # Set the number of thresholds you want to test for
  num_th<-50
  # # The minimum 
  min_th_val<-0
  # # And the maximum 
  max_th_val<-0.4
  # # create the vector that has the actual threshold values in it 
  th_vec<-linspace( x1=min_th_val, x2=max_th_val, num_th)
  
  # Now determine the current threshold based on the array number
  # This comes from the variable [4] above
  # For model 11 we have a total of 50 possible numbers, so 50 different jobs will each run for a different th
  cur_th<-th_vec[th_comb_numb]
  
  # No wthe environmnets need to run in parallel 
    
    # put into the environment fnction 
    env_results<-env_func_1_1_par_hpc(days = days, N= N, th_forage_sc = cur_th, daylight_h = daylight_h, modelType=modelType)

    # This contains the results for the current threshold, through all 18 environments 
    env_results
    
  # clean up cluster 
  stopImplicitCluster()
  
  # Add some information to the output data 
  # Add the total number of th, the minima and maxima 
  args<-c(args, num_th, cur_th, min_th_val, max_th_val)
  args<-as.data.frame(t(args))
  colnames(args)<-c('days', 'N', 'day_h', 'th_comb_input', 'mod_type', 'output_dir', 'rep_num','total_num_th', 'th', 'min_th_value', 'max_th_value')
  
  #add this to the output list
  env_results[[3]]<-args
  
  # save the data 
  setwd(out_dir)
  # make sure to attach the threshold to the dataframe 
  save(env_results, file=paste0('outcome_1_1_HPC_th', th_comb_numb, '_', format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_', rep_num,'.Rda'))
  
} else if (modelType==12){
  
  # Set the number of options for which each threshold needs to be tested 
  num_th<-50
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
  th1_th2_comb<-as.data.frame(as.matrix(expand.grid(th1_vec, th2_vec)))
  colnames(th1_th2_comb)<-c('th1', 'th2')
  
  # Subset only relevant combinations
  th1_th2_relev_comb<-th1_th2_comb[(th1_th2_comb$th1<th1_th2_comb$th2),]
  
  # Now determine the current threshold based on the array number
  # This comes from the variable [4] above
  # For model 12 we have a total of 50*50=2500 possible numbers, of which 1225 relevant combinations
  # so 1225 jobs will be ran on the HPC 
  # Set the current thresholds 
  cur_th1<-th1_th2_relev_comb[th_comb_numb,1]
  cur_th2<-th1_th2_relev_comb[th_comb_numb,2]
  
  # Add some information to the output data 
  # Add the total number of th, the minima and maxima 
  args<-c(args, num_th,cur_th1, cur_th2, min_th_sc1, min_th_sc2, max_th_sc1, max_th_sc2)
  args<-as.data.frame(t(args))
  colnames(args)<-c('days', 'N', 'day_h', 'th_comb_input', 'mod_type', 'output_dir', 'rep_num', 'total_num_per_th', 'th1', 'th2','min_th_sc1', 'min_th_sc2', 'max_th_sc1', 'max_th_sc2' )
  
  
  # No wthe environmnets need to run in parallel 

  # put into the environment fnction 
  env_results<-env_func_1_2_par_hpc(days = days, N= N, th_forage_sc1 = cur_th1, th_forage_sc2=cur_th2, daylight_h = daylight_h, modelType=modelType)
  
  # This contains the results for the current threshold, through all 18 environments 
  env_results

  #add this to the output list
  env_results[[3]]<-args
  
  # save the data 
  setwd(out_dir)
  # make sure to attach the threshold to the dataframe 
  save(env_results, file=paste0('outcome_1_2_HPC_th', th_comb_numb, '_', format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_', rep_num,'.Rda'))

} else if (modelType==131){
  
  #Set the number of options for which each trheshold needs to be tested
  num_th<-50
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
  th1_th2_th3_comb<-as.data.frame(as.matrix(expand.grid(th1_vec, th2_vec, th3_vec)))
  colnames(th1_th2_th3_comb)<-c('th1', 'th2', 'th3')
  
  # Subset only relevant combinations
  th1_th2_th3_relev_comb<-th1_th2_th3_comb[(th1_th2_th3_comb$th1<th1_th2_th3_comb$th2 & th1_th2_th3_comb$th2<th1_th2_th3_comb$th3),]
  
  # Set the current thresholds 
  cur_th1<-th1_th2_th3_relev_comb[th_comb_numb,1]
  cur_th2<-th1_th2_th3_relev_comb[th_comb_numb,2]
  cur_th3<-th1_th2_th3_relev_comb[th_comb_numb,3]
  
  # Add some information to the output data 
  # Add the total number of th, the minima and maxima 
  args<-c(args, num_th, cur_th1, cur_th2, cur_th3,min_th_sc1, min_th_sc2, min_th_sc3, max_th_sc1,max_th_sc2, max_th_sc3)
  args<-as.data.frame(t(args))
  colnames(args)<-c('days', 'N', 'day_h', 'th_comb_input', 'mod_type',  'output_dir', 'rep_num' , 'total_num_per_th','th1', 'th2', 'th3', 'min_th_sc1', 'min_th_sc2', 'min_th_sc3', 'max_th_sc1', 'max_th_sc2', 'max_th_sc3')
  

      # put into the environment fnction 
      env_results<-env_func_1_3_1_par_hpc(days = days, N= N, th_forage_sc1 = cur_th1, th_forage_sc2=cur_th2, th_forage_sc3=cur_th3, daylight_h = daylight_h, modelType=modelType)
      # Return it, so it will be included in the endresult of the optimization list 
      env_results
    
  #add this to the output list
  env_results[[3]]<-args
  
    # save the data 
  setwd(out_dir)
  save(env_results, file=paste0('outcome_1_3_1_HPC_th', th_comb_numb, '_', format(Sys.time(), "%Y-%m-%d_%H_%M_%S_%OS3"),'_', rep_num,'.Rda'))
  
  
} else if (modelType==132){
  #Set the number of options for which each trheshold needs to be tested
  num_th<-50
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
  th1_th2_th3_comb<-as.data.frame(as.matrix(expand.grid(th1_vec, th2_vec, th3_vec)))
  colnames(th1_th2_th3_comb)<-c('th1', 'th2', 'th3')
  
  # Subset only relevant combinations
  th1_th2_th3_relev_comb<-th1_th2_th3_comb[(th1_th2_th3_comb$th1<th1_th2_th3_comb$th2 & th1_th2_th3_comb$th2<th1_th2_th3_comb$th3),]
  
  # Set the current thresholds 
  cur_th1<-th1_th2_th3_relev_comb[th_comb_numb,1]
  cur_th2<-th1_th2_th3_relev_comb[th_comb_numb,2]
  cur_th3<-th1_th2_th3_relev_comb[th_comb_numb,3]
  
  # Add some information to the output data 
  # Add the total number of th, the minima and maxima 
  args<-c(args, num_th, cur_th1, cur_th2, cur_th3,min_th_sc1, min_th_sc2, min_th_sc3, max_th_sc1,max_th_sc2, max_th_sc3)
  args<-as.data.frame(t(args))
  colnames(args)<-c('days', 'N', 'day_h', 'th_comb_input', 'mod_type',  'output_dir', 'rep_num' , 'total_num_per_th','th1', 'th2', 'th3', 'min_th_sc1', 'min_th_sc2', 'min_th_sc3', 'max_th_sc1', 'max_th_sc2', 'max_th_sc3' )
  
  
  # put into the environment fnction 
  env_results<-env_func_1_3_2_par_hpc(days = days, N= N, th_forage_sc1 = cur_th1, th_forage_sc2=cur_th2, th_forage_sc3=cur_th3, daylight_h = daylight_h, modelType=modelType)
  # Return it, so it will be included in the endresult of the optimization list 
  env_results
  
  #add this to the output list
  env_results[[3]]<-args
  
  # save the data 
  setwd(out_dir)
  save(env_results, file=paste0('outcome_1_3_2_HPC_th', th_comb_numb,'_', format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_', rep_num, '.Rda'))
  
  
} else if (modelType==21){
  
  # Set the number of thresholds you want to test for
  num_th<-50
  # # The minimum 
  min_th_val<-0
  # # And the maximum 
  max_th_val<-4
  # # create the vector that has the actual threshold values in it 
  th_vec<-linspace( x1=min_th_val, x2=max_th_val, num_th)
  
  # Now determine the current threshold based on the array number
  # This comes from the variable [4] above
  # For model 11 we have a total of 50 possible numbers, so 50 different jobs will each run for a different th
  cur_th<-th_vec[th_comb_numb]
  
  # No wthe environmnets need to run in parallel 
  
  # put into the environment fnction 
  env_results<-env_func_2_1_par_hpc(days = days, N= N, th_forage_fr = cur_th, daylight_h = daylight_h, modelType=modelType)
  
  # This contains the results for the current threshold, through all 18 environments 
  env_results
  
  # clean up cluster 
  stopImplicitCluster()
  
  
  # Add some information to the output data 
  # Add the total number of th, the minima and maxima 
  args<-c(args, num_th, cur_th, min_th_val, max_th_val)
  args<-as.data.frame(t(args))
  colnames(args)<-c('days', 'N', 'day_h', 'th_comb_input', 'mod_type', 'output_dir', 'rep_num', 'total_num_th', 'th', 'min_th_value', 'max_th_value')
  
  #add this to the output list
  env_results[[3]]<-args
  
  # save the data 
  setwd(out_dir)
  # make sure to attach the threshold to the dataframe 
  save(env_results, file=paste0('outcome_2_1_HPC_th', th_comb_numb,'_', format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_', rep_num, '.Rda'))
  
  
} else if (modelType==22){
  
  # Set the number of options for which each threshold needs to be tested 
  num_th<-50
  # set the minima 
  min_th_fr1<-0
  min_th_fr2<-0
  # set the maxima
  max_th_fr1<-4
  max_th_fr2<-4
  # create the vectors
  th1_vec<-linspace(x1=min_th_fr1, x2=max_th_fr1, n=num_th)
  th2_vec<-linspace(x1=min_th_fr2, x2=max_th_fr2, n=num_th)
  # create a matrix that contains all possible combinations 
  # var 1 = th 1
  # var 2 = th 2 
  th1_th2_comb<-as.data.frame(as.matrix(expand.grid(th1_vec, th2_vec)))
  colnames(th1_th2_comb)<-c('th1', 'th2')
  
  # Subset only relevant combinations
  th1_th2_relev_comb<-th1_th2_comb[(th1_th2_comb$th1<th1_th2_comb$th2),]
  
  # Now determine the current threshold based on the array number
  # This comes from the variable [4] above
  # For model 12 we have a total of 50*50=2500 possible numbers, so 2500 different jobs will each run for a different th
  # Set the current thresholds 
  cur_th1<-th1_th2_relev_comb[th_comb_numb,1]
  cur_th2<-th1_th2_relev_comb[th_comb_numb,2]
  
  # Add some information to the output data 
  # Add the total number of th, the minima and maxima 
  args<-c(args, num_th,cur_th1, cur_th2, min_th_fr1, min_th_fr2, max_th_fr1, max_th_fr2)
  args<-as.data.frame(t(args))
  colnames(args)<-c('days', 'N', 'day_h', 'th_comb_input', 'mod_type', 'output_dir','rep_num', 'total_num_per_th', 'th1', 'th2','min_th_fr1', 'min_th_fr2', 'max_th_fr1', 'max_th_fr2' )
  
  
  # No wthe environmnets need to run in parallel 
  
  # put into the environment fnction 
  env_results<-env_func_2_2_par_hpc(days = days, N= N, th_forage_fr1 = cur_th1, th_forage_fr2=cur_th2, daylight_h = daylight_h, modelType=modelType)
  
  # This contains the results for the current threshold, through all 18 environments 
  env_results
  
  #add this to the output list
  env_results[[3]]<-args
  
  # save the data 
  setwd(out_dir)
  # make sure to attach the threshold to the dataframe 
  save(env_results, file=paste0('outcome_2_2_HPC_th', th_comb_numb,'_', format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_', rep_num, '.Rda'))
  
  
} else if(modelType==231) {
  
  #Set the number of options for which each trheshold needs to be tested
  num_th<-50
  # set the minima
  min_th_fr1<-0
  min_th_fr2<-0
  min_th_fr3<-0
  # set the maxima
  max_th_fr1<-4
  max_th_fr2<-4
  max_th_fr3<-4
  # create the vectors
  th1_vec<-linspace(x1=min_th_fr1, x2=max_th_fr1, n=num_th)
  th2_vec<-linspace(x1=min_th_fr2, x2=max_th_fr2, n=num_th)
  th3_vec<-linspace(x1=min_th_fr3, x2=max_th_fr3, n=num_th)
  # create a matrix that contains all possible combinations
  # var 1 = th 1
  # var 2 = th 2
  # var 3 = th 3
  th1_th2_th3_comb<-as.data.frame(as.matrix(expand.grid(th1_vec, th2_vec, th3_vec)))
  colnames(th1_th2_th3_comb)<-c('th1', 'th2', 'th3')
  
  # Subset only relevant combinations
  th1_th2_th3_relev_comb<-th1_th2_th3_comb[(th1_th2_th3_comb$th1<th1_th2_th3_comb$th2 & th1_th2_th3_comb$th2<th1_th2_th3_comb$th3),]
  
  # Set the current thresholds 
  cur_th1<-th1_th2_th3_relev_comb[th_comb_numb,1]
  cur_th2<-th1_th2_th3_relev_comb[th_comb_numb,2]
  cur_th3<-th1_th2_th3_relev_comb[th_comb_numb,3]
  
  # Add some information to the output data 
  # Add the total number of th, the minima and maxima 
  args<-c(args, num_th, cur_th1, cur_th2, cur_th3,min_th_fr1, min_th_fr2, min_th_fr3, max_th_fr1,max_th_fr2, max_th_fr3)
  args<-as.data.frame(t(args))
  colnames(args)<-c('days', 'N', 'day_h', 'th_comb_input', 'mod_type',  'output_dir', 'rep_num' , 'total_num_per_th','th1', 'th2', 'th3', 'min_th_fr1', 'min_th_fr2', 'min_th_fr3', 'max_th_fr1', 'max_th_fr2', 'max_th_fr3' )
  
  
  # put into the environment fnction 
  env_results<-env_func_2_3_1_par_hpc(days = days, N= N, th_forage_fr1 = cur_th1, th_forage_fr2=cur_th2, th_forage_fr3=cur_th3, daylight_h = daylight_h, modelType=modelType)
  # Return it, so it will be included in the endresult of the optimization list 
  env_results
  
  #add this to the output list
  env_results[[3]]<-args
  
  # save the data 
  setwd(out_dir)
  save(env_results, file=paste0('outcome_2_3_1_HPC_th', th_comb_numb,'_', format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_', rep_num, '.Rda'))
  
} else if(modelType==232) {
  #Set the number of options for which each trheshold needs to be tested
  num_th<-50
  # set the minima
  min_th_fr1<-0
  min_th_fr2<-0
  min_th_fr3<-0
  # set the maxima
  max_th_fr1<-4
  max_th_fr2<-4
  max_th_fr3<-4
  # create the vectors
  th1_vec<-linspace(x1=min_th_fr1, x2=max_th_fr1, n=num_th)
  th2_vec<-linspace(x1=min_th_fr2, x2=max_th_fr2, n=num_th)
  th3_vec<-linspace(x1=min_th_fr3, x2=max_th_fr3, n=num_th)
  # create a matrix that contains all possible combinations
  # var 1 = th 1
  # var 2 = th 2
  # var 3 = th 3
  th1_th2_th3_comb<-as.data.frame(as.matrix(expand.grid(th1_vec, th2_vec, th3_vec)))
  colnames(th1_th2_th3_comb)<-c('th1', 'th2', 'th3')
  
  # Subset only relevant combinations
  th1_th2_th3_relev_comb<-th1_th2_th3_comb[(th1_th2_th3_comb$th1<th1_th2_th3_comb$th2 & th1_th2_th3_comb$th2<th1_th2_th3_comb$th3),]
  
  # Set the current thresholds 
  cur_th1<-th1_th2_th3_relev_comb[th_comb_numb,1]
  cur_th2<-th1_th2_th3_relev_comb[th_comb_numb,2]
  cur_th3<-th1_th2_th3_relev_comb[th_comb_numb,3]
  
  # Add some information to the output data 
  # Add the total number of th, the minima and maxima 
  args<-c(args, num_th, cur_th1, cur_th2, cur_th3, min_th_fr1, min_th_fr2, min_th_fr3, max_th_fr1, max_th_fr2, max_th_fr3)
  args<-as.data.frame(t(args))
  colnames(args)<-c('days', 'N', 'day_h', 'th_comb_input', 'mod_type',  'output_dir' , 'rep_num', 'total_num_per_th','th1', 'th2', 'th3', 'min_th_fr1', 'min_th_fr2', 'min_th_fr3', 'max_th_fr1', 'max_th_fr2', 'max_th_fr3')
  
    # put into the environment fnction 
  env_results<-env_func_2_3_2_par_hpc(days = days, N= N, th_forage_fr1 = cur_th1, th_forage_fr2=cur_th2, th_forage_fr3=cur_th3, daylight_h = daylight_h, modelType=modelType)
  # Return it, so it will be included in the endresult of the optimization list 
  env_results
  
  #add this to the output list
  env_results[[3]]<-args
  
  # save the data 
  setwd(out_dir)
  save(env_results, file=paste0('outcome_2_3_2_HPC_th', th_comb_numb, '_', format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_', rep_num,'.Rda'))
  
} else if(modelType=='31'){
  
  # Set the number of thresholds you want to test for
  num_th<-50
  # The minimum 
  min_th_val<-(-0.6)
  # And the maximum 
  max_th_val<-0.6
  # # create the vector that has the actual threshold values in it 
  th_vec<-linspace( x1=min_th_val, x2=max_th_val, num_th)
  
  # Now determine the current threshold based on the array number
  # This comes from the variable [4] above
  # For model 11 we have a total of 50 possible numbers, so 50 different jobs will each run for a different th
  cur_th<-th_vec[th_comb_numb]
  
  # No wthe environmnets need to run in parallel 
  
  # put into the environment fnction 
  env_results<-env_func_3_1_par_hpc(days = days, N= N, th_forage_flr = cur_th, daylight_h = daylight_h, modelType=modelType)
  
  # This contains the results for the current threshold, through all 18 environments 
  env_results
  
  # clean up cluster 
  stopImplicitCluster()
  
  
  # Add some information to the output data 
  # Add the total number of th, the minima and maxima 
  args<-c(args, num_th, cur_th, min_th_val, max_th_val)
  args<-as.data.frame(t(args))
  colnames(args)<-c('days', 'N', 'day_h', 'th_comb_input', 'mod_type', 'output_dir', 'rep_num', 'total_num_th', 'th', 'min_th_value', 'max_th_value')
  
  #add this to the output list
  env_results[[3]]<-args
  
  # save the data 
  setwd(out_dir)
  # make sure to attach the threshold to the dataframe 
  save(env_results, file=paste0('outcome_3_1_HPC_th', th_comb_numb,'_', format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_', rep_num, '.Rda'))
  
  
}  else if(modelType=='32'){
  # Set the number of options for which each threshold needs to be tested 
  num_th<-50
  # set the minima 
  min_th_flr1<-(-0.6)
  min_th_flr2<-(-0.6)
  # set the maxima
  max_th_flr1<-0.6
  max_th_flr2<-0.6
  # create the vectors
  th1_vec<-linspace(x1=min_th_flr1, x2=max_th_flr1, n=num_th)
  th2_vec<-linspace(x1=min_th_flr2, x2=max_th_flr2, n=num_th)
  # create a matrix that contains all possible combinations 
  # var 1 = th 1
  # var 2 = th 2 
  th1_th2_comb<-as.data.frame(as.matrix(expand.grid(th1_vec, th2_vec)))
  colnames(th1_th2_comb)<-c('th1', 'th2')
  
  # Subset only relevant combinations
  th1_th2_relev_comb<-th1_th2_comb[(th1_th2_comb$th1<th1_th2_comb$th2),]
  
  # Now determine the current threshold based on the array number
  # This comes from the variable [4] above
  # For model 12 we have a total of 50*50=2500 possible numbers, so 2500 different jobs will each run for a different th
  # Set the current thresholds 
  cur_th1<-th1_th2_relev_comb[th_comb_numb,1]
  cur_th2<-th1_th2_relev_comb[th_comb_numb,2]
  
  # Add some information to the output data 
  # Add the total number of th, the minima and maxima 
  args<-c(args, num_th,cur_th1, cur_th2, min_th_flr1, min_th_flr2, max_th_flr1, max_th_flr2)
  args<-as.data.frame(t(args))
  colnames(args)<-c('days', 'N', 'day_h', 'th_comb_input', 'mod_type', 'output_dir', 'rep_num', 'total_num_per_th', 'th1', 'th2','min_th_flr1', 'min_th_flr2', 'max_th_flr1', 'max_th_flr2' )
  
  # put into the environment fnction 
  env_results<-env_func_3_2_par_hpc(days = days, N= N, th_forage_flr1 = cur_th1, th_forage_flr2=cur_th2, daylight_h = daylight_h, modelType=modelType)
  
  # This contains the results for the current threshold, through all 18 environments 
  env_results
  
  #add this to the output list
  env_results[[3]]<-args
  
  # save the data 
  setwd(out_dir)
  # make sure to attach the threshold to the dataframe 
  save(env_results, file=paste0('outcome_3_2_HPC_th', th_comb_numb, '_', format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_', rep_num,'.Rda'))
  
} else if(modelType=='331'){
  #Set the number of options for which each trheshold needs to be tested
  num_th<-50
  # set the minima
  min_th_flr1<-(-0.6)
  min_th_flr2<-(-0.6)
  min_th_flr3<-(-0.6)
  # set the maxima
  max_th_flr1<-0.6
  max_th_flr2<-0.6
  max_th_flr3<-0.6
  # create the vectors
  th1_vec<-linspace(x1=min_th_flr1, x2=max_th_flr1, n=num_th)
  th2_vec<-linspace(x1=min_th_flr2, x2=max_th_flr2, n=num_th)
  th3_vec<-linspace(x1=min_th_flr3, x2=max_th_flr3, n=num_th)
  # create a matrix that contains all possible combinations
  # var 1 = th 1
  # var 2 = th 2
  # var 3 = th 3
  th1_th2_th3_comb<-as.data.frame(as.matrix(expand.grid(th1_vec, th2_vec, th3_vec)))
  colnames(th1_th2_th3_comb)<-c('th1', 'th2', 'th3')
  
  # Subset only relevant combinations
  th1_th2_th3_relev_comb<-th1_th2_th3_comb[(th1_th2_th3_comb$th1<th1_th2_th3_comb$th2 & th1_th2_th3_comb$th2<th1_th2_th3_comb$th3),]
  
  # Set the current thresholds 
  cur_th1<-th1_th2_th3_relev_comb[th_comb_numb,1]
  cur_th2<-th1_th2_th3_relev_comb[th_comb_numb,2]
  cur_th3<-th1_th2_th3_relev_comb[th_comb_numb,3]
  
  # Add some information to the output data 
  # Add the total number of th, the minima and maxima 
  args<-c(args, num_th, cur_th1, cur_th2, cur_th3, min_th_flr1, min_th_flr2, min_th_flr3, max_th_flr1, max_th_flr2, max_th_flr3)
  args<-as.data.frame(t(args))
  colnames(args)<-c('days', 'N', 'day_h', 'th_comb_input', 'mod_type',  'output_dir', 'rep_num', 'total_num_per_th','th1', 'th2', 'th3', 'min_th_flr1', 'min_th_flr2', 'min_th_flr3', 'max_th_flr1', 'max_th_flr2', 'max_th_flr3' )
  
  
  # put into the environment fnction 
  env_results<-env_func_3_3_1_par_hpc(days = days, N= N, th_forage_flr1 = cur_th1, th_forage_flr2=cur_th2, th_forage_flr3=cur_th3, daylight_h = daylight_h, modelType=modelType)
  # Return it, so it will be included in the endresult of the optimization list 
  env_results
  
  #add this to the output list
  env_results[[3]]<-args
  
  # save the data 
  setwd(out_dir)
  save(env_results, file=paste0('outcome_3_3_1_HPC_th', th_comb_numb,'_', format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_', rep_num, '.Rda'))
  
}else if(modelType=='332'){
  
  #Set the number of options for which each trheshold needs to be tested
  num_th<-50
  # set the minima
  min_th_flr1<-(-0.6)
  min_th_flr2<-(-0.6)
  min_th_flr3<-(-0.6)
  # set the maxima
  max_th_flr1<-0.6
  max_th_flr2<-0.6
  max_th_flr3<-0.6
  # create the vectors
  th1_vec<-linspace(x1=min_th_flr1, x2=max_th_flr1, n=num_th)
  th2_vec<-linspace(x1=min_th_flr2, x2=max_th_flr2, n=num_th)
  th3_vec<-linspace(x1=min_th_flr3, x2=max_th_flr3, n=num_th)
  # create a matrix that contains all possible combinations
  # var 1 = th 1
  # var 2 = th 2
  # var 3 = th 3
  th1_th2_th3_comb<-as.data.frame(as.matrix(expand.grid(th1_vec, th2_vec, th3_vec)))
  colnames(th1_th2_th3_comb)<-c('th1', 'th2', 'th3')
  
  # Subset only relevant combinations
  th1_th2_th3_relev_comb<-th1_th2_th3_comb[(th1_th2_th3_comb$th1<th1_th2_th3_comb$th2 & th1_th2_th3_comb$th2<th1_th2_th3_comb$th3),]
  
  # Set the current thresholds 
  cur_th1<-th1_th2_th3_relev_comb[th_comb_numb,1]
  cur_th2<-th1_th2_th3_relev_comb[th_comb_numb,2]
  cur_th3<-th1_th2_th3_relev_comb[th_comb_numb,3]
  
  # Add some information to the output data 
  # Add the total number of th, the minima and maxima 
  args<-c(args, num_th, cur_th1, cur_th2, cur_th3, min_th_flr1, min_th_flr2, min_th_flr3, max_th_flr1, max_th_flr2, max_th_flr3)
  args<-as.data.frame(t(args))
  colnames(args)<-c('days', 'N', 'day_h', 'th_comb_input', 'mod_type',  'output_dir', 'rep_num', 'total_num_per_th','th1', 'th2', 'th3', 'min_th_flr1', 'min_th_flr2', 'min_th_flr3', 'max_th_flr1', 'max_th_flr2', 'max_th_flr3' )
  
  # put into the environment fnction 
  env_results<-env_func_3_3_2_par_hpc(days = days, N= N, th_forage_flr1 = cur_th1, th_forage_flr2=cur_th2, th_forage_flr3=cur_th3, daylight_h = daylight_h, modelType=modelType)
  # Return it, so it will be included in the endresult of the optimization list 
  env_results
  
  #add this to the output list
  env_results[[3]]<-args
  
  # save the data 
  setwd(out_dir)
  save(env_results, file=paste0('outcome_3_3_2_HPC_th', th_comb_numb,'_', format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_', rep_num, '.Rda'))
  
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
