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
source('ModelSource_HPC.R')

#############################
#  COMMAND LINE ARGUMENTS   #
#############################
args<-commandArgs(trailingOnly=TRUE)
# hold on to the old ones for now 
args_old<-args

# Input variables 
    # Number of days in the simulation 
    days <- as.numeric(args[1])
    # Number of agents in the simulation 
    N <- as.numeric(args[2])
    # Number of hours of daylight 
    daylight_h <- as.numeric(args[3])
    # model type 
    modelType<-as.numeric(args[4])
    # level 
    level<-as.numeric(args[5])
    # OUTPUT DIR
    out_dir<-args[6]
    # The array_ID, this will be equal to the row number in the dataframe with threshold combinations 
    th_row<-as.numeric(args[7])
    # repeat number
    rep_num<-as.numeric(args[8])
    # thresholds --> this will create an array as long as the number of thresholds 
    thresholds<-as.numeric((args[9:length(args)]))

args<-c(days, N, daylight_h, modelType, level, out_dir, th_row, rep_num, thresholds)

###################################
#  RUN THE ENVIRONMENT FUNCTION   # 
###################################

# Some checks for debugging (can be removed later)
print(paste("length of args_old=",length(args_old)))
print(paste("this is R - thresholds=", thresholds))
print(paste("level=", level))
print(paste("threshold row =", th_row))

# Start the if-statement and determine which model you are running 

if (modelType==0){
  # for testing 
  print("something is wrong with teh modeltype = 0 ")
  print(paste(thresholds))
  
}else if (modelType==11|modelType==21|modelType==31){
    # modeltype 11 is the SC non-hoardig bird with 1 variable
  # The only relevant variable is v1_th1
  cur_th<-thresholds[1]
 
        if(modelType==11){
          # run evnironment function 
          env_results<-env_func_1_1_par_hpc(days = days, N= N, th_forage_sc = cur_th, daylight_h = daylight_h, modelType=modelType)
        }else if(modelType==21){
          # run evnironment function 
          env_results<-env_func_2_1_par_hpc(days = days, N= N, th_forage_fr = cur_th, daylight_h = daylight_h, modelType=modelType)
        } else if(modelType==31){
          # run environment function 
          env_results<-env_func_3_1_par_hpc(days = days, N= N, th_forage_flr = cur_th, daylight_h = daylight_h, modelType=modelType)
        }else{
          print("Problem: modeltype is not set correctly, but within x.1 subset 1")
          }
  
  # This contains the results for the current threshold, through all 18 environments (I've commented this out)
  env_results
  # clean up cluster 
  stopImplicitCluster()
  # Add some information to the output data 
  args<-as.data.frame(t(args))
  colnames(args)<-c('days', 'N', 'day_h', 'mod_type', "level", 'output_dir', "th_row", 'rep_num', "cur_th")
  
  #add this to the output list
  env_results[[3]]<-args
  # save the data 
  setwd(out_dir)
  # make sure to attach the threshold to the dataframe 
  save(env_results, file=paste0('out_', modelType, '_HPC_', format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),"_lev", level, '_THrow', th_row,"_rep", rep_num,'.Rda'))
  
} else if (modelType==12|modelType==22|modelType==32){
  
  cur_th1<-thresholds[1]
  print(paste("th1 =", cur_th1))
  cur_th2<-thresholds[2]
  print(paste("th2=", cur_th2))
  
  # Add some information to the output data 
  args<-as.data.frame(t(args))
  colnames(args)<-c('days', 'N', 'day_h', 'mod_type', "level", 'output_dir', "th_row",'rep_num',"cur_th1", "cur_th2")
  
      
        if(modelType==12){
          # run evnironment function 
          env_results<-env_func_1_2_par_hpc(days = days, N= N, th_forage_sc1 = cur_th1, th_forage_sc2=cur_th2, daylight_h = daylight_h, modelType=modelType)
        }else if(modelType==22){
          # run evnironment function 
          env_results<-env_func_2_2_par_hpc(days = days, N= N, th_forage_fr1 = cur_th1, th_forage_fr2=cur_th2, daylight_h = daylight_h, modelType=modelType)
        }else if(modelType==32){
          # run environment function 
          env_results<-env_func_3_2_par_hpc(days = days, N= N, th_forage_flr1 = cur_th1, th_forage_flr2=cur_th2, daylight_h = daylight_h, modelType=modelType)
        }else{
          print("Problem: modeltype is not set correctly, but within x.2 subset 1")
        }
  
  # This contains the results for the current threshold, through all 18 environments 
  env_results
  
  # clean up cluster 
  stopImplicitCluster()
  
  #add this to the output list
  env_results[[3]]<-args
  
  # save the data 
  setwd(out_dir)
  # make sure to attach the threshold to the dataframe 
  save(env_results, file=paste0('out_', modelType, '_HPC_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),"_lev", level, '_THrow', th_row,"_rep", rep_num,'.Rda'))
  
} else if (modelType==131|modelType==132|modelType==231|modelType==232|modelType==331|modelType==332){
  # This is a 3 trheshold model - SC - subset 1 - direct hoarder / hoarding up top 
  # Set the thresholds 
  cur_th1<-thresholds[1]
  cur_th2<-thresholds[2]
  cur_th3<-thresholds[3]
  
  # Add some information to the output data 
  args<-as.data.frame(t(args))
  colnames(args)<-c('days', 'N', 'day_h', 'mod_type', "level", 'output_dir', "th_row", 'rep_num',"cur_th1", "cur_th2", "cur_th3")
  
  
      if(modelType==131){
        # run evnironment function 
        env_results<-env_func_1_3_1_par_hpc(days = days, N= N, th_forage_sc1 = cur_th1, th_forage_sc2=cur_th2, 
                                            th_forage_sc3=cur_th3, daylight_h = daylight_h, modelType=modelType)
      }else if(modelType==132){
        # run evnironment function 
        env_results<-env_func_1_3_2_par_hpc(days = days, N= N, th_forage_sc1 = cur_th1, th_forage_sc2=cur_th2, 
                                            th_forage_sc3=cur_th3, daylight_h = daylight_h, modelType=modelType)
      }else if(modelType==231){
        # run environment function
        env_results<-env_func_2_3_1_par_hpc(days = days, N= N, th_forage_fr1 = cur_th1, th_forage_fr2=cur_th2, 
                                            th_forage_fr3=cur_th3, daylight_h = daylight_h, modelType=modelType)
      }else if(modelType==232){
        # put into the environment fnction 
        env_results<-env_func_2_3_2_par_hpc(days = days, N= N, th_forage_fr1 = cur_th1, th_forage_fr2=cur_th2, 
                                            th_forage_fr3=cur_th3, daylight_h = daylight_h, modelType=modelType)
      }else if(modelType==331){
        # put into the environment fnction 
        env_results<-env_func_3_3_1_par_hpc(days = days, N= N, th_forage_flr1 = cur_th1, th_forage_flr2=cur_th2, 
                                            th_forage_flr3=cur_th3, daylight_h = daylight_h, modelType=modelType)
      }else if (modelType==332){
        # put into the environment fnction 
        env_results<-env_func_3_3_2_par_hpc(days = days, N= N, th_forage_flr1 = cur_th1, th_forage_flr2=cur_th2, 
                                            th_forage_flr3=cur_th3, daylight_h = daylight_h, modelType=modelType)
      }else{
        print("Problem: modeltype is not set correctly, but within x.3 subset 1")
      }
      
      
  # Return it, so it will be included in the endresult of the optimization list 
  env_results
  #add this to the output list
  env_results[[3]]<-args
  # save the data 
  setwd(out_dir)
  save(env_results, file=paste0('out_', modelType, '_HPC_', format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),"_lev", level, '_THrow', th_row,"_rep", rep_num,'.Rda'))
  
  
} else if (modelType==41|modelType==51|modelType==61){
  
  
  # No wthe environmnets need to run in parallel 
  
  # put into the environment fnction 
  env_results<-env_func_4_1_par_hpc(days = days, N= N, th_forage_fr = cur_th11, th_forage_flr=cur_th21, daylight_h = daylight_h, modelType=modelType)
  
  # This contains the results for the current threshold, through all 18 environments 
  env_results
  
  #add this to the output list
  env_results[[3]]<-args
  
  # save the data 
  setwd(out_dir)
  # make sure to attach the threshold to the dataframe 
  save(env_results, file=paste0('outcome_4_1_HPC_th', th_comb_numb, '_', format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_', rep_num,'.Rda'))
  
  
}else if(modelType=='42'){
  
  # Set the number of options for which each threshold needs to be tested 
  num_th<-50
  # set the minima 
  # For 4.1 the first energy proxy is FR
  # For 4.1 the second energy proxy is FLR 
  min_th_11<-0
  min_th_12<-0
  min_th_21<-(-0.6)
  min_th_22<-(-0.6)
  # set the maxima
  max_th_11<-4
  max_th_12<-4
  max_th_21<-0.6
  max_th_22<-0.6
  # create the vectors
  th11_vec<-linspace(x1=min_th_11, x2=max_th_11, n=num_th)
  th12_vec<-linspace(x1=min_th_12, x2=max_th_12, n=num_th)
  th21_vec<-linspace(x1=min_th_21, x2=max_th_21, n=num_th)
  th22_vec<-linspace(x1=min_th_22, x2=max_th_22, n=num_th)
  # create a matrix that contains all possible combinations 
  th_comb_4<-as.data.frame(as.matrix(expand.grid(th11_vec, th12_vec, th21_vec, th22_vec)))
  colnames(th_comb_4)<-c('th11', 'th12', 'th21', 'th22')
  # Select the relevant combinations
  relev_th_comb_4<-th_comb_4[(th_comb_4$th11<th_comb_4$th12 & th_comb_4$th21<th_comb_4$th22),]
  
  # Set the current thresholds 
  cur_th11<-relev_th_comb_4[th_comb_numb,1]
  cur_th12<-relev_th_comb_4[th_comb_numb,2]
  cur_th21<-relev_th_comb_4[th_comb_numb,3]
  cur_th22<-relev_th_comb_4[th_comb_numb,4]
  
  # Add some information to the output data 
  # Add the total number of th, the minima and maxima 
  args<-c(args, num_th,cur_th11, cur_th12, cur_th21, cur_th22, min_th_11, min_th_12, min_th_21, min_th_22, max_th_11, max_th_12, max_th_21, max_th_22)
  args<-as.data.frame(t(args))
  colnames(args)<-c('days', 'N', 'day_h', 'th_comb_input', 'mod_type', 'output_dir', 'rep_num', 'total_num_per_th', 'th11', 'th12', 'th21', 'th22', 'min_th_11', 'min_th_12', 'min_th_21', 'min_th_22', 'max_th_11', 'max_th_12', 'max_th_21', 'max_th_22' )
  
  
  # No wthe environmnets need to run in parallel 
  
  # put into the environment fnction 
  env_results<-env_func_4_2_par_hpc(days = days, N= N, th_forage_fr1 = cur_th11, th_forage_fr2= cur_th12, th_forage_flr1=cur_th21, th_forage_flr2=cur_th22, daylight_h = daylight_h, modelType=modelType)
  
  # This contains the results for the current threshold, through all 18 environments 
  env_results
  
  #add this to the output list
  env_results[[3]]<-args
  
  # to not clutter the entire HPC
  env_results[[2]]<-NA
  
  # save the data 
  setwd(out_dir)
  # make sure to attach the threshold to the dataframe 
  save(env_results, file=paste0('outcome_4_2_HPC_th', th_comb_numb, '_', format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_', rep_num,'.Rda'))
  
}else if (modelType=='431'){
  
  # Set the number of options for which each threshold needs to be tested 
  num_th<-50
  # set the minima 
  # For 4.1 the first energy proxy is FR
  # For 4.1 the second energy proxy is FLR 
  min_th_11<-0
  min_th_12<-0
  min_th_13<-0
  min_th_21<-(-0.6)
  min_th_22<-(-0.6)
  min_th_23<-(-0.6)
  # set the maxima
  max_th_11<-4
  max_th_12<-4
  max_th_13<-4
  max_th_21<-0.6
  max_th_22<-0.6
  max_th_23<-0.6
  # create the vectors
  th11_vec<-linspace(x1=min_th_11, x2=max_th_11, n=num_th)
  th12_vec<-linspace(x1=min_th_12, x2=max_th_12, n=num_th)
  th13_vec<-linspace(x1=min_th_13, x2=max_th_13, n=num_th)
  th21_vec<-linspace(x1=min_th_21, x2=max_th_21, n=num_th)
  th22_vec<-linspace(x1=min_th_22, x2=max_th_22, n=num_th)
  th23_vec<-linspace(x1=min_th_23, x2=max_th_23, n=num_th)
  # create a matrix that contains all possible combinations 
  th_comb_31<-as.data.frame(as.matrix(expand.grid(th11_vec, th12_vec, th13_vec)))
  th_comb_32<-as.data.frame(as.matrix(expand.grid(th21_vec, th22_vec, th23_vec)))
  colnames(th_comb_31)<-c('th11', 'th12', 'th13')
  colnames(th_comb_32)<-c('th21', 'th22', 'th23')
  # select only relevant 
  relev_31<-th_comb_31[(th_comb_31$th11<th_comb_31$th12 & th_comb_31$th12<th_comb_31$th13),]
  relev_32<-th_comb_32[(th_comb_32$th21<th_comb_32$th22 & th_comb_32$th22<th_comb_32$th23),]
  
  # Create a new dataframe with all possible combinations of thes two dataframes 
  th_comb_6 <- expand.grid(1:nrow(relev_31), 1:nrow(relev_32)) %>%
    mutate(
      th11 = relev_31$th11[Var1],
      th12 = relev_31$th12[Var1],
      th13 = relev_31$th13[Var1],
      th21 = relev_32$th21[Var2],
      th22 = relev_32$th22[Var2],
      th23 = relev_32$th23[Var2]
    ) %>%
    select(th11, th12, th13, th21, th22, th23)
  
  
  # Set the current thresholds 
  cur_th11<-th_comb_6[th_comb_numb,1]
  cur_th12<-th_comb_6[th_comb_numb,2]
  cur_th13<-th_comb_6
  cur_th21<-th_comb_6[th_comb_numb,3]
  cur_th22<-th_comb_6[th_comb_numb,4]
  
  # Add some information to the output data 
  # Add the total number of th, the minima and maxima 
  args<-c(args, num_th,cur_th11, cur_th12, cur_th21, cur_th22, min_th_11, min_th_12, min_th_21, min_th_22, max_th_11, max_th_12, max_th_21, max_th_22)
  args<-as.data.frame(t(args))
  colnames(args)<-c('days', 'N', 'day_h', 'th_comb_input', 'mod_type', 'output_dir', 'rep_num', 'total_num_per_th', 'th11', 'th12', 'th21', 'th22', 'min_th_11', 'min_th_12', 'min_th_21', 'min_th_22', 'max_th_11', 'max_th_12', 'max_th_21', 'max_th_22' )
  
  
  # No wthe environmnets need to run in parallel 
  
  # put into the environment fnction 
  env_results<-env_func_4_1_par_hpc(days = days, N= N, th_forage_fr1 = cur_th11, th_forage_fr2= cur_th12, th_forage_flr1=cur_th21, th_forage_flr2=cur_th22, daylight_h = daylight_h, modelType=modelType)
  
  # This contains the results for the current threshold, through all 18 environments 
  env_results
  
  #add this to the output list
  env_results[[3]]<-args
  
  # save the data 
  setwd(out_dir)
  # make sure to attach the threshold to the dataframe 
  save(env_results, file=paste0('outcome_4_1_HPC_th', th_comb_numb, '_', format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_', rep_num,'.Rda'))
  
  
  
} else if(modelType=='432'){
  
  
}else {
  print('help stop, something is wrong with the modeltype ')
  
}

# End memory profiling 
# Rprof(NULL)
# summaryRprof(tf, memory='both')
