######################################################################################
# 22/08/2023
# Rscript that can run job arrays where each seperate job runs for 1 threshold combo 
# Vera Vinken 
# Last changes: 07/02/2024

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
print(paste("this is R - thresholds=", thresholds, "And the repetition number is =", rep_num))
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
      
      
  # This contains the results for the current threshold, through all 18 environments 
  env_results
  # clean up cluster 
  stopImplicitCluster()
  #add this to the output list
  env_results[[3]]<-args
  # save the data 
  setwd(out_dir)
  # make sure to attach the threshold to the dataframe 
  save(env_results, file=paste0('out_', modelType, '_HPC_', format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),"_lev", level, '_THrow', th_row,"_rep", rep_num,'.Rda'))
  
  
} else if (modelType==41|modelType==51|modelType==61){
  # print for output file 
  print(paste("this is R, we have modeltype=", modelType))
  # This is a 2 variale - 1 theshold model - subset 2 - non-hoarder
  # Set the thresholds 
  v1_th1<-thresholds[1]
  v2_th1<-thresholds[2]
  
  # Add some information to the output data 
  args<-as.data.frame(t(args))
  colnames(args)<-c('days', 'N', 'day_h', 'mod_type', "level", 'output_dir', "th_row", 'rep_num',"v1_th1", "v2_th1")
  
  # Run the model-specific part 
  
          if(modelType==41){
            # run environment function 
            env_results<-env_func_4_1_par_hpc(days = days, N= N, th_forage_fr = v1_th1, th_forage_flr=v2_th1, daylight_h = daylight_h, modelType=modelType)
            
          }else if(modelType==51){
            # run environment function 
            env_results<-env_func_5_1_par_hpc(days = days, N= N, th_forage_sc = v1_th1, th_forage_fr=v2_th1, daylight_h = daylight_h, modelType=modelType)
            
          }else if(modelType==61){
            # run environment function 
            env_results<-env_func_6_1_par_hpc(days = days, N= N, th_forage_sc = v1_th1, th_forage_flr=v2_th1, daylight_h = daylight_h, modelType=modelType)
            
          }else{
            print("Problem: modeltype is not set correctly, but within x.1 subset 2")
          }
          
      
      
  # Return it, so it will be included in the endresult of the optimization list 
  env_results
  #add this to the output list
  env_results[[3]]<-args
  # save the data 
  setwd(out_dir)
  save(env_results, file=paste0('out_', modelType, '_HPC_', format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),"_lev", level, '_THrow', th_row,"_rep", rep_num,'.Rda'))
  
  
}else if(modelType==42|modelType==52|modelType==62){
  # print for output file 
  print(paste("this is R, we have modeltype=", modelType))
  
  # This is a 2 variale - 2 theshold model - subset 2 - leftover-hoarder
  # Set the thresholds 
  v1_th1<-thresholds[1]
  v2_th2<-thresholds[2]
  v1_th2<-thresholds[3]
  v2_th2<-thresholds[4]
  
  # Add some information to the output data 
  args<-as.data.frame(t(args))
  colnames(args)<-c('days', 'N', 'day_h', 'mod_type', "level", 'output_dir', "th_row", 'rep_num',"v1_th1", "v2_th1","v1_th2", "v2_th2")

  # Run through the specific models 
        if(modelType==42){
          # run environment function 
          env_results<-env_func_4_2_par_hpc(days = days, N= N, th_forage_fr1 = v1_th1, th_forage_fr2= v1_th2, 
                                            th_forage_flr1=v2_th1, th_forage_flr2=v2_th2, daylight_h = daylight_h, modelType=modelType)
          
        }else if(modelType==52){
          env_results<-env_func_5_2_par_hpc(days = days, N= N, th_forage_sc1 = v1_th1, th_forage_sc2= v1_th2, 
                                            th_forage_fr1=v2_th1, th_forage_fr2=v2_th2, daylight_h = daylight_h, modelType=modelType)
    
        }else if(modelType==62){
          env_results<-env_func_6_2_par_hpc(days = days, N= N, th_forage_sc1 = v1_th1, th_forage_sc2= v1_th2, 
                                            th_forage_flr1=v2_th1, th_forage_flr2=v2_th2, daylight_h = daylight_h, modelType=modelType)
     
        }else{
          print("Problem: modeltype is not set correctly, but within x.2 subset 2")
        }
        
  
  # Return it, so it will be included in the endresult of the optimization list 
  env_results
  #add this to the output list
  env_results[[3]]<-args
  # save the data 
  setwd(out_dir)
  save(env_results, file=paste0('out_', modelType, '_HPC_', format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),"_lev", level, '_THrow', th_row,"_rep", rep_num,'.Rda'))
  
  
}else if (modelType==431|modelType==531|modelType==631|modelType==432|modelType==532|modelType==632){
  # print for output file 
  print(paste("this is R, we have modeltype=", modelType))
  
  # This is a 2 variale - 23theshold model - subset 2 - leftover-hoarder
  # Set the thresholds 
  v1_th1<-thresholds[1]
  v2_th1<-thresholds[2]
  v1_th2<-thresholds[3]
  v2_th2<-thresholds[4]
  v1_th3<-thresholds[5]
  v2_th3<-thresholds[6]
  
  # Add some information to the output data 
  args<-as.data.frame(t(args))
  colnames(args)<-c('days', 'N', 'day_h', 'mod_type', "level", 'output_dir', "th_row", 'rep_num',"v1_th1", "v2_th1","v1_th2", "v2_th2","v1_th3", "v2_th3")
  
  # Run through the specific models 
        if(modelType==431){
          # run environment function 
          env_results<-env_func_4_3_1_par_hpc(days = days, N= N, th_forage_fr1 = v1_th1, th_forage_fr2= v1_th2, th_forage_fr3= v1_th3,
                                              th_forage_flr1=v2_th1, th_forage_flr2=v2_th2, th_forage_flr3=v2_th3, daylight_h = daylight_h, modelType=modelType)
        }else if(modelType==432){
          env_results<-env_func_4_3_2_par_hpc(days = days, N= N, th_forage_fr1 = v1_th1, th_forage_fr2= v1_th2, th_forage_fr3= v1_th3,
                                              th_forage_flr1=v2_th1, th_forage_flr2=v2_th2, th_forage_flr3=v2_th3, daylight_h = daylight_h, modelType=modelType)
          
        }else if(modelType==531){
          env_results<-env_func_5_3_1_par_hpc(days = days, N= N, th_forage_sc1 = v1_th1, th_forage_sc2= v1_th2, th_forage_sc3= v1_th3,
                                              th_forage_fr1=v2_th1, th_forage_fr2=v2_th2, th_forage_fr3=v2_th3, daylight_h = daylight_h, modelType=modelType)
          
        }else if(modelType==532){
          env_results<-env_func_5_3_2_par_hpc(days = days, N= N, th_forage_sc1 = v1_th1, th_forage_sc2= v1_th2, th_forage_sc3= v1_th3,
                                              th_forage_fr1=v2_th1, th_forage_fr2=v2_th2, th_forage_fr3=v2_th3, daylight_h = daylight_h, modelType=modelType)
          
        }else if(modelType==631){
          env_results<-env_func_6_3_1_par_hpc(days = days, N= N, th_forage_sc1 = v1_th1, th_forage_sc2= v1_th2, th_forage_sc3= v1_th3,
                                              th_forage_flr1=v2_th1, th_forage_flr2=v2_th2, th_forage_flr3=v2_th3, daylight_h = daylight_h, modelType=modelType)
        }else if(modelType==632){
          env_results<-env_func_6_3_2_par_hpc(days = days, N= N, th_forage_sc1 = v1_th1, th_forage_sc2= v1_th2, th_forage_sc3= v1_th3,
                                              th_forage_flr1=v2_th1, th_forage_flr2=v2_th2, th_forage_flr3=v2_th3, daylight_h = daylight_h, modelType=modelType)
        }else{
          print("Problem: modeltype is not set correctly, but within x.3 subset 2")
        }
        
  
  # Return it, so it will be included in the endresult of the optimization list 
  env_results
  #add this to the output list
  env_results[[3]]<-args
  # save the data 
  setwd(out_dir)
  save(env_results, file=paste0('out_', modelType, '_HPC_', format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),"_lev", level, '_THrow', th_row,"_rep", rep_num,'.Rda'))

  
}else {
  print('help stop, something is wrong with the modeltype ')
  
}


