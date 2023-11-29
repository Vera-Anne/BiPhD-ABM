######################################################################################
# 29/11/2023
# Rscript that runs the binary branching for models x.2
# Vera Vinken 
# Last changes: 29/11/2023
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

# Input variables 
# Number of days in the simulation 
days<-as.numeric(args[1])

# Number of agents in the simulation
N<-as.numeric(args[2])
# set the ranges 
range_min_start_th1 <- as.numeric(args[3])
range_max_start_th1 <- as.numeric(args[4])

range_min_start_th2 <- as.numeric(args[5])
range_max_start_th2 <- as.numeric(args[6])

range_min_start_th3 <- as.numeric(args[7])
range_max_start_th3 <- as.numeric(args[8])

# Number of hours of daylight 
daylight_h<- as.numeric(args[9])
# model type 
modelType<-as.numeric(args[10])
# OUTPUT DIR
out_dir<-args[11]


###################################
#   RUN THE branching FUNCTION     # 
###################################

branching_output<-branch_4_optimization_x3_hpc(model_type = modelType, range_min_start_th1 = range_min_start_th1, range_max_start_th1 = range_max_start_th1, range_min_start_th2 = range_min_start_th2, range_max_start_th2 = range_max_start_th2, range_min_start_th3 = range_min_start_th3, range_max_start_th3 = range_max_start_th3)

# add some details to save 
branching_output_list<-list(branching_output, args)
# set directory 
setwd(out_dir)
# make sure to attach the threshold to the dataframe 
save(branching_output_list, file=paste0('out_4branch_x2_', modelType, '_', format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_', rep_num,'.Rda'))

