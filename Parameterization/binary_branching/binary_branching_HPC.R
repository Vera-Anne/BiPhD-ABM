######################################################################################
# 28/11/2023
# Rscript that runs the binary branching for models x.1 
# Vera Vinken 
# Last changes: 28/11/2023
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
#("C:/Local_R/BiPhD-ABM/May23")
# checking memory usage 
#Rprof(tf<-"rprof.log", memory.profiling=TRUE)

###############################
#    USE WHEN RUNNING LOCAL   # 
###############################
#args<-c(30, 100, 8, 5, 11, NA, 0)


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

# Number of hours of daylight 
daylight_h<- as.numeric(args[5])
# model type 
modelType<-as.numeric(args[6])
# OUTPUT DIR
out_dir<-args[7]
# repeat number
#rep_num<-args[7]

###################################
#   RUN THE branching FUNCTION     # 
###################################

branching_output<-branch_4_optimization_x1(model_type = modelType, range_min_start = range_min_start, range_max_start = range_max_start)

# add some details to save 
branching_output_list<-list(branching_output, args)
# set directory 
setwd(out_dir)
# make sure to attach the threshold to the dataframe 
save(branching_output_list, file=paste0('out_4branch_x1_', modelType, '_', format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_', rep_num,'.Rda'))

