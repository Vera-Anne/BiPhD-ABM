######################################################################################
# 23/12/2023
# Rscript that makes new thresholds from a range that is either the starting range, or given  
# Vera Vinken 
# Last changes: 23/12/2023
######################################################################################

# Aim: script takes the given range and determines the next 4 branches
# The next script can than function for the job-seeding in arrays 

# link to the function file 
source('MOD_1_FuncSource.R')


#############################
#  COMMAND LINE ARGUMENTS   #
#############################
args<-commandArgs(trailingOnly=TRUE)

# Input variables 
  # set the ranges 
  range_min_start <- as.numeric(args[1])
  range_max_start <- as.numeric(args[2])
  
  # set the level 
  level<-as.numeric(args[3])
  
  # model type 
  modelType<-as.numeric(args[4])
  
  # Output directory 
  out_dir<-args[5]


##########################
##    BRANCHING         ##
##########################

# Run the branch function 
vals<-branch_func(range_min = range_min_start, range_max = range_max_start, branch_num = 4)
  
  
#####################
#   SAVE RESULTS    # 
#####################
  
  
# set directory 
  setwd(out_dir)
  
 
 #vals<-vals[1:4]
 vals<-t(vals)
  
# make sure to attach the threshold to the dataframe 
  write.table(vals, sep=" ", file=paste0('out_4branch_th', modelType, '_level', level, "_", '.csv'), row.names = F, col.names = F)
  #write.csv(vals, file="test.csv")
