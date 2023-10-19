######################################################################################
# 18/10/2023
# Rscript that can run job arrays where each seperate job runs for 1 threshold combo 
# Vera Vinken 
# Last changes: 18/10/2023
######################################################################################

##############################
#      load packages         #
##############################
library(GA)
library(purrr)
library(doParallel)
library(foreach)
library(iterators)

# link to the function file 
source('MOD_1_FuncSource.R')
source('ModelSource.R')


args<-commandArgs(trailingOnly=TRUE)

# Input variables 
# Number of days in the simulation 
out_dir<- as.numeric(args[1])

# Set the default variables 
days=30
num_birds=100
daylight_h=8

# set the running function for model 1.2
  f<-function(x){
    env_func_1_2_par_hpc(days = days, N= num_birds, th_forage_sc1 = x[1], th_forage_sc2 = x[2] , daylight_h = daylight_h, modelType = 12)
    return(output_env_func[[1]][1])
  }
# For constriction 1 I want x1 to be smaller than x2 
  c1<-function(x){
    x[1]-x[2]
  }

# Write the fitness function 
fitness <- function(x) 
  { 
    f <- f(x)                   # we need to maximise -f(x)
    pen <- sqrt(.Machine$double.xmax)  # penalty term
    penalty1 <- max(c1(x),0)*pen       # penalisation for 1st inequality constraint that needs X2 to be larger or equal to X1 
    if (c1(x)==0){pentalty2<-pen} else{penalty2<-0}
    
    f - penalty1 - penalty2     # fitness function value
  }

# now run the GA with the right bounds 
GA <- ga("real-valued", 
         fitness = fitness, 
         lower = c(0,0), 
         upper = c(0.4, 0.4), 
         maxiter = 50,
         popSize = 10)

output_ga<-summary(GA)

# save the data 
setwd(out_dir)
# make sure to attach the threshold to the dataframe 
save(output_ga, file=paste0('GA_out_', format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'.Rda'))
