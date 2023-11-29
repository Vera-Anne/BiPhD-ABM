# Run the 1.1 model 25x 


# clear workspace
rm(list=ls())
# load everything 
setwd("C:/Local_R/BiPhD-ABM/May23") # for hp elitebook 
source('MOD_1_FuncSource.R')
source('ModelSource.R')

# set the value 
current_th<-0.021875


# Start the loop for running 1 value 25x 

for (i in 1:25){
  if (i==1){
    outcome_list<-list()
  }
  # environment model 
  env_func_1_1_par(days = 30, N= 1000, th_forage_sc = current_th, daylight_h = 8, modelType = 11)
  
  outcome_list[i]<-performance[[1]]  
  
  print(paste("done with ", i))
}

df<-as.data.frame(t(data.frame(t(sapply(outcome_list, c)))))

mean_performance<-mean(df$V1)  
mean_performance[2]<-current_th
mean_performance
beep()

###################################################################################

# model 2.1 

# clear workspace
rm(list=ls())
# load everything 
setwd("C:/Local_R/BiPhD-ABM/May23") # for hp elitebook 
source('MOD_1_FuncSource.R')
source('ModelSource.R')

# set the value 
current_th<-1.95918

# Start the loop for running 1 value 25x 

for (i in 1:25){
  if (i==1){
    outcome_list<-list()
  }
  # environment model 
  env_func_2_1_par(days = 30, N= 100, th_forage_fr = current_th, daylight_h = 8, modelType = 11)
  
  outcome_list[i]<-performance[[1]]  
  
  print(paste("done with ", i))
}

df<-as.data.frame(t(data.frame(t(sapply(outcome_list, c)))))
mean_performance<-mean(df$V1)  
mean_performance[2]<-current_th
mean_performance
beep()

