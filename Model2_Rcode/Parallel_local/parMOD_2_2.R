#################################
# Small bird in winter - ABM 
# Start date: 15/05/2023
# Vera Vinken 
# Model 2_2 parallelized locally 
#################################

##############################
#      load packages         #
##############################
library(usethis)
library(devtools)
library(truncnorm)
library(pracma)
library(ggplot2)
library(plotly) # for 3D surface plot 
library(rgl)
library(plot3D)
library(htmlwidgets)
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
library(purrr)
library(beepr)
library(tidyr)

# link to the function file 
setwd("C:/Local_R/BiPhD-ABM/Model1_Rcode/")
source('MOD_1_FuncSource.R')

# set the number of cores 
numCores<-detectCores()
registerDoParallel(numCores)

###############################
#    USE WHEN RUNNING LOCAL   # 
###############################

# Input variables 
# Number of days in the simulation 
days <- 30
# Number of agents in the simulation 
N <- 1000
# Type of environment (there are 18)
env_type <- 8
# Threshold stomach-content below which you forage (not relevant in model 2.2)
#th_forage_sc <- 0.2
# Threshold fat-reserve below which you forage  (not relevant in model 2.2)
#th_forage_fr <-2.0
# Threshold stomach-content below which you forage 
th_forage_fr1 <- 1.0  # Threshold 1 = below this you will retrieve 
th_forage_fr2<- 3.0   # Threshold 2 = above this you will rest 
# Number of hours of daylight 
daylight_h <- 8

######################################################################
##    Model 2.2: leftover-hoarding bird, Access to Fat-reserves     ##
######################################################################

# Start the model 
system.time({
  
  
  # Set up the general environment 
  # This part is the same for each bird 
  set_up_func_general(days, env_type, daylight_h)
  
  ################################
  #      individual loops        # 
  ################################
  
  # The individual loops need to start now
  # These should be parallelised 
  
  outcome_2_2<- foreach(icount(N), .packages = "truncnorm", .combine='rbind') %dopar% {
    
    # Do a setup for the individual bird
    # This includes the individual temperature pattern 
    # And individual matrices 
    set_up_func_indiv(days, env_type, daylight_h)
    
    # As we are running this in parallele, there is no 'i' for the number of indivuals 
    # So we can use the same functions, but just need to make sure i is always set to 1 
    i<-1
    
    ###################################
    #   start the for loop  timesteps # 
    ###################################
    
    # Start a for loop for each timestep 
    for (t in 1:TS){
      
      # Set the current temperature 
      temp_cur<-total_temp_profile[t]
      # Check if it is night or day 
      if ((t%%72)<= n_daylight_timestep){
        dayOrNight<-1                       # this means it is day 
      } else {
        dayOrNight<-0                       # this means it is night 
      }
      
      ###########################
      #     DEAD OR ALIVE?      #
      ###########################
      # Check which birds are dead or alive 
      # set some variables for dead birds
      # set some variables for alive birds 
      dead_or_alive_func(t,i)
      # The function above sets matrices of dead birds to 'NA' 
      # The rest of the code only needs to happen for the alive birds 
      
      if(mat_alive[i,t]==1){
        
        ####################
        #     SLEEPING     #
        ####################
        
        sleep_func(t,i)
        
        if (sleep_count[i,t]==0){
          
          # RULE SPECIFIC FOR MODEL 2_2 
          
          # Only access to stomach-content, birds need to retrieve if the stomach content is below the lowest threshold 
          # There also needs to be a minimum number of caches available 
          if ((mat_fr[i,t]<=th_forage_fr1) && (mat_caches[i,t]>retrieve_min)){
            
            ####################
            ##   RETRIEVING   ## 
            #################### 
            
            # The bird will retrieve food items 
            retrieve_func(t,i)
            # End of retrieving statement 
            
            # RULE SPECIFIC FOR MODEL 1_2 
            
          } else if ((mat_fr[i,t]) >= th_forage_fr2){
            
            ##################
            #    RESTING     # 
            ##################
            
            # If the bird has a SC above the second threshold, it is not hungry at all and can go rest        
            rest_func(t,i)
            
            # RULE SPECIFIC FOR MODEL 1_2 
            
          } else {
            # If the SC is not lower than Th1 and not higher then TH2, the bird needs to find a new food item 
            
            #################
            #     FORAGE    # 
            #################
            
            # At this point, foraging means to go out and find a NEW food item 
            forage_function(t,i)
            
            # RULE SPECIFIC TO MODEL 1_2
            
            # In model 1.2 the bird will eat and then hoard the leftovers 
            # The eat-haord function will count the behaviour as 'eat' if there is no leftover food. 
            # 'eat-hoard' matrix is used if there is leftover food and the bird hoards this (successful or not)
            eat_hoard_func(t,i)
            
          } # ends the foraging statement 
          
        } # End of the statement for awake birds 
        
        
        ###################
        #    EVERYONE     # 
        ###################
        # All alive birds, no matter if asleep or awake need to update these variables 
        
        ####################
        #    PREDATION     #
        ####################
        
        # Check if the bird is predated upon & caught 
        predation_func(t,i)
        
        ##########################
        #   ENERGY METABOLISM   # 
        #########################
        # Move food from stomach to fat, use some fat for metabolism and make sure nothing is below 0 
        en_metab_func(t,i)
        
        ##################################
        #   PREPARE FOR NEXT TIMESTEP   # 
        ##################################
        
        ts_prep_func(t,i, TS)
        
      } # end of loop for alive individuals 
      
      
      
      
    } # end timestep loop
    
    
    # Alternatively, I could try to create lists with the output 
    list(eat_count, eat_hoard_count, forage_count, hoard_count, mat_alive, mat_caches, mat_find_food, mat_fr, mat_sc, mat_mass, mat_Pkill, predation_count)
    
  } # end of the foreach loop (individuals) 
  
  # clean up cluster 
  stopImplicitCluster()
  
}) # ending system.time 


#################################
#  CONCATENATE THE DATAFRAMES   # 
#################################

# For each of teh 12 variables that we want the matrices off 
system.time({
  for (k in 1:12){
    if (k==1){
      # create a clean list in the first round 
      list_outcome_vars<-list()
    }
    # Create a dataframe from the first column of the total matrix 
    cur_df<-as.data.frame(do.call(rbind, outcome_2_2[1:N, k]))
    # add this to the empty list created 
    list_outcome_vars<-append(list_outcome_vars, list(cur_df))
  }
  
  # Now name them correctly 
  df_eat<-list_outcome_vars[[1]]
  df_eat_hoard<-list_outcome_vars[[2]]
  df_forage<-list_outcome_vars[[3]]
  df_dir_hoard<-list_outcome_vars[[4]]
  df_alive<-list_outcome_vars[[5]]
  df_caches<-list_outcome_vars[[6]]
  df_find_food<-list_outcome_vars[[7]]
  df_fr<-list_outcome_vars[[8]]
  df_sc<-list_outcome_vars[[9]]
  df_mass<-list_outcome_vars[[10]]
  df_Pkill<-list_outcome_vars[[11]]
  df_predation<-list_outcome_vars[[12]]
  
}) # end of system time 


