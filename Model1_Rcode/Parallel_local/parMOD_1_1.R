#################################
# Small bird in winter - ABM 
# Start date: 05/05/2023
# Vera Vinken 
# Model 1_1 parallelized locally 
#################################

##############################
#      load packages         #
##############################
# library(usethis)
# library(devtools)
# library(truncnorm)
# library(pracma)
# library(ggplot2)
# library(plotly) # for 3D surface plot 
# library(rgl)
# library(plot3D)
# library(htmlwidgets)
# library(webshot)
# library(withr)
# library('plyr')
# library('gridExtra')
# library(grid)
# library(lattice)
# library(dplyr)
# library(data.table)
# library(tidyverse)
# library(viridis)
library(foreach)
library(doParallel)
# library(purrr)
# library(beepr)
# library(tidyr)

# link to the function file 
setwd("C:/Local_R/BiPhD-ABM/")
source('MOD_1_FuncSource.R')



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
    # Threshold stomach-content below which you forage
    th_forage_sc <- 0.2
    # Threshold fat-reserve below which you forage  (not relevant in model 1.1)
    #th_forage_fr <-2.0
    # Number of hours of daylight
    daylight_h <- 8

#################################################################
##   Model 1.1: Non-hoarding bird, Access to Stomach Content   ##
#################################################################

# Start the model 
  
  # set the number of cores 
  numCores<-detectCores()
  registerDoParallel(numCores)
  
# Set up the general environment 
# This part is the same for each bird 
set_up_func_general(days, env_type, daylight_h)

################################
#      individual loops        # 
################################

# The individual loops need to start now
# These should be parallelised 

outcome_1_1<-foreach(icount(N), .packages = "truncnorm", .combine='rbind') %dopar% {
  
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
        
        # RULE SPECIFIC FOR MODEL 1_1 
        
        # Only access to stomach-content & forage under threshold 
        if ((mat_sc[i,t]) < th_forage_sc){
          # If this is the case, the bird is hungry and needs to forage for food 
          
          #################
          #     FORAGE    # 
          #################
          
          # At this point, foraging means to go out and find a NEW food item 
          forage_function(t,i)
          
          # RULE SPECIFIC TO MODEL 1_1
          
          # This means that we have a non-hoarding bird that forages under the sc-threshold 
          # It can only eat or rest. Eat-hoarding and hoarding are not possible 
          eat_func(t,i)
          
        } else {
          
          ##################
          #    RESTING     # 
          ##################
          
          # If the bird is not foraging and eating, it will be resting (model 1.1)           
          rest_func(t,i)
          
        } # end resting statement 
        
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




############################# 
#   SAVE THE RESULTS        # 
#############################

# Make sure all the folders are there and set to the correct working directory 
direct_func(modelType = 'MOD_1_1')
# save it 
save(outcome_1_1, file=paste0('MOD_1_1_out_D',days, '_N', N, '_eType', env_type, '_th_sc', th_forage_sc, '_DL', daylight_h, '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S")))








