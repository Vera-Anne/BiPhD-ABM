#################################
# Small bird in winter - ABM 
# Start date: 15/05/2023
# Vera Vinken 
# Model 1_3 parallelized locally 
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
# Threshold stomach-content below which you forage (not relevant in model 1.3)
#th_forage_sc <- 0.2
# Threshold fat-reserve below which you forage  (not relevant in model 1.3)
#th_forage_fr <-2.0
# Threshold stomach-content below which you forage 
th_forage_sc1 <- 0.1  # Threshold 1 = below this you will retrieve 
th_forage_sc2<- 0.2   # Threshold 2 = below this you forage - eat 
th_forage_sc3<-0.3    # Threshold 3 = below this you rest and above this you forage - hoard 
# Number of hours of daylight 
daylight_h <- 8

#################################################################
##   Model 1.1: Non-hoarding bird, Access to Stomach Content   ##
#################################################################

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
  
  outcome_1_3<- foreach(icount(N), .packages = "truncnorm", .combine='rbind') %dopar% {
    
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
          
          # RULE SPECIFIC FOR MODEL 1_3 
          
          if ((mat_sc[i,t])> th_forage_sc3){
            # This is above the third threshold, so the bird will do its direct hoarding, without eating at all 
            
            ############################
            #  FORAGE + HOARD DIRECT   #
            ############################
            
            #print('a bird forages and hoards')
            
            # At this point, foraging means to go out and find a NEW food item 
            forage_function(t,i)
            
            # Then directly just hoard it
            dir_hoard_func(t,i)
            
            
          } else if (((mat_sc[i,t])> th_forage_sc2) && ((mat_sc[i,t])<= th_forage_sc3)){
            # The bird will be resting 
            
            ##################
            #    RESTING     # 
            ##################
            #print('a bird rests')
            # If the bird has a SC above the second threshold, it is not hungry at all and can go rest        
            rest_func(t,i)
            
          } else if (((mat_sc[i,t])<=th_forage_sc1) && ((mat_caches[i,t])>retrieve_min)){
            
            #print('a bird retrieves')
            
            ####################
            ##   RETRIEVING   ## 
            #################### 
            
            # The bird will retrieve food items 
            retrieve_func(t,i)
            # End of retrieving statement 
            
            # RULE SPECIFIC FOR MODEL 1_3 
            
          }else {
            
            # The the stomach content is larger than TH1 but smaller or equal to TH2, the bird needs to go out to forage and eat 
            # Note that the birds that tried to retrieve (low stomach content) but did not have caches are in here 
            
            ######################
            #     FORAGE  + EAT  # 
            ######################
            if (mat_sc[i,t]<=th_forage_sc1){
              #print('bird tried to retrieve but went to forage and eat ')
            }
            
            #print('a bird forage + eats ')
            
            # At this point, foraging means to go out and find a NEW food item 
            forage_function(t,i)
            
            # The bird can then eat the food item 
            # This means that we have a non-hoarding bird that forages under the sc-threshold 
            eat_func(t,i)
            
          }
          
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
    cur_df<-as.data.frame(do.call(rbind, outcome_1_3[1:N, k]))
    # add this to the empty list created 
    list_outcome_vars<-append(list_outcome_vars, list(cur_df))
  }
  
  # Now name them correctly 
  df_eat<-list_outcome_vars[[1]]
  df_eat_hoard<-list_outcome_vars[[2]]
  df_forage<-list_outcome_vars[[3]]
  df_hoard<-list_outcome_vars[[4]]
  df_alive<-list_outcome_vars[[5]]
  df_caches<-list_outcome_vars[[6]]
  df_find_food<-list_outcome_vars[[7]]
  df_fr<-list_outcome_vars[[8]]
  df_sc<-list_outcome_vars[[9]]
  df_mass<-list_outcome_vars[[10]]
  df_Pkill<-list_outcome_vars[[11]]
  df_predation<-list_outcome_vars[[12]]
  
}) # end of system time 


