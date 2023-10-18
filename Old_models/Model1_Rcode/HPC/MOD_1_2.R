#######################################
# Small bird in winter - ABM 
# Start date: 02/05/2023
# Vera Vinken 
# Model 1_2 - leftover-hoarder- SC 
#######################################

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

# link to the function file 
source('MOD_1_FuncSource.R')

###############################
#    USE WHEN RUNNING LOCAL   # 
###############################
# args<-c(30, 10, 8, 0.1, 0.3, 2.0, 8, NA, NA)


#############################
#  COMMAND LINE ARGUMENTS   #
#############################
args<-commandArgs(trailingOnly=TRUE)

# Input variables 
# Number of days in the simulation 
days <- as.numeric(args[1])
# Number of agents in the simulation 
N <- as.numeric(args[2])
# Type of environment (there are 18)
env_type <- as.numeric(args[3])
# Threshold stomach-content below which you forage 
th_forage_sc1 <- as.numeric(args[4])   # Threshold 1 = below this you will retrieve 
th_forage_sc2<- as.numeric(args[5])    # Threshold 2 = above this you will rest 
# Threshold fat-reserve below which you forage  (not relevant in model 1.1)
th_forage_fr <-as.numeric(args[6])
# Number of hours of daylight 
daylight_h <- as.numeric(args[7])
# array number
ar_num<- as.numeric(args[8])
# OUTPUT DIR
out_dir<-args[9]

##############################################################################
##   Model 1.2 Leftover-hoarder with 2 trhesholds based on stomach content  ##
##############################################################################

# Start the model 

# Set up the environment 
set_up_env(days,N, env_type, daylight_h)

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
  
  ################################
  #      individual loops        # 
  ################################
  
  # now start a loop for every individual 
  for (i in (1:N)){
    
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
        
  # RULE SPECIFIC FOR MODEL 1_2 
        
        # Only access to stomach-content, birds need to retrieve if the stomach content is below the lowest threshold 
        # There also needs to be a minimum number of caches available 
        if ((mat_sc[i,t]<=th_forage_sc1) && (mat_caches[i,t]>retrieve_min)){
          
          ####################
          ##   RETRIEVING   ## 
          #################### 
          
          # The bird will retrieve food items 
          retrieve_func(t,i)
          # End of retrieving statement 
          
  # RULE SPECIFIC FOR MODEL 1_2 
          
        } else if ((mat_sc[i,t]) >= th_forage_sc2){
          
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
    
  } # end of loop for each individual 
  
} # end timestep loop



##################################
#     GENERATE HPC OUTPUT        #
##################################

# All timesteps have passed
# Now  it is time to save the matrices as csvs 


# Code if you are not running in a loop 
#setwd('C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Bin/')

# current date and time 
#cur_date<-format(Sys.time(), "%Y-%m-%d_%H_%M_%S") 
# dir.create(paste0('R_mat_', cur_date))
# setwd(paste0('R_mat_', cur_date))

setwd(out_dir)

# behaviours 
write.table(forage_count, file=(paste0('mat_forage', ar_num,'.csv')), col.names = FALSE, sep=",")
write.table(rest_count, file=(paste0('mat_rest', ar_num,'.csv')), col.names = FALSE, sep=",")
write.table(retrieve_count, file=(paste0('mat_retrieved', ar_num,'.csv')), col.names = FALSE, sep=",")
write.table(eat_hoard_count, file=(paste0('mat_eat_hoard', ar_num,'.csv')), col.names = FALSE, sep=",")
write.table(eat_count, file=(paste0('mat_eat', ar_num,'.csv')), col.names = FALSE, sep=",")
write.table(predation_count, file=(paste0('mat_predated', ar_num,'.csv')), col.names = FALSE, sep=",")
write.table(sleep_count, file=(paste0('mat_sleep', ar_num,'.csv')), col.names = FALSE, sep=",")
write.table(hoard_count, file=(paste0('mat_dir_hoard', ar_num,'.csv')), col.names = FALSE, sep=",")

# # agent owned variables 
write.table(mat_fr, file=(paste0('mat_fat_reserve', ar_num,'.csv')), col.names = FALSE, sep=",")
write.table(mat_sc, file=(paste0('mat_stomach_content', ar_num,'.csv')), col.names = FALSE, sep=",")
write.table(mat_mass, file=(paste0('mat_mass', ar_num,'.csv')), col.names = FALSE, sep=",")
write.table(mat_caches, file=(paste0('mat_caches', ar_num,'.csv')), col.names = FALSE, sep=",")
write.table(mat_alive, file=(paste0('mat_alive', ar_num,'.csv')), col.names = FALSE, sep=",")
write.table(mat_Pkill, file=(paste0('mat_predRisk', ar_num,'.csv')), col.names = FALSE, sep=",")
write.table(mat_find_food, file=(paste0('mat_findFood', ar_num,'.csv')), col.names = FALSE, sep=",")



print(paste('The R script did run at', Sys.time()))







