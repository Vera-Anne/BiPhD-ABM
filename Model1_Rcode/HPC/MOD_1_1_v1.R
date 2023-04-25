#################################
# Small bird in winter - ABM 
# Start date: 25/04/2023
# Vera Vinken 
# Model 1_1 
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

# link to the function file 
source('MOD_1_FuncSource.R')

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
  th_forage_sc <- as.numeric(args[4])
  # Threshold fat-reserve below which you forage  (not relevant in model 1.1)
  th_forage_fr <-as.numeric(args[5])
  # Number of hours of daylight 
  daylight_h <- as.numeric(args[6])

#############################
#    MODEL 1: 1 VARIABLE    # 
#############################

#################################################################
##   Model 1.1: Non-hoarding bird, Access to Stomach Content   ##
#################################################################

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
      
      # Check if individual is alive? 
      # in step 1 all birds are alive 
      if (t==1){
        mat_alive[i,t]<- 1
      } else if (mat_alive[i,(t-1)]==0){
        # if not step 1, check if bird was previously dead
        # if previously dead, it needs to be dead now 
        mat_alive[i,t]<-0
      } else if (mat_fr[i,t]==0){
        # if not step 1 and not previously dead 
        # check if the bird should die now 
        mat_alive[i,t]<-0
      } else {
        # in all other cases the bird is alive 
        mat_alive[i,t]<-1
      }
      
      ################
      #  DEAD BIRDS  #
      ################
      if(mat_alive[i,t]==0){
        # these are the dead birds 
        # Set the matrices to 'NA' for dead birds 
        # For the fr matrix 
        mat_fr[i,t]<-NA
        # For the mass matrix 
        mat_mass[i,t]<-NA
        # For the sc matrix 
        mat_sc[i,t]<-NA
        # for the caches matrix 
        mat_caches[i,t]<-NA
      } else {
        
        #################
        #  ALIVE BIRDS  #
        #################
        
        # Set the current BMR 
        # Note: I have made the decision to calculate this at the start of the tick. 
        # So this is before any behaviour, or food is moved around 
        # set the current mass 
        mass_cur<-mat_mass[i,t]
        # calculate the current mr 
        mr_function(temp_cur)                                   # note that this will need to be changed if we're using different temperatures
        # calculate the current 
        bmr_function(mr_cur, mass_cur)
        
        # Check if the bird should be sleeping 
        if(dayOrNight==0){
          
          ################
          #   SLEEPING   # 
          ################
          
          # code checking 
          #print('a bird sleeps')
          
          # set the sleeping matrix to 1 
          sleep_count[i,t]<-1
          # set the forage to 0
          forage_count[i, t]<-0
          # set the resting matrix to 0
          rest_count[i,t]<-0
          # set the retrieval matrix to 0 
          retrieve_count[i,t]<-0
          # set the eat-hoarding matrix to 0
          eat_hoard_count[i,t]<-0
          # set the eating matrix to 0 
          eat_count[i,t]<-0
          
          # set the BMR-multi
          BMR_multi<-1
          #set the predation risk 
          Patt_cur<-Patt_sleep
          
          # Food will be moved from the stomach
          # Into the fat reserves 
          # and be burned depending on BMR-multi
          # in the ' Everyone '  part of the code below
          # end of birds that are asleep   
        } else {
          
          # NON SLEEPING BIRDS START HERE : >>>>>>>>>
          # set the sleeping matrix to 0 
          sleep_count[i,t]<-0
          
          # Check what behavior the bird should do if it is day 
          # CHECK IF BIRD IS HUNGRY AND NEEDS FOOOD 
          
          # Time to forage: 
          
          # Only access to stomach-content 
          if ((mat_sc[i,t]) < th_forage_sc){
            # If this is the case, the bird is hungry and needs to forage for food 
            # Now, different things will happen for hoarding vs. non-hoarding birds 
            
            #################
            #     FORAGE    # 
            #################
            
            # 'Forage' is used as a general term for trying to find food
            # For hoarding birds this can be retrieving, eat-hoard and eating 
            # For non-hoarding birds this can be eating 
            
            # SET COUNTING MATRICES (for both NonH and H)
            # In this case the bird should forage
            # set the forage to 1
            forage_count[i, t]<-1
            # set the resting matrix to 0
            rest_count[i,t]<-0
            
            #set the predation risk 
            # Note: this is currently the same for all types of foraging
            Patt_cur<-Patt_for
            
            # The non-hoarding birds can only 'eat' food they find. 
            
            # Run the forage function 
            # The outcome here is 'food_item_found'
            forage_function(num_food_mean, prob_b_forage, b_size)
            
            # Pop this in the matrix (this is in number of items found)
            mat_find_food[i,t]<-food_item_found
            # convert to grams
            food_item_found_gram<-(food_item_found*food_item)
            
            # Now, increase the stomach content
            mat_sc[i,(t)]<-(mat_sc[i,t])+(food_item_found_gram)
            # now check if this doesn't exceed the stomach size
            # if so, set the stomach content to stomach size 
            if (mat_sc[i,(t)]>stom_size){
              mat_sc[i,(t)]<-stom_size
            }
            # Set the BMR to the right level: cost of foraging
            # BMR-multi is not a global variable: stays local with the agent
            BMR_multi<-8
            #set the predation risk 
            Patt_cur<-Patt_for
            
            # update the eating-matrix 
            # update the global counters 
            eat_hoard_count[i,t]<-0
            retrieve_count[i,t]<-0
            eat_count[i,t]<-1
            
            # Stomach content is already updated 
            # Update BMR multi
            #BMR_multi<<-8
            # ends the foraging for non hoarding birds statement 
            # ends the foraging statement   
          } else {
            ##################
            #    RESTING     # 
            ##################
            # testing code 
            #print(paste('bird', i, ' is resting'))
            # SET COUNTING MATRICES 
            # set the unused behaviour matrices to 0
            forage_count[i,t]<-0
            retrieve_count[i,t]<-0
            eat_hoard_count[i,t]<-0
            eat_count[i,t]<-0
            # set the rest matrix to 1
            rest_count[i,t]<-1
            
            # SET AGENT OWNED VARIABLES 
            BMR_multi<-1.95                    # resting BMR 
            # the stomach content stays the same (initial value)
            # or at least for now 
            
            #set the predation risk 
            Patt_cur<-Patt_rest
            
          } # end resting statement 
        } # end of 'Time of day = day ' statement 
        
        
        ###################
        #    EVERYONE     # 
        ###################
        # No matter what behaviour you've done, these need updating for all alive birds
        
        # PREDATION 
        # first check if the bird actually survived the behaviour it did 
        mass_cur<-mat_mass[i,t]                                            # find out the current mass of the bird 
        Pcap_cur<-(0.78+(0.5*(10^-8)*exp(1.4*mass_cur)))                  # calculate the current Pcapture 
        Pkill_cur<-Pcap_cur*Patt_cur                                       # calculate the current Pkill 
        mat_Pkill[i,t]<-Pkill_cur                                         # put in the matrix 
        # now check if the bird dies or not 
        Psurv_cur<-runif(1)                                                 # Random number between 0 and 1 for survival chance 
        if(Psurv_cur<(mat_Pkill[i,t])){                                            # if the prob for survival < prob to die 
          mat_alive[i,t]<-0                                                # Set the matrix to 'dead' 
          predation_count[i,t]<-1
          #print(paste0('a bird ', 'i=', i , ' got eaten at t=', t))
        } else {
          # Surviving birds should update their values: 
          predation_count[i,t]<-0
          
          # UPDATE THE FAT RESERVES AND STOMACH CONTENT
          # SC down and FR up 
          # first check if stomach has enough to actually move
          # move food out of stomach into fat 
          if (mat_sc[i,(t)]>= stom_to_fat){
            # new sc from resting/foraging can be used
            mat_sc[i,(t)]<-(mat_sc[i,(t)]-stom_to_fat)
            # the new fat reserve has not been determined yet
            mat_fr[i,(t)]<-(mat_fr[i,t]+stom_to_fat)
          } else {
            mat_fr[i,t]<-(mat_fr[i,t]+mat_sc[i,t])   # move whatever is left in the stomach to fat 
            mat_sc[i,t]<-0                           # set the stomach content to 0 
          }
          
          # ENERGY EXPENDITURE 
          # Set the fat reserves down depending on bmr-multi
          
          # first subtract the amount
          mat_fr[i,(t)]<-(mat_fr[i,t]-(bmr_cur*BMR_multi))
          # then make sure if this doesnt go below 0 
          if((mat_fr[i,(t)]<0)){
            mat_fr[i,(t)]<-0
          }
          # or above the maximum for fat-reserves 
          if((mat_fr[i,(t)]>fat_max)){
            mat_fr[i,(t)]<-fat_max
          }
          # check if the stomach content is above 0 
          if((mat_sc[i, (t)]<0)){
            mat_sc[i, (t)]<-0
          }
          # check if it is not above the stomach size either
          if((mat_sc[i,t]>stom_size)){
            mat_sc[i,t]<-stom_size
          }
          
          # SET MASS 
          # set the new mass for all individuals 
          mat_mass[i,t]<-((mass_init[i])+(mat_fr[i,t])+(mat_sc[i,t]))
          
          
          # MOVE ALL VARAIBLES TO T+1 
          # Note that this should only happen if youre not in the last timestep 
          if(t<TS){
            # For the fr matrix 
            mat_fr[,(t+1)]<-mat_fr[,t]
            # For the mass matrix 
            mat_mass[,(t+1)]<-mat_mass[,t]
            # For the sc matrix 
            mat_sc[,(t+1)]<-mat_sc[,t]
            # for the caches matrix 
            mat_caches[,(t+1)]<-mat_caches[,t]
          }
          
        } # end of statement for birds that survived predation 
        
      } # end of loop for alive individuals 
      
    } # end of loop for each individual 
    

  } # end timestep loop


  
  
##################################
#     GENERATE HPC OUTPUT        #
##################################
  
# All timesteps have passed
# Now  it is time to save the matrices as csvs 
  
  #setwd('C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Bin/')
  
  # current date and time 
  cur_date<-format(Sys.time(), "%Y-%m-%d_%H_%M_%S") 
  dir.create(paste0('R_mat_', cur_date))
  setwd(paste0('R_mat_', cur_date))
  
  # behaviours 
  write.csv((as.data.frame(forage_count)), file=(paste0('mat_forage_', cur_date, '.csv')))
  write.csv((as.data.frame(rest_count)), file=(paste0('mat_rest_', cur_date, '.csv')))
  write.csv((as.data.frame(retrieve_count)), file=(paste0('mat_retrieved_', cur_date, '.csv')))
  write.csv((as.data.frame(eat_hoard_count)), file=(paste0('mat_eat_hoard_', cur_date,'.csv')))
  write.csv((as.data.frame(eat_count)), file=(paste0('mat_eat_', cur_date, '.csv')))
  write.csv((as.data.frame(predation_count)), file=(paste0('mat_predated_', cur_date, '.csv')))
  write.csv((as.data.frame(sleep_count)), file=(paste0('mat_sleep_', cur_date,'.csv')))
  
  # agent owned variables 
  write.csv((as.data.frame(mat_fr)), file=(paste0('mat_fat_reserve_', cur_date, '.csv')))
  write.csv((as.data.frame(mat_sc)), file=(paste0('mat_stomach_content_', cur_date, '.csv')))
  write.csv((as.data.frame(mat_mass)), file=(paste0('mat_mass_',cur_date,'.csv')))
  write.csv((as.data.frame(mat_caches)), file=(paste0('mat_caches_', cur_date, '.csv')))
  write.csv((as.data.frame(mat_alive)), file=(paste0('mat_alive_', cur_date, '.csv')))
  write.csv((as.data.frame(mat_Pkill)), file=(paste0('mat_predRisk_', cur_date, '.csv')))
  write.csv((as.data.frame(mat_find_food)), file=(paste0('mat_findFood_', cur_date, '.csv')))
  
  
  
  print(paste('The R script did run at', Sys.time()))
  
  
  

  
  
  
  