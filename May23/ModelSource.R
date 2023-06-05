#################################
# Small bird in winter - ABM 
# Start date: 16/05/2023
# Vera Vinken 
# all models - for general use 
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
# library(foreach)
# library(doParallel)
# library(purrr)
# library(beepr)
# library(tidyr)

# link to the function file 
# This contains all the general, smaller funcitons needed for the models 
# setwd("C:/Local_R/BiPhD-ABM/")
# source('MOD_1_FuncSource.R')



###############################
#    USE WHEN RUNNING LOCAL   # 
###############################

# Input variables 
      # #Number of days in the simulation
      # days <- 5
      # # Number of agents in the simulation
      # N <- 10
      # # Type of environment (there are 18)
      # env_type <- 8
      # # Threshold stomach-content below which you forage
      # th_forage_sc <- 0.2
      # # Threshold fat-reserve below which you forage  (not relevant in model 1.1)
      # #th_forage_fr <-2.0
      # # Number of hours of daylight
      # daylight_h <- 8

#################################################################
##   Model 1.1: Non-hoarding bird, Access to Stomach Content   ##
#################################################################
      
      # model 1.1 
      mod_1_1<-function(days, N, env_type, th_forage_sc, daylight_h){
        require(foreach)
        require(doParallel)
        # Start the model 

        # Start the model 
        # link to the function file 
        setwd("C:/Local_R/BiPhD-ABM/May23")
        source('MOD_1_FuncSource.R')
        # set the number of cores 
        #numCores<-(detectCores()-1)
        #registerDoParallel(numCores)
        
        # Set up the general environment 
        # This part is the same for each bird 
        set_up_func_general(days, env_type, daylight_h)
        
        
        ################################
        #      individual loops        # 
        ################################
        
        # The individual loops need to start now
        # These should be parallelised 
        
        outcome_1_1<-foreach(icount(N), .packages = "truncnorm", .combine='rbind') %do% {
          
          # link to the function file 
          # setwd("C:/Local_R/BiPhD-ABM/")
          # source('MOD_1_FuncSource.R')
          
          # Do a setup for the individual bird
          # This includes the individual temperature pattern 
          # And individual matrices 
          #print('here')
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
            temp_cur<<-total_temp_profile[t]
            # Check if it is night or day 
            if ((t%%72)<= n_daylight_timestep){
              dayOrNight<<-1                       # this means it is day 
            } else {
              dayOrNight<<-0                       # this means it is night 
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
          list(eat_count, eat_hoard_count, forage_count, hoard_count, mat_alive, mat_caches, mat_find_food, mat_fr, mat_sc, mat_mass, predation_count, rest_count)
          
        } # end of the foreach loop (individuals) 
        
        # clean up cluster 
        stopImplicitCluster()
        
        #return(outcome_1_1)
        assign(paste0('outcome_1_1_env', env_type),outcome_1_1, envir=.GlobalEnv)
        
        create_df_func(outputFile = outcome_1_1, modelType = '11', env_type= env_type)

        #assign(paste0('output_means_list',modelType, 'env', env_type, sep=''), mean_dfs, envir=.GlobalEnv)
        
        
    

        
      } # end of model 1 function 

      # run it here 
      #mod_1_1(10, 100, 8, 0.2, 8)
      
      ########################
      #   ENVIRONMENT LOOP   #
      ########################
      
      # The environment loop 
      env_func_1_1<-function(days, N, th_forage_sc, daylight_h, modelType){
        
        # Loop through the environments 
        for (env in 1:18){
          if (env==1){
            list_means_envs<-list()
          }
          
          mod_1_1(days = days, N = N, env_type = env, th_forage_sc = th_forage_sc, daylight_h = daylight_h)
          
          
          list_means_envs[[env]]<-mean_dfs
          
        }
        
        # print(environment())
        #list_means_envs<<-mget(ls(pattern = "output_means_list11env"))
        
        # now select only the information about survival
        list_means_envs<-lapply(list_means_envs, function(x){x$alive})
        # now find the row with the closest value of survival to 0.5 (halflife)
        halflife_per_env<-lapply(list_means_envs, function(x){x$timestep[which.min(abs(0.5-x$value))]})
        # Same for the end survival
        end_survival_per_env<-lapply(list_means_envs, function(x){x$value[x$timestep==TS]})
        
        # now put relevant data in the global environment
        # assign(paste0('HL_pEnv_th_sc', th_forage_sc), halflife_per_env, envir=.GlobalEnv)
        # assign(paste0('ES_pEnv_th_sc', th_forage_sc), end_survival_per_env, envir=.GlobalEnv)
        # assign(paste0('list_means_per_env_thsc', th_forage_sc), list_means_envs, envir=.GlobalEnv)
        
        # generate the average average end-survival for this threshold, across all the environments 
        mean_ES_cur_th<-mean(unlist(end_survival_per_env))
        # and now for the average time till halflife 
        mean_HL_cur_th<-mean(unlist(halflife_per_env))
        # do the same for the 
        
        
        output_env_func<-cbind(mean_ES_cur_th, mean_HL_cur_th)
        
        assign(paste0('output_env_function_th', th_forage_sc), output_env_func, envir=.GlobalEnv)
        
        return(output_env_func)
        
        # Set the coefficient for plotting the optimisations 
        #assign(total_timesteps, (days*72), envir = .GlobalEnv)
        
      } # end environment function loop 
      
      # environment loop paralelel 
      env_func_1_1_par<-function(days, N, th_forage_sc, daylight_h, modelType){
        
        require(doParallel)
        require(foreach)
        
        numCores<-(detectCores()-1)
        registerDoParallel(numCores)
        
        num_env<-18 
        
        outcome_env_1_1_par<- foreach(i=1:num_env, .packages = c( "truncnorm", "purrr")) %dopar% {
          
          setwd("C:/Local_R/BiPhD-ABM/May23")
          source('MOD_1_FuncSource.R')
          source('ModelSource.R')
          
          mod_1_1(days = days, N = N, env_type = i, th_forage_sc = th_forage_sc, daylight_h = daylight_h)
          
          #print('done')
          
          
        }
        
        # clean up cluster 
        stopImplicitCluster()
        
        # print(environment())
        #list_means_envs<<-mget(ls(pattern = "output_means_list11env"))
        
        # now select only the information about survival
        list_means_envs<-lapply(outcome_env_1_1_par, function(x){subset(x, x$id=='alive')})
        
        # now find the row with the closest value of survival to 0.5 (halflife)
        halflife_per_env<-lapply(list_means_envs, function(x){x$timestep[which.min(abs(0.5-x$value))]})
        # Same for the end survival
        end_survival_per_env<-lapply(list_means_envs, function(x){x$value[x$timestep==(days*72)]})
        
        # now put relevant data in the global environment
        # assign(paste0('HL_pEnv_th_sc', th_forage_sc), halflife_per_env, envir=.GlobalEnv)
        # assign(paste0('ES_pEnv_th_sc', th_forage_sc), end_survival_per_env, envir=.GlobalEnv)
        # assign(paste0('list_means_per_env_thsc', th_forage_sc), list_means_envs, envir=.GlobalEnv)
        
        # generate the average average end-survival for this threshold, across all the environments 
        mean_ES_cur_th<-mean(unlist(end_survival_per_env))
        # and now for the average time till halflife 
        mean_HL_cur_th<-mean(unlist(halflife_per_env))
        # do the same for the 
        
        
        output_env_func<<-cbind(mean_ES_cur_th, mean_HL_cur_th)
        
        # assign(paste0('output_env_function_th1=', th_forage_sc1, ' th2=', th_forage_sc2), output_env_func, envir=.GlobalEnv)
        
        return(output_env_func)
        
      } # end environment function loop 
      
      
######################################################################
##   Model 1.2: Leftover-hoarding bird, Access to Stomach Content   ##
######################################################################
      
      # model 1.2 
      mod_1_2<-function(days, N, env_type, th_forage_sc1, th_forage_sc2, daylight_h){
        require(foreach)
        require(doParallel)
        # Start the model 
        # link to the function file 
        setwd("C:/Local_R/BiPhD-ABM/May23")
        source('MOD_1_FuncSource.R')
        # set the number of cores 
        #umCores<-(detectCores()-1)
        #registerDoParallel(numCores)
        
        # Set up the general environment 
        # This part is the same for each bird 
        set_up_func_general(days, env_type, daylight_h)
        
        ################################
        #      individual loops        # 
        ################################
        
        # The individual loops need to start now
        # These should be parallelised 
        outcome_1_2<- foreach(icount(N), .packages = "truncnorm", .combine='rbind') %do% {
          
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
            temp_cur<<-total_temp_profile[t]
            # Check if it is night or day 
            if ((t%%72)<= n_daylight_timestep){
              dayOrNight<<-1                       # this means it is day 
            } else {
              dayOrNight<<-0                       # this means it is night 
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
            
            
            
            
          } # end timestep loop
          
          
          # Alternatively, I could try to create lists with the output 
          list(eat_count, eat_hoard_count, forage_count, hoard_count, mat_alive, mat_caches, mat_find_food, mat_fr, mat_sc, mat_mass,  predation_count, rest_count)
          
        } # end of the foreach loop (individuals) 
        
        # clean up cluster 
        stopImplicitCluster()
        
        #return(outcome_1_1)
        assign(paste0('outcome_1_2_env', env_type),outcome_1_2, envir=.GlobalEnv)
        
        create_df_func(outputFile = outcome_1_2, modelType = '12', env_type= env_type)
        
        #assign(paste0('output_means_list',modelType, 'env', env_type, sep=''), mean_dfs, envir=.GlobalEnv)
        
      } # end of model 1.2 function 
      

      ########################
      #   ENVIRONMENT LOOP   #
      ########################
      
      # The environment loop 
      env_func_1_2<-function(days, N, th_forage_sc1, th_forage_sc2, daylight_h, modelType){
        
        # Loop through the environments 
        for (env in 1:18){
          if (env==1){
            list_means_envs<-list()
          }
          
          mod_1_2(days = days, N = N, env_type = env, th_forage_sc1 = th_forage_sc1, th_forage_sc2 = th_forage_sc2, daylight_h = daylight_h)
          
          
          list_means_envs[[env]]<-mean_dfs
          
        }
        
        # print(environment())
        #list_means_envs<<-mget(ls(pattern = "output_means_list11env"))
        
        # now select only the information about survival
        list_means_envs<-lapply(list_means_envs, function(x){x$alive})
        # now find the row with the closest value of survival to 0.5 (halflife)
        halflife_per_env<-lapply(list_means_envs, function(x){x$timestep[which.min(abs(0.5-x$value))]})
        # Same for the end survival
        end_survival_per_env<-lapply(list_means_envs, function(x){x$value[x$timestep==TS]})
        
        # now put relevant data in the global environment
        # assign(paste0('HL_pEnv_th_sc', th_forage_sc), halflife_per_env, envir=.GlobalEnv)
        # assign(paste0('ES_pEnv_th_sc', th_forage_sc), end_survival_per_env, envir=.GlobalEnv)
        # assign(paste0('list_means_per_env_thsc', th_forage_sc), list_means_envs, envir=.GlobalEnv)
        
        # generate the average average end-survival for this threshold, across all the environments 
        mean_ES_cur_th<-mean(unlist(end_survival_per_env))
        # and now for the average time till halflife 
        mean_HL_cur_th<-mean(unlist(halflife_per_env))
        # do the same for the 
        
        
        output_env_func<-cbind(mean_ES_cur_th, mean_HL_cur_th)
        
        assign(paste0('output_env_function_th1=', th_forage_sc1, ' th2=', th_forage_sc2), output_env_func, envir=.GlobalEnv)
        
        return(output_env_func)
        
      } # end environment function loop 
      
      # environment loop paralelel 
      env_func_1_2_par<-function(days, N, th_forage_sc1, th_forage_sc2, daylight_h, modelType){
        
        require(doParallel)
        require(foreach)
        
        numCores<-(detectCores()-1)
        registerDoParallel(numCores)
        
        num_env<-18 
        
        outcome_env_1_2_par<- foreach(i=1:num_env, .packages = c( "truncnorm", "purrr")) %dopar% {
          
          setwd("C:/Local_R/BiPhD-ABM/May23")
          source('MOD_1_FuncSource.R')
          source('ModelSource.R')
          
          mod_1_2(days = days, N = N, env_type = i, th_forage_sc1 = th_forage_sc1, th_forage_sc2 = th_forage_sc2, daylight_h = daylight_h)
          
          #print('done')
          
          
        }
        
        # clean up cluster 
        stopImplicitCluster()
        
        # print(environment())
        #list_means_envs<<-mget(ls(pattern = "output_means_list11env"))
        
        # now select only the information about survival
        list_means_envs<-lapply(outcome_env_1_2_par, function(x){subset(x, x$id=='alive')})
        
        # now find the row with the closest value of survival to 0.5 (halflife)
        halflife_per_env<-lapply(list_means_envs, function(x){x$timestep[which.min(abs(0.5-x$value))]})
        # Same for the end survival
        end_survival_per_env<-lapply(list_means_envs, function(x){x$value[x$timestep==(days*72)]})
        
        # now put relevant data in the global environment
        # assign(paste0('HL_pEnv_th_sc', th_forage_sc), halflife_per_env, envir=.GlobalEnv)
        # assign(paste0('ES_pEnv_th_sc', th_forage_sc), end_survival_per_env, envir=.GlobalEnv)
        # assign(paste0('list_means_per_env_thsc', th_forage_sc), list_means_envs, envir=.GlobalEnv)
        
        # generate the average average end-survival for this threshold, across all the environments 
        mean_ES_cur_th<-mean(unlist(end_survival_per_env))
        # and now for the average time till halflife 
        mean_HL_cur_th<-mean(unlist(halflife_per_env))
        # do the same for the 
        
        
        output_env_func<<-cbind(mean_ES_cur_th, mean_HL_cur_th)
        
        # assign(paste0('output_env_function_th1=', th_forage_sc1, ' th2=', th_forage_sc2), output_env_func, envir=.GlobalEnv)
        
        return(output_env_func)
        
      } # end environment function loop 
      

######################################################################
##    Model 1.3: direct-hoarding bird, Access to Stomach Content    ##
######################################################################
      
      # model 1.3.1 
      mod_1_3_1<-function(days, N, env_type, th_forage_sc1, th_forage_sc2, th_forage_sc3, daylight_h){
        require(doParallel)
        require(foreach)
        # Start the model 
        # link to the function file 
        setwd("C:/Local_R/BiPhD-ABM/May23")
        source('MOD_1_FuncSource.R')
        # set the number of cores 
        #numCores<-(detectCores()-1)
        #registerDoParallel(numCores)
        
        # Set up the general environment 
        # This part is the same for each bird 
        set_up_func_general(days, env_type, daylight_h)
        
        ################################
        #      individual loops        # 
        ################################
        
        # The individual loops need to start now
        # These should be parallelised 
        
        outcome_1_3_1<- foreach(icount(N), .packages = "truncnorm", .combine='rbind') %do% {
          
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
            temp_cur<<-total_temp_profile[t]
            # Check if it is night or day 
            if ((t%%72)<= n_daylight_timestep){
              dayOrNight<<-1                       # this means it is day 
            } else {
              dayOrNight<<-0                       # this means it is night 
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
          list(eat_count, eat_hoard_count, forage_count, hoard_count, mat_alive, mat_caches, mat_find_food, mat_fr, mat_sc, mat_mass,  predation_count, rest_count)
          
        } # end of the foreach loop (individuals) 
        

        # clean up cluster 
        stopImplicitCluster()
        
        #return(outcome_1_1)
        assign(paste0('outcome_1_3_1_env', env_type),outcome_1_3_1, envir=.GlobalEnv)
        
        create_df_func(outputFile = outcome_1_3_1, modelType = '131', env_type= env_type)
        
        #assign(paste0('output_means_list',modelType, 'env', env_type, sep=''), mean_dfs, envir=.GlobalEnv)
        
      } # end of model 1.3.1 function 
      
      # model 1.3.2 
      mod_1_3_2<-function(days, N, env_type, th_forage_sc1, th_forage_sc2, th_forage_sc3, daylight_h){
        require(doParallel)
        require(foreach)
        # Start the model 
        # link to the function file 
        setwd("C:/Local_R/BiPhD-ABM/May23")
        source('MOD_1_FuncSource.R')
        # set the number of cores 
        numCores<-(detectCores()-1)
        registerDoParallel(numCores)
        
        # Set up the general environment 
        # This part is the same for each bird 
        set_up_func_general(days, env_type, daylight_h)
        
        ################################
        #      individual loops        # 
        ################################
        
        # The individual loops need to start now
        # These should be parallelised 
        
        outcome_1_3_2<- foreach(icount(N), .packages = "truncnorm", .combine='rbind') %do% {
          
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
            temp_cur<<-total_temp_profile[t]
            # Check if it is night or day 
            if ((t%%72)<= n_daylight_timestep){
              dayOrNight<<-1                       # this means it is day 
            } else {
              dayOrNight<<-0                       # this means it is night 
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
                
                # RULE SPECIFIC FOR MODEL 1_3_2 
                
                if ((mat_sc[i,t])> th_forage_sc3){
                  # The bird will be resting above the 3rd threshold 
                  
                  ##################
                  #    RESTING     # 
                  ##################
                  #print('a bird rests')
                  # If the bird has a SC above the second threshold, it is not hungry at all and can go rest        
                  rest_func(t,i)
                 
                  
                  
                } else if (((mat_sc[i,t])> th_forage_sc2) && ((mat_sc[i,t])<= th_forage_sc3)){
                  # This is betwen th2 and th3 , so the bird will do its direct hoarding, without eating at all 
                  
                  ############################
                  #  FORAGE + HOARD DIRECT   #
                  ############################
                  
                  #print('a bird forages and hoards')
                  
                  # At this point, foraging means to go out and find a NEW food item 
                  forage_function(t,i)
                  
                  # Then directly just hoard it
                  dir_hoard_func(t,i)
           
                  
                } else if (((mat_sc[i,t])<=th_forage_sc1) && ((mat_caches[i,t])>retrieve_min)){
                  
                  #print('a bird retrieves')
                  
                  ####################
                  ##   RETRIEVING   ## 
                  #################### 
                  
                  # The bird will retrieve food items 
                  retrieve_func(t,i)
                  # End of retrieving statement 
                  
                  # RULE SPECIFIC FOR MODEL 1_3_2 
                  
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
          list(eat_count, eat_hoard_count, forage_count, hoard_count, mat_alive, mat_caches, mat_find_food, mat_fr, mat_sc, mat_mass, predation_count, rest_count)
          
        } # end of the foreach loop (individuals) 
        
        
        # clean up cluster 
        stopImplicitCluster()
        
        #return(outcome_1_1)
        assign(paste0('outcome_1_3_2_env', env_type),outcome_1_3_2, envir=.GlobalEnv)
        
        create_df_func(outputFile = outcome_1_3_2, modelType = '132', env_type= env_type)
        
        #assign(paste0('output_means_list',modelType, 'env', env_type, sep=''), mean_dfs, envir=.GlobalEnv)
        
      } # end of model 1.3.2 function 
      
      
      ########################
      #   ENVIRONMENT LOOP   #
      ########################
      
      # The environment loop 
      env_func_1_3_1<-function(days, N, th_forage_sc1, th_forage_sc2, th_forage_sc3, daylight_h, modelType){
        
        # Loop through the environments 
        for (env in 1:18){
          if (env==1){
            list_means_envs<-list()
          }
          
          mod_1_3_1(days = days, N = N, env_type = env, th_forage_sc1 = th_forage_sc1, th_forage_sc2 = th_forage_sc2, th_forage_sc3 = th_forage_sc3, daylight_h = daylight_h)
          
          
          list_means_envs[[env]]<-mean_dfs
          
        }
        
        # print(environment())
        #list_means_envs<<-mget(ls(pattern = "output_means_list11env"))
        
        # now select only the information about survival
        list_means_envs<-lapply(list_means_envs, function(x){x$alive})
        # now find the row with the closest value of survival to 0.5 (halflife)
        halflife_per_env<-lapply(list_means_envs, function(x){x$timestep[which.min(abs(0.5-x$value))]})
        # Same for the end survival
        end_survival_per_env<-lapply(list_means_envs, function(x){x$value[x$timestep==TS]})
        
        # now put relevant data in the global environment
        # assign(paste0('HL_pEnv_th_sc', th_forage_sc), halflife_per_env, envir=.GlobalEnv)
        # assign(paste0('ES_pEnv_th_sc', th_forage_sc), end_survival_per_env, envir=.GlobalEnv)
        # assign(paste0('list_means_per_env_thsc', th_forage_sc), list_means_envs, envir=.GlobalEnv)
        
        # generate the average average end-survival for this threshold, across all the environments 
        mean_ES_cur_th<-mean(unlist(end_survival_per_env))
        # and now for the average time till halflife 
        mean_HL_cur_th<-mean(unlist(halflife_per_env))
        # do the same for the 
        
        
        output_env_func<-cbind(mean_ES_cur_th, mean_HL_cur_th)
        
        assign(paste0('output_env_function_th1=', th_forage_sc1, 'th2=', th_forage_sc2, 'th3=', th_forage_sc3), output_env_func, envir=.GlobalEnv)
        
        return(output_env_func)
        
      } # end environment function loop 
     
       # the one that runs parallel 
      env_func_1_3_1_par<-function(days, N, th_forage_sc1, th_forage_sc2, th_forage_sc3, daylight_h, modelType){
        
        
        require(doParallel)
        require(foreach)
        
        numCores<-(detectCores()-1)
        registerDoParallel(numCores)
        
        num_env<-18 
        
        outcome_env_1_3_1_par<- foreach(i=1:num_env, .packages = c( "truncnorm", "purrr")) %dopar% {
          
          setwd("C:/Local_R/BiPhD-ABM/May23")
          source('MOD_1_FuncSource.R')
          source('ModelSource.R')
          
          mod_1_3_1(days = days, N = N, env_type = i, th_forage_sc1 = th_forage_sc1, th_forage_sc2 = th_forage_sc2, th_forage_sc3= th_forage_sc3, daylight_h = daylight_h)
          
          #print('done')
          
          
        }
        
        # clean up cluster 
        stopImplicitCluster()
        
        # print(environment())
        #list_means_envs<<-mget(ls(pattern = "output_means_list11env"))
        
        # now select only the information about survival
        list_means_envs<-lapply(outcome_env_1_3_1_par, function(x){subset(x, x$id=='alive')})
        
        # now find the row with the closest value of survival to 0.5 (halflife)
        halflife_per_env<-lapply(list_means_envs, function(x){x$timestep[which.min(abs(0.5-x$value))]})
        # Same for the end survival
        end_survival_per_env<-lapply(list_means_envs, function(x){x$value[x$timestep==(days*72)]})
        
        # now put relevant data in the global environment
        # assign(paste0('HL_pEnv_th_sc', th_forage_sc), halflife_per_env, envir=.GlobalEnv)
        # assign(paste0('ES_pEnv_th_sc', th_forage_sc), end_survival_per_env, envir=.GlobalEnv)
        # assign(paste0('list_means_per_env_thsc', th_forage_sc), list_means_envs, envir=.GlobalEnv)
        
        # generate the average average end-survival for this threshold, across all the environments 
        mean_ES_cur_th<-mean(unlist(end_survival_per_env))
        # and now for the average time till halflife 
        mean_HL_cur_th<-mean(unlist(halflife_per_env))
        # do the same for the 
        
        
        output_env_func<<-cbind(mean_ES_cur_th, mean_HL_cur_th)
        
        # assign(paste0('output_env_function_th1=', th_forage_sc1, ' th2=', th_forage_sc2), output_env_func, envir=.GlobalEnv)
        
        return(output_env_func)
        
        
      } # end environment function loop 
      
      
      
      env_func_1_3_2<-function(days, N, th_forage_sc1, th_forage_sc2, th_forage_sc3, daylight_h, modelType){
        
        # Loop through the environments 
        for (env in 1:18){
          if (env==1){
            list_means_envs<-list()
          }
          
          mod_1_3_2(days = days, N = N, env_type = env, th_forage_sc1 = th_forage_sc1, th_forage_sc2 = th_forage_sc2, th_forage_sc3 = th_forage_sc3, daylight_h = daylight_h)
          
          
          list_means_envs[[env]]<-mean_dfs
          
        }
        
        # print(environment())
        #list_means_envs<<-mget(ls(pattern = "output_means_list11env"))
        
        # now select only the information about survival
        list_means_envs<-lapply(list_means_envs, function(x){x$alive})
        # now find the row with the closest value of survival to 0.5 (halflife)
        halflife_per_env<-lapply(list_means_envs, function(x){x$timestep[which.min(abs(0.5-x$value))]})
        # Same for the end survival
        end_survival_per_env<-lapply(list_means_envs, function(x){x$value[x$timestep==TS]})
        
        # now put relevant data in the global environment
        # assign(paste0('HL_pEnv_th_sc', th_forage_sc), halflife_per_env, envir=.GlobalEnv)
        # assign(paste0('ES_pEnv_th_sc', th_forage_sc), end_survival_per_env, envir=.GlobalEnv)
        # assign(paste0('list_means_per_env_thsc', th_forage_sc), list_means_envs, envir=.GlobalEnv)
        
        # generate the average average end-survival for this threshold, across all the environments 
        mean_ES_cur_th<-mean(unlist(end_survival_per_env))
        # and now for the average time till halflife 
        mean_HL_cur_th<-mean(unlist(halflife_per_env))
        # do the same for the 
        
        
        output_env_func<-cbind(mean_ES_cur_th, mean_HL_cur_th)
        
        assign(paste0('output_env_function_th1=', th_forage_sc1, 'th2=', th_forage_sc2, 'th3=', th_forage_sc3), output_env_func, envir=.GlobalEnv)
        
        return(output_env_func)
        
      } # end environment function loop 
      
      
      
      
      
#################################################################
##     Model 2.1: Non-hoarding bird, Access to Fat-reserves    ##
#################################################################
      
      # model 2.1 
      mod_2_1<-function(days, N, env_type, th_forage_fr, daylight_h){
        # Necessary packages if running the environments parallel 
        require(foreach)
        require(doParallel)
        # Start the model 
        # link to the function file 
        setwd("C:/Local_R/BiPhD-ABM/May23")
        source('MOD_1_FuncSource.R')
        # set the number of cores 
        numCores<-(detectCores()-1)
        registerDoParallel(numCores)
        
        # Set up the general environment 
        # This part is the same for each bird 
        set_up_func_general(days, env_type, daylight_h)
        
        
        ################################
        #      individual loops        # 
        ################################
        
        # The individual loops need to start now
        # These should be parallelised 
        
        outcome_2_1<-foreach(icount(N), .packages = "truncnorm", .combine='rbind') %do% {
          
          # link to the function file 
          # setwd("C:/Local_R/BiPhD-ABM/")
          # source('MOD_1_FuncSource.R')
          
          # Do a setup for the individual bird
          # This includes the individual temperature pattern 
          # And individual matrices 
          #print('here')
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
            temp_cur<<-total_temp_profile[t]
            # Check if it is night or day 
            if ((t%%72)<= n_daylight_timestep){
              dayOrNight<<-1                       # this means it is day 
            } else {
              dayOrNight<<-0                       # this means it is night 
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
                if ((mat_fr[i,t]) < th_forage_fr){
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
          list(eat_count, eat_hoard_count, forage_count, hoard_count, mat_alive, mat_caches, mat_find_food, mat_fr, mat_sc, mat_mass, predation_count, rest_count)
          
        } # end of the foreach loop (individuals) 
        
        # clean up cluster 
        stopImplicitCluster()
        
        #return(outcome_1_1)
        assign(paste0('outcome_2_1_env', env_type),outcome_2_1, envir=.GlobalEnv)
        
        create_df_func(outputFile = outcome_2_1, modelType = '21', env_type= env_type)
        
        #assign(paste0('output_means_list',modelType, 'env', env_type, sep=''), mean_dfs, envir=.GlobalEnv)
        
        
        
        
        
      } # end of model 2.1 function 
      
      
      ########################
      #   ENVIRONMENT LOOP   #
      ########################
      
      # The environment loop 
      env_func_2_1<-function(days, N, th_forage_fr, daylight_h, modelType){
        
        # Loop through the environments 
        for (env in 1:18){
          if (env==1){
            list_means_envs<-list()
          }
          
          mod_2_1(days = days, N = N, env_type = env, th_forage_fr = th_forage_fr, daylight_h = daylight_h)
          
          
          list_means_envs[[env]]<-mean_dfs
          
        }
        
        # print(environment())
        #list_means_envs<<-mget(ls(pattern = "output_means_list11env"))
        
        # now select only the information about survival
        list_means_envs<-lapply(list_means_envs, function(x){x$alive})
        # now find the row with the closest value of survival to 0.5 (halflife)
        halflife_per_env<-lapply(list_means_envs, function(x){x$timestep[which.min(abs(0.5-x$value))]})
        # Same for the end survival
        end_survival_per_env<-lapply(list_means_envs, function(x){x$value[x$timestep==TS]})
        
        # now put relevant data in the global environment
        # assign(paste0('HL_pEnv_th_sc', th_forage_sc), halflife_per_env, envir=.GlobalEnv)
        # assign(paste0('ES_pEnv_th_sc', th_forage_sc), end_survival_per_env, envir=.GlobalEnv)
        # assign(paste0('list_means_per_env_thsc', th_forage_sc), list_means_envs, envir=.GlobalEnv)
        
        # generate the average average end-survival for this threshold, across all the environments 
        mean_ES_cur_th<-mean(unlist(end_survival_per_env))
        # and now for the average time till halflife 
        mean_HL_cur_th<-mean(unlist(halflife_per_env))
        # do the same for the 
        
        
        output_env_func<-cbind(mean_ES_cur_th, mean_HL_cur_th)
        
        assign(paste0('output_env_function_th', th_forage_sc), output_env_func, envir=.GlobalEnv)
        
        return(output_env_func)
        
        # Set the coefficient for plotting the optimisations 
        #assign(total_timesteps, (days*72), envir = .GlobalEnv)
        
      } # end environment function loop 
      
      # Parallel version 
      env_func_2_1_par<-function(days, N, th_forage_fr, daylight_h, modelType){
        
        require(doParallel)
        require(foreach)
        
        numCores<-(detectCores()-1)
        registerDoParallel(numCores)
        
        num_env<-18 
        
        outcome_env_2_1_par<- foreach(i=1:num_env, .packages = c( "truncnorm", "purrr")) %dopar% {
          
          setwd("C:/Local_R/BiPhD-ABM/May23")
          source('MOD_1_FuncSource.R')
          source('ModelSource.R')
          
          mod_2_1(days = days, N = N, env_type = i, th_forage_fr = th_forage_fr, daylight_h = daylight_h)
          
          #print('done')
          
          
        }
        
        # clean up cluster 
        stopImplicitCluster()
        
        # print(environment())
        #list_means_envs<<-mget(ls(pattern = "output_means_list11env"))
        
        # now select only the information about survival
        list_means_envs<-lapply(outcome_env_2_1_par, function(x){subset(x, x$id=='alive')})
        
        # now find the row with the closest value of survival to 0.5 (halflife)
        halflife_per_env<-lapply(list_means_envs, function(x){x$timestep[which.min(abs(0.5-x$value))]})
        # Same for the end survival
        end_survival_per_env<-lapply(list_means_envs, function(x){x$value[x$timestep==(days*72)]})
        
        # now put relevant data in the global environment
        # assign(paste0('HL_pEnv_th_sc', th_forage_sc), halflife_per_env, envir=.GlobalEnv)
        # assign(paste0('ES_pEnv_th_sc', th_forage_sc), end_survival_per_env, envir=.GlobalEnv)
        # assign(paste0('list_means_per_env_thsc', th_forage_sc), list_means_envs, envir=.GlobalEnv)
        
        # generate the average average end-survival for this threshold, across all the environments 
        mean_ES_cur_th<-mean(unlist(end_survival_per_env))
        # and now for the average time till halflife 
        mean_HL_cur_th<-mean(unlist(halflife_per_env))
        # do the same for the 
        
        
        output_env_func<<-cbind(mean_ES_cur_th, mean_HL_cur_th)
        
        # assign(paste0('output_env_function_th1=', th_forage_sc1, ' th2=', th_forage_sc2), output_env_func, envir=.GlobalEnv)
        
        return(output_env_func)
        
      } # end environment function loop 
      
      
      
      
  ######################################################################
  ##   Model 2.2: Leftover-hoarding bird, Access to Fat-Reserves      ##
  ######################################################################
      
      # model 2.2 
      mod_2_2<-function(days, N, env_type, th_forage_fr1, th_forage_fr2, daylight_h){
        require(foreach)
        require(doParallel)
        # Start the model 
        # link to the function file 
        setwd("C:/Local_R/BiPhD-ABM/May23")
        source('MOD_1_FuncSource.R')
        # set the number of cores 
        #umCores<-(detectCores()-1)
        #registerDoParallel(numCores)
        
        # Set up the general environment 
        # This part is the same for each bird 
        set_up_func_general(days, env_type, daylight_h)
        
        ################################
        #      individual loops        # 
        ################################
        
        # The individual loops need to start now
        # These should be parallelised 
        outcome_2_2<- foreach(icount(N), .packages = "truncnorm", .combine='rbind') %do% {
          
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
            temp_cur<<-total_temp_profile[t]
            # Check if it is night or day 
            if ((t%%72)<= n_daylight_timestep){
              dayOrNight<<-1                       # this means it is day 
            } else {
              dayOrNight<<-0                       # this means it is night 
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
                  
                  # RULE SPECIFIC FOR MODEL 2_2 
                  
                } else if ((mat_sc[i,t]) >= th_forage_fr2){
                  
                  ##################
                  #    RESTING     # 
                  ##################
                  
                  # If the bird has a SC above the second threshold, it is not hungry at all and can go rest        
                  rest_func(t,i)
                  
                  # RULE SPECIFIC FOR MODEL 2_2 
                  
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
          list(eat_count, eat_hoard_count, forage_count, hoard_count, mat_alive, mat_caches, mat_find_food, mat_fr, mat_sc, mat_mass,  predation_count, rest_count)
          
        } # end of the foreach loop (individuals) 
        
        # clean up cluster 
        stopImplicitCluster()
        
        # assign the dataframe and return it to global environment
        assign(paste0('outcome_2_2_env', env_type),outcome_2_2, envir=.GlobalEnv)
        
        create_df_func(outputFile = outcome_2_2, modelType = '22', env_type= env_type)
        
        #assign(paste0('output_means_list',modelType, 'env', env_type, sep=''), mean_dfs, envir=.GlobalEnv)
        
      } # end of model 2.2 function 
      
      
      ########################
      #   ENVIRONMENT LOOP   #
      ########################

      
      # environment loop paralelel 
      env_func_2_2_par<-function(days, N, th_forage_fr1, th_forage_fr2, daylight_h, modelType){
        
        require(doParallel)
        require(foreach)
        
        numCores<-(detectCores()-1)
        registerDoParallel(numCores)
        
        num_env<-18 
        
        outcome_env_2_2_par<- foreach(i=1:num_env, .packages = c( "truncnorm", "purrr")) %dopar% {
          
          setwd("C:/Local_R/BiPhD-ABM/May23")
          source('MOD_1_FuncSource.R')
          source('ModelSource.R')
          
          mod_2_2(days = days, N = N, env_type = i, th_forage_fr1 = th_forage_fr1, th_forage_fr2 = th_forage_fr2, daylight_h = daylight_h)
          
          #print('done')
          
          
        }
        
        # clean up cluster 
        stopImplicitCluster()
        
        # print(environment())
        #list_means_envs<<-mget(ls(pattern = "output_means_list11env"))
        
        # now select only the information about survival
        list_means_envs<-lapply(outcome_env_2_2_par, function(x){subset(x, x$id=='alive')})
        
        # now find the row with the closest value of survival to 0.5 (halflife)
        halflife_per_env<-lapply(list_means_envs, function(x){x$timestep[which.min(abs(0.5-x$value))]})
        # Same for the end survival
        end_survival_per_env<-lapply(list_means_envs, function(x){x$value[x$timestep==(days*72)]})
        
        # now put relevant data in the global environment
        # assign(paste0('HL_pEnv_th_sc', th_forage_sc), halflife_per_env, envir=.GlobalEnv)
        # assign(paste0('ES_pEnv_th_sc', th_forage_sc), end_survival_per_env, envir=.GlobalEnv)
        # assign(paste0('list_means_per_env_thsc', th_forage_sc), list_means_envs, envir=.GlobalEnv)
        
        # generate the average average end-survival for this threshold, across all the environments 
        mean_ES_cur_th<-mean(unlist(end_survival_per_env))
        # and now for the average time till halflife 
        mean_HL_cur_th<-mean(unlist(halflife_per_env))
        # do the same for the 
        
        
        output_env_func<<-cbind(mean_ES_cur_th, mean_HL_cur_th)
        
        # assign(paste0('output_env_function_th1=', th_forage_sc1, ' th2=', th_forage_sc2), output_env_func, envir=.GlobalEnv)
        
        return(output_env_func)
        
      } # end environment function loop 
      
      
