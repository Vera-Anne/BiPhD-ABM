#################################################################################
# Small bird in winter - ABM 
# Start date: 16/05/2023
# Vera Vinken 
# all models - for general use 
# LAST UPDATE: 18/09/2023
#################################################################################


#################################################################
##   Model 1.1: Non-hoarding bird, Access to Stomach Content   ##
#################################################################

# model 1.1 
mod_1_1<-function(days, N, env_type, th_forage_sc, daylight_h){
  # Make the variables global 
  days<<-days
  N<<-N
  env_type<<-env_type
  th_forage_sc<<-th_forage_sc
  daylight_h<<-daylight_h
  
  # load necessary packages for parallel (if needed)
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
  
  outcome_1_1<-foreach(icount(N), .packages =c("truncnorm", "SciViews"), .combine='rbind') %do% {
    
    # link to the function file 
    # setwd("C:/Local_R/BiPhD-ABM/")
    # source('MOD_1_FuncSource.R')
    
    # Do a setup for the individual bird
    # This includes the individual temperature pattern 
    
    
    # Run hte temperature function 
    # Running this seperately for each individual brings in some desired stochasticity 
    temp_func(TS, Tmax_range_low, Tmax_range_high, Tmin_range_low, Tmin_range_high, days, daylight_h, n_daylight_timestep)
    
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
        
        # Calculate the current fat loss rate 
        flr_func(t,i)
        
        # Set the current temperature 
        temp_cur<<-total_temp_profile[t]
        
        # Check if it is night or day 
        if ((t%%72)<= n_daylight_timestep){
          dayOrNight<<-1                       # this means it is day 
        } else {
          dayOrNight<<-0                       # this means it is night 
        }
        
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
        
        ####################
        #    PILFERAGE     #
        ####################
        # Pilfer, but only if there are actually caches left 
        if(mat_caches[i,t]>0){
        pilf_func(i=i,t=t,lambda=lambda)
        }
        
        ##################################
        #   PREPARE FOR NEXT TIMESTEP   # 
        ##################################
        
        ts_prep_func(t,i, TS)
        
      } # end of loop for alive individuals 
      
      
    } # end timestep loop
    
    # Alternatively, I could try to create lists with the output 
    list(eat_count, eat_hoard_count, forage_count, hoard_count, mat_alive, mat_caches, mat_find_food, mat_fr, mat_sc, mat_flr, mat_mass, predation_count, rest_count, retrieve_count, sleep_count, decay_count,mat_temp)
    
  } # end of the foreach loop (individuals) 
  
  # clean up cluster 
  stopImplicitCluster()
  
  #return(outcome_1_1)
  assign(paste0('outcome_1_1_env', env_type),outcome_1_1, envir=.GlobalEnv)
  
  create_df_func(outputFile = outcome_1_1, modelType = '11', env_type= env_type)

} # end of model 1 function 

# model 1.1 
mod_1_1_hpc<-function(days, N, env_type, th_forage_sc, daylight_h){
  # Make the variables global 
  days<<-days
  N<<-N
  env_type<<-env_type
  th_forage_sc<<-th_forage_sc
  daylight_h<<-daylight_h
  
  # load necessary packages for parallel (if needed)
  require(foreach)
  require(doParallel)
  # Start the model 
  
  # Set up the general environment 
  # This part is the same for each bird 
  set_up_func_general(days, env_type, daylight_h)

  ################################
  #      individual loops        # 
  ################################
  
  # The individual loops need to start now
  # These should be parallelised 
  
  outcome_1_1<-foreach(icount(N), .packages = "truncnorm", .combine='rbind') %do% {
    
    # Do a setup for the individual bird
    # This includes the individual temperature pattern 
    
    # Run hte temperature function 
    # Running this seperately for each individual brings in some desired stochasticity 
    temp_func(TS, Tmax_range_low, Tmax_range_high, Tmin_range_low, Tmin_range_high, days, daylight_h, n_daylight_timestep)
    
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
        
        # Calculate the current fat loss rate 
        flr_func(t,i)
        
        # Set the current temperature 
        temp_cur<<-total_temp_profile[t]
        
        # Check if it is night or day 
        if ((t%%72)<= n_daylight_timestep){
          dayOrNight<<-1                       # this means it is day 
        } else {
          dayOrNight<<-0                       # this means it is night 
        }
        
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
        ####################
        #    PILFERAGE     #
        ####################
        # Pilfer, but only if there are actually caches left 
        if(mat_caches[i,t]>0){
          pilf_func(i=i,t=t,lambda=lambda)
        }
        
        ##################################
        #   PREPARE FOR NEXT TIMESTEP   # 
        ##################################
        
        ts_prep_func(t,i, TS)
        
      } # end of loop for alive individuals 
      
    } # end timestep loop
    
    # Alternatively, I could try to create lists with the output 
    list(eat_count, eat_hoard_count, forage_count, hoard_count, mat_alive, mat_caches, mat_find_food, mat_fr, mat_sc, mat_flr, mat_mass, predation_count, rest_count, retrieve_count, sleep_count,decay_count, mat_temp)
    
  } # end of the foreach loop (individuals) 
  
  # clean up cluster 
  stopImplicitCluster()
  
  #return(outcome_1_1)
  assign(paste0('outcome_1_1_env', env_type),outcome_1_1, envir=.GlobalEnv)
  
  create_df_func(outputFile = outcome_1_1, modelType = '11', env_type= env_type)
 
} # end of model 1 function - HPC 


########################
#   ENVIRONMENT LOOP   #
########################

# environment loop paralelel 
env_func_1_1_par<-function(days, N, th_forage_sc, daylight_h, modelType){
  
  require(doParallel)
  require(foreach)
  
  numCores<-(detectCores()-1)
  registerDoParallel(numCores)
  
  num_env<-18 
  
  outcome_env_1_1_par<- foreach(i=1:num_env, .packages = c( "truncnorm", "purrr", "SciViews")) %dopar% {
    
    setwd("C:/Local_R/BiPhD-ABM/May23")
    source('MOD_1_FuncSource.R')
    source('ModelSource.R')
    
    mod_1_1(days = days, N = N, env_type = i, th_forage_sc = th_forage_sc, daylight_h = daylight_h)
    
  }
  
  # clean up cluster 
  stopImplicitCluster()
  # or try this one maybe 
  #stopCluster()
  
  # The result we have at this point is 'outcome_env_1_1_par' 
  # These will have the same form for each of the models
  # So I will need to write a function that takes care of this and outputs the t_halflife 
  
  # load any functions 
  setwd("C:/Local_R/BiPhD-ABM/May23")
  source('MOD_1_FuncSource.R')
  source('ModelSource.R')
  
  # Create the variable called halflife_input
  halflife_input<-outcome_env_1_1_par
  
  # run the t_halflife function 
  t_halflife_func(halflife_input)
  # put the output into a dataframe 
  t_HL_df<-map_dfr(t_HL_list, ~as.data.frame(t(.x)))
  t_HL_df$env<-1:18
  
  # Calculate mean 
  t_HL_mean<-mean(t_HL_df$V1)
  t_HL_SD<-sd(t_HL_df$V1)
  
  
  performance<<-cbind(t_HL_mean, t_HL_SD)
  colnames(performance)<-c('mean', 'SD')
  output_env_func<<-list(performance, outcome_env_1_1_par)
  
  # # save the data 
  setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/MOD_1_1/env_par")
  save(output_env_func, file=paste0(format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_env_func_out_', modelType, 'd', days, 'N', N,'th_sc', th_forage_sc, 'dayh', daylight_h,   '.Rda'))
  
  
  return(output_env_func)
  
  
} # end environment function loop 
# parallel for the hpc 
env_func_1_1_par_hpc<-function(days, N, th_forage_sc, daylight_h, modelType){
  
  require(doParallel)
  require(foreach)
  
  numCores<-(10)
  registerDoParallel(numCores)
  
  num_env<-18 
  
  outcome_env_1_1_par<- foreach(i=1:num_env, .packages = c( "truncnorm", "purrr", "SciViews")) %dopar% {

    # Run the model 
    mod_1_1_hpc(days = days, N = N, env_type = i, th_forage_sc = th_forage_sc, daylight_h = daylight_h)
    
  }
  
  # clean up cluster 
  stopImplicitCluster()
  # or try this one maybe 
  #stopCluster()
  
  # The result we have at this point is 'outcome_env_1_1_par' 
  # These will have the same form for each of the models
  # So I will need to write a function that takes care of this and outputs the t_halflife 
  
  # Create the variable called halflife_input
  halflife_input<-outcome_env_1_1_par
  
  # run the t_halflife function 
  t_halflife_func(halflife_input)
  # put the output into a dataframe 
  t_HL_df<-map_dfr(t_HL_list, ~as.data.frame(t(.x)))
  t_HL_df$env<-1:18
  
  # Calculate mean 
  t_HL_mean<-mean(t_HL_df$V1)
  t_HL_SD<-sd(t_HL_df$V1)
  
  
  performance<<-cbind(t_HL_mean, t_HL_SD)
  colnames(performance)<-c('mean', 'SD')
  output_env_func<<-list(performance, outcome_env_1_1_par)
  
  return(output_env_func)
  
  
} # end environment function loop 

######################################################################
##   Model 1.2: Leftover-hoarding bird, Access to Stomach Content   ##
######################################################################

# model 1.2 
mod_1_2<-function(days, N, env_type, th_forage_sc1, th_forage_sc2, daylight_h){
  # make some of the variables global for saving 
  days<<-days
  N<<-N
  env_type<<-env_type
  th_forage_sc1<<-th_forage_sc1
  th_forage_sc2<<-th_forage_sc2
  daylight_h<<-daylight_h
  
  # load packages for parallel if needed 
  require(foreach)
  require(doParallel)
  # Start the model 
  # link to the function file 
  setwd("C:/Local_R/BiPhD-ABM/May23")
  source('MOD_1_FuncSource.R')

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
    
    # Run hte temperature function 
    # Running this seperately for each individual brings in some desired stochasticity 
    temp_func(TS, Tmax_range_low, Tmax_range_high, Tmin_range_low, Tmin_range_high, days, daylight_h, n_daylight_timestep)
    
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
        
        # Calculate the current fat loss rate 
        flr_func(t,i)
        
        # Set the current temperature 
        temp_cur<<-total_temp_profile[t]
        # Check if it is night or day 
        if ((t%%72)<= n_daylight_timestep){
          dayOrNight<<-1                       # this means it is day 
        } else {
          dayOrNight<<-0                       # this means it is night 
        }
        
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
        
        ####################
        #    PILFERAGE     #
        ####################
        # Pilfer, but only if there are actually caches left 
        if(mat_caches[i,t]>0){
          pilf_func(i=i,t=t,lambda=lambda)
        }

        ##################################
        #   PREPARE FOR NEXT TIMESTEP   # 
        ##################################
        
        ts_prep_func(t,i, TS)
        
      } # end of loop for alive individuals 
      
    } # end timestep loop
    
    # Alternatively, I could try to create lists with the output 
    list(eat_count, eat_hoard_count, forage_count, hoard_count, mat_alive, mat_caches, mat_find_food, mat_fr, mat_sc, mat_flr, mat_mass,  predation_count, rest_count, retrieve_count, sleep_count, decay_count, decay_count,mat_temp)
    
  } # end of the foreach loop (individuals) 
  
  stopImplicitCluster()     # clean up cluster 
  
  #return(outcome_1_1)
  assign(paste0('outcome_1_2_env', env_type),outcome_1_2, envir=.GlobalEnv)
  
  create_df_func(outputFile = outcome_1_2, modelType = '12', env_type= env_type)
  
} # end of model 1.2 function - LOCAL 

# model 1.2 for hpc 
mod_1_2_hpc<-function(days, N, env_type, th_forage_sc1, th_forage_sc2, daylight_h){
  # make some of the variables global for saving 
  days<<-days
  N<<-N
  env_type<<-env_type
  th_forage_sc1<<-th_forage_sc1
  th_forage_sc2<<-th_forage_sc2
  daylight_h<<-daylight_h
  
  # load packages for parallel if needed 
  require(foreach)
  require(doParallel)
  
  # Start the model 
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
    
    # Run hte temperature function 
    # Running this seperately for each individual brings in some desired stochasticity 
    temp_func(TS, Tmax_range_low, Tmax_range_high, Tmin_range_low, Tmin_range_high, days, daylight_h, n_daylight_timestep)
    
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

        # Calculate the current fat loss rate 
        flr_func(t,i)

        # Set the current temperature 
        temp_cur<<-total_temp_profile[t]
        # Check if it is night or day 
        if ((t%%72)<= n_daylight_timestep){
          dayOrNight<<-1                       # this means it is day 
        } else {
          dayOrNight<<-0                       # this means it is night 
        }
        
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
        
        ####################
        #    PILFERAGE     #
        ####################
        # Pilfer, but only if there are actually caches left 
        if(mat_caches[i,t]>0){
          pilf_func(i=i,t=t,lambda=lambda)
        }
        
        ##################################
        #   PREPARE FOR NEXT TIMESTEP   # 
        ##################################
        
        ts_prep_func(t,i, TS)
        
      } # end of loop for alive individuals 

    } # end timestep loop
    
    # Alternatively, I could try to create lists with the output 
    list(eat_count, eat_hoard_count, forage_count, hoard_count, mat_alive, mat_caches, mat_find_food, mat_fr, mat_sc, mat_flr, mat_mass,  predation_count, rest_count, retrieve_count, sleep_count, decay_count,mat_temp)
    
  } # end of the foreach loop (individuals) 
  
  # clean up cluster 
  stopImplicitCluster()
  
  #return(outcome_1_1)
  assign(paste0('outcome_1_2_env', env_type),outcome_1_2, envir=.GlobalEnv)
  
  create_df_func(outputFile = outcome_1_2, modelType = '12', env_type= env_type)
  
  #assign(paste0('output_means_list',modelType, 'env', env_type, sep=''), mean_dfs, envir=.GlobalEnv)
  
} # end of model 1.2 function - HPC

########################
#   ENVIRONMENT LOOP   #
########################

# environment loop paralelel 
env_func_1_2_par<-function(days, N, th_forage_sc1, th_forage_sc2, daylight_h, modelType){
  # parallel computing 
  require(doParallel)
  require(foreach)
  numCores<-(detectCores()-1)
  registerDoParallel(numCores)
  # hardcode env 
  num_env<-18 
  # start loop
  outcome_env_1_2_par<- foreach(i=1:num_env, .packages = c( "truncnorm", "purrr", "SciViews")) %dopar% {
    # load files 
    setwd("C:/Local_R/BiPhD-ABM/May23")
    source('MOD_1_FuncSource.R')
    source('ModelSource.R')
    # run model
    mod_1_2(days = days, N = N, env_type = i, th_forage_sc1 = th_forage_sc1, th_forage_sc2 = th_forage_sc2, daylight_h = daylight_h)
  }
  
  # clean up cluster 
  stopImplicitCluster()
  
  # The result we have at this point is 'outcome_env_1_2_par' 
  # This is a list with the averages fo each behaviour/usrvival/physiology for each timestep per environment 
  # It will go into the halflife function, so we find out at what point in time half of the birds are alive 
  
  # load any functions 
  setwd("C:/Local_R/BiPhD-ABM/May23")
  source('MOD_1_FuncSource.R')
  source('ModelSource.R')
  
  # Create the variable called halflife_input
  halflife_input<-outcome_env_1_2_par
  
  # run the t_halflife function 
  t_halflife_func(halflife_input)
  # put the output into a dataframe 
  t_HL_df<-map_dfr(t_HL_list, ~as.data.frame(t(.x)))
  t_HL_df$env<-1:18
  
  # Calculate mean 
  t_HL_mean<-mean(t_HL_df$V1)
  t_HL_SD<-sd(t_HL_df$V1)
  
  # prepare for output of the env_function 
  performance<<-cbind(t_HL_mean, t_HL_SD)
  colnames(performance)<-c('mean', 'SD')
  output_env_func<<-list(performance, outcome_env_1_2_par)
  
  # # save the data 
  setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/MOD_1_2/env_par")
  save(output_env_func, file=paste0(format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_env_func_out_', 'd', days, 'N', N, 'th1', th_forage_sc1, 'th2', th_forage_sc2, 'dayh', daylight_h,  '.Rda'))
  
  return(output_env_func)
  
} # end environment function loop for local parallel 12

# environment loop paralelel 
env_func_1_2_par_hpc<-function(days, N, th_forage_sc1, th_forage_sc2, daylight_h, modelType){
  # for parallel computing 
  require(doParallel)
  require(foreach)
  numCores<-(10)
  registerDoParallel(numCores)
  num_env<-18 
  # start parallel loop through environments 
  outcome_env_1_2_par<- foreach(i=1:num_env, .packages = c( "truncnorm", "purrr", "SciViews")) %dopar% {
    
    # Run the model 
    mod_1_2_hpc(days = days, N = N, env_type = i, th_forage_sc1 = th_forage_sc1, th_forage_sc2 = th_forage_sc2, daylight_h = daylight_h)
  }
  
  # clean up cluster 
  stopImplicitCluster()

  # The result we have at this point is 'outcome_env_1_2_par' 
  # This is a list with the averages fo each behaviour/usrvival/physiology for each timestep per environment 
  # It will go into the halflife function, so we find out at what point in time half of the birds are alive 

  # Create the variable called halflife_input
  halflife_input<-outcome_env_1_2_par
  
  # run the t_halflife function 
  t_halflife_func(halflife_input)
  # put the output into a dataframe 
  t_HL_df<-map_dfr(t_HL_list, ~as.data.frame(t(.x)))
  t_HL_df$env<-1:18
  
  # Calculate mean 
  t_HL_mean<-mean(t_HL_df$V1)
  t_HL_SD<-sd(t_HL_df$V1)

  # prepare for output of the env_function 
  performance<<-cbind(t_HL_mean, t_HL_SD)
  colnames(performance)<-c('mean', 'SD')
  output_env_func<<-list(performance, outcome_env_1_2_par)
  
  return(output_env_func)
  
} # end environment function loop 
   

######################################################################
##    Model 1.3: direct-hoarding bird, Access to Stomach Content    ##
######################################################################

# model 1.3.1 
mod_1_3_1<-function(days, N, env_type, th_forage_sc1, th_forage_sc2, th_forage_sc3, daylight_h){
  # make some variables global 
  days<<-days
  N<<-N
  env_type<<-env_type
  th_forage_sc1<<-th_forage_sc1
  th_forage_sc2<<-th_forage_sc2
  th_forage_sc3<<-th_forage_sc3
  daylight_h<<-daylight_h
  
  # load packages if needed 
  require(doParallel)
  require(foreach)
  # Start the model 
  
  # link to the function file 
  setwd("C:/Local_R/BiPhD-ABM/May23")
  source('MOD_1_FuncSource.R')
  
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
    
    # Run hte temperature function 
    # Running this seperately for each individual brings in some desired stochasticity 
    temp_func(TS, Tmax_range_low, Tmax_range_high, Tmin_range_low, Tmin_range_high, days, daylight_h, n_daylight_timestep)
    
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
        
        # Calculate the current fat loss rate 
        flr_func(t,i)

        # Set the current temperature 
        temp_cur<<-total_temp_profile[t]
        # Check if it is night or day 
        if ((t%%72)<= n_daylight_timestep){
          dayOrNight<<-1                       # this means it is day 
        } else {
          dayOrNight<<-0                       # this means it is night 
        }
        
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

            # At this point, foraging means to go out and find a NEW food item 
            forage_function(t,i)
            
            # Then directly just hoard it
            dir_hoard_func(t,i)
            
          } else if (((mat_sc[i,t])> th_forage_sc2)){
            # The bird will be resting 
            
            ##################
            #    RESTING     # 
            ##################
          
            # If the bird has a SC above the second threshold, it is not hungry at all and can go rest        
            rest_func(t,i)
            
          } else if (((mat_sc[i,t])<=th_forage_sc1) && ((mat_caches[i,t])>retrieve_min)){

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
        
        ####################
        #    PILFERAGE     #
        ####################
        # Pilfer, but only if there are actually caches left 
        if(mat_caches[i,t]>0){
          pilf_func(i=i,t=t,lambda=lambda)
        }

        ##################################
        #   PREPARE FOR NEXT TIMESTEP   # 
        ##################################
        
        ts_prep_func(t,i, TS)
        
      } # end of loop for alive individuals 

    } # end timestep loop

    # Alternatively, I could try to create lists with the output 
    list(eat_count, eat_hoard_count, forage_count, hoard_count, mat_alive, mat_caches, mat_find_food, mat_fr, mat_sc, mat_flr,mat_mass,  predation_count, rest_count, retrieve_count, sleep_count, decay_count,decay_count,mat_temp)
    
  } # end of the foreach loop (individuals) 
  
  # clean up cluster 
  stopImplicitCluster()
  
  #return(outcome_1_1)
  assign(paste0('outcome_1_3_1_env', env_type),outcome_1_3_1, envir=.GlobalEnv)
  
  create_df_func(outputFile = outcome_1_3_1, modelType = '131', env_type= env_type)

} # end of model 1.3.1 function - LOCAL 

# model 1.3.1 
mod_1_3_1_hpc<-function(days, N, env_type, th_forage_sc1, th_forage_sc2, th_forage_sc3, daylight_h){
  # make some variables global 
  days<<-days
  N<<-N
  env_type<<-env_type
  th_forage_sc1<<-th_forage_sc1
  th_forage_sc2<<-th_forage_sc2
  th_forage_sc3<<-th_forage_sc3
  daylight_h<<-daylight_h
  
  # load packages if needed 
  require(doParallel)
  require(foreach)
  # Start the model 
  
   # Set up the general environment 
  # This part is the same for each bird 
  set_up_func_general(days, env_type, daylight_h)
  
  ################################
  #      individual loops        # 
  ################################
  
  # The individual loops need to start now
  # These should be parallelised 
  
  outcome_1_3_1<- foreach(icount(N), .packages =c( "truncnorm","SciViews"), .combine='rbind') %do% {
    
    # Do a setup for the individual bird
    # This includes the individual temperature pattern 
    
    # Run hte temperature function 
    # Running this seperately for each individual brings in some desired stochasticity 
    temp_func(TS, Tmax_range_low, Tmax_range_high, Tmin_range_low, Tmin_range_high, days, daylight_h, n_daylight_timestep)
    
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
        
        # Calculate the current fat loss rate 
        flr_func(t,i)

        # Set the current temperature 
        temp_cur<<-total_temp_profile[t]
        # Check if it is night or day 
        if ((t%%72)<= n_daylight_timestep){
          dayOrNight<<-1                       # this means it is day 
        } else {
          dayOrNight<<-0                       # this means it is night 
        }
        
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

            # At this point, foraging means to go out and find a NEW food item 
            forage_function(t,i)
            
            # Then directly just hoard it
            dir_hoard_func(t,i)

          } else if (((mat_sc[i,t])> th_forage_sc2)){
            # The bird will be resting 
            
            ##################
            #    RESTING     # 
            ##################
           
            # If the bird has a SC above the second threshold, it is not hungry at all and can go rest        
            rest_func(t,i)
            
          } else if (((mat_sc[i,t])<=th_forage_sc1) && ((mat_caches[i,t])>retrieve_min)){

            ####################
            ##   RETRIEVING   ## 
            #################### 
            
            # The bird will retrieve food items 
            retrieve_func(t,i)
            # End of retrieving statement 
            
          }else {
            
            # The the stomach content is larger than TH1 but smaller or equal to TH2, the bird needs to go out to forage and eat 
            # Note that the birds that tried to retrieve (low stomach content) but did not have caches are in here 
            
            ######################
            #     FORAGE  + EAT  # 
            ######################
            
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
        
        ####################
        #    PILFERAGE     #
        ####################
        # Pilfer, but only if there are actually caches left 
        if(mat_caches[i,t]>0){
          pilf_func(i=i,t=t,lambda=lambda)
        }
        
        ##################################
        #   PREPARE FOR NEXT TIMESTEP   # 
        ##################################
        
        ts_prep_func(t,i, TS)
        
      } # end of loop for alive individuals 

    } # end timestep loop
    
    
    # Alternatively, I could try to create lists with the output 
    list(eat_count, eat_hoard_count, forage_count, hoard_count, mat_alive, mat_caches, mat_find_food, mat_fr, mat_sc, mat_flr,mat_mass,  predation_count, rest_count, retrieve_count, sleep_count,decay_count, mat_temp)
    
  } # end of the foreach loop (individuals) 
  
  # clean up cluster 
  stopImplicitCluster()
  
  #return(outcome_1_1)
  assign(paste0('outcome_1_3_1_env', env_type),outcome_1_3_1, envir=.GlobalEnv)
  
  create_df_func(outputFile = outcome_1_3_1, modelType = '131', env_type= env_type)
  
} # end of model 1.3.1 function - HPC 

# model 1.3.2 
mod_1_3_2<-function(days, N, env_type, th_forage_sc1, th_forage_sc2, th_forage_sc3, daylight_h){
  # make some variables global 
  days<<-days
  N<<-N
  env_type<<-env_type
  th_forage_sc1<<-th_forage_sc1
  th_forage_sc2<<-th_forage_sc2
  th_forage_sc3<<-th_forage_sc3
  daylight_h<<-daylight_h
  
  # load packages if needed 
  require(doParallel)
  require(foreach)
  # Start the model 
  
  # link to the function file 
  setwd("C:/Local_R/BiPhD-ABM/May23")
  source('MOD_1_FuncSource.R')

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
    
    # Run hte temperature function 
    # Running this seperately for each individual brings in some desired stochasticity 
    temp_func(TS, Tmax_range_low, Tmax_range_high, Tmin_range_low, Tmin_range_high, days, daylight_h, n_daylight_timestep)
    
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
        
        # Calculate the current fat loss rate 
        flr_func(t,i)
        
        # Set the current temperature 
        temp_cur<<-total_temp_profile[t]
        # Check if it is night or day 
        if ((t%%72)<= n_daylight_timestep){
          dayOrNight<<-1                       # this means it is day 
        } else {
          dayOrNight<<-0                       # this means it is night 
        }
        
        ####################
        #     SLEEPING     #
        ####################
        
        sleep_func(t,i)
        
        if (sleep_count[i,t]==0){
          
          # RULE SPECIFIC FOR MODEL 1_3 
          
          if ((mat_sc[i,t])> th_forage_sc3){
            # This is above the third threshold, so the bird will do its direct hoarding, without eating at all 
            
            # The bird will be resting 
            
            ##################
            #    RESTING     # 
            ##################
         
            # If the bird has a SC above the second threshold, it is not hungry at all and can go rest        
            rest_func(t,i)
            
          } else if (((mat_sc[i,t])> th_forage_sc2)){
            
            ############################
            #  FORAGE + HOARD DIRECT   #
            ############################
   
            # At this point, foraging means to go out and find a NEW food item 
            forage_function(t,i)
            
            # Then directly just hoard it
            dir_hoard_func(t,i)
            
          } else if (((mat_sc[i,t])<=th_forage_sc1) && ((mat_caches[i,t])>retrieve_min)){
            
            ####################
            ##   RETRIEVING   ## 
            #################### 
            
            # The bird will retrieve food items 
            retrieve_func(t,i)
            # End of retrieving statement 

          }else {
            
            # The the stomach content is larger than TH1 but smaller or equal to TH2, the bird needs to go out to forage and eat 
            # Note that the birds that tried to retrieve (low stomach content) but did not have caches are in here 
            
            ######################
            #     FORAGE  + EAT  # 
            ######################
            
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
        
        ####################
        #    PILFERAGE     #
        ####################
        # Pilfer, but only if there are actually caches left 
        if(mat_caches[i,t]>0){
          pilf_func(i=i,t=t,lambda=lambda)
        }
        
        ##################################
        #   PREPARE FOR NEXT TIMESTEP   # 
        ##################################
        
        ts_prep_func(t,i, TS)
        
      } # end of loop for alive individuals 

    } # end timestep loop
    
    
    # Alternatively, I could try to create lists with the output 
    list(eat_count, eat_hoard_count, forage_count, hoard_count, mat_alive, mat_caches, mat_find_food, mat_fr, mat_sc, mat_flr,mat_mass,  predation_count, rest_count, retrieve_count, sleep_count, decay_count,mat_temp)
    
  } # end of the foreach loop (individuals) 

  # clean up cluster 
  stopImplicitCluster()
  
  #return(outcome_1_1)
  assign(paste0('outcome_1_3_2_env', env_type),outcome_1_3_2, envir=.GlobalEnv)
  
  create_df_func(outputFile = outcome_1_3_2, modelType = '132', env_type= env_type)
  
} # end of model 1.3.2 function - LOCAL

# model 1.3.2 for the hpc
mod_1_3_2_hpc<-function(days, N, env_type, th_forage_sc1, th_forage_sc2, th_forage_sc3, daylight_h){
  # make some variables global 
  days<<-days
  N<<-N
  env_type<<-env_type
  th_forage_sc1<<-th_forage_sc1
  th_forage_sc2<<-th_forage_sc2
  th_forage_sc3<<-th_forage_sc3
  daylight_h<<-daylight_h
  
  # load packages if needed 
  require(doParallel)
  require(foreach)
  # Start the model 

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
    
    # Run hte temperature function 
    # Running this seperately for each individual brings in some desired stochasticity 
    temp_func(TS, Tmax_range_low, Tmax_range_high, Tmin_range_low, Tmin_range_high, days, daylight_h, n_daylight_timestep)
    
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
        
        # Calculate the current fat loss rate 
        flr_func(t,i)

        # Set the current temperature 
        temp_cur<<-total_temp_profile[t]
        # Check if it is night or day 
        if ((t%%72)<= n_daylight_timestep){
          dayOrNight<<-1                       # this means it is day 
        } else {
          dayOrNight<<-0                       # this means it is night 
        }
        
        ####################
        #     SLEEPING     #
        ####################
        
        sleep_func(t,i)
        
        if (sleep_count[i,t]==0){
          
          # RULE SPECIFIC FOR MODEL 1_3 
          
          if ((mat_sc[i,t])> th_forage_sc3){
            # This is above the third threshold, so the bird will do its direct hoarding, without eating at all 
            # The bird will be resting 
            
            ##################
            #    RESTING     # 
            ##################
           
            # If the bird has a SC above the second threshold, it is not hungry at all and can go rest        
            rest_func(t,i)
            
          } else if (((mat_sc[i,t])> th_forage_sc2)){
            
            ############################
            #  FORAGE + HOARD DIRECT   #
            ############################
            
            # At this point, foraging means to go out and find a NEW food item 
            forage_function(t,i)
            
            # Then directly just hoard it
            dir_hoard_func(t,i)

          } else if (((mat_sc[i,t])<=th_forage_sc1) && ((mat_caches[i,t])>retrieve_min)){
 
            ####################
            ##   RETRIEVING   ## 
            #################### 
            
            # The bird will retrieve food items 
            retrieve_func(t,i)
            # End of retrieving statement 
            
          }else {
            
            # The the stomach content is larger than TH1 but smaller or equal to TH2, the bird needs to go out to forage and eat 
            # Note that the birds that tried to retrieve (low stomach content) but did not have caches are in here 
            
            ######################
            #     FORAGE  + EAT  # 
            ######################
            
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
        
        ####################
        #    PILFERAGE     #
        ####################
        # Pilfer, but only if there are actually caches left 
        if(mat_caches[i,t]>0){
          pilf_func(i=i,t=t,lambda=lambda)
        }
        
        ##################################
        #   PREPARE FOR NEXT TIMESTEP   # 
        ##################################
        
        ts_prep_func(t,i, TS)
        
      } # end of loop for alive individuals 
      
    } # end timestep loop

    # Alternatively, I could try to create lists with the output 
    list(eat_count, eat_hoard_count, forage_count, hoard_count, mat_alive, mat_caches, mat_find_food, mat_fr, mat_sc, mat_flr,mat_mass,  predation_count, rest_count, retrieve_count, sleep_count,decay_count, mat_temp)
    
  } # end of the foreach loop (individuals) 

  # clean up cluster 
  stopImplicitCluster()

  assign(paste0('outcome_1_3_2_env', env_type),outcome_1_3_2, envir=.GlobalEnv)
  
  create_df_func(outputFile = outcome_1_3_2, modelType = '132', env_type= env_type)
  
} # end of model 1.3.2 function 


########################
#   ENVIRONMENT LOOP   #
########################

# the one that runs parallel 
env_func_1_3_1_par<-function(days, N, th_forage_sc1, th_forage_sc2, th_forage_sc3, daylight_h, modelType){
  # for parallel computing 
  require(doParallel)
  require(foreach)
  numCores<-(detectCores()-1)
  registerDoParallel(numCores)
  # set number of environments 
  num_env<-18 
  # start parallel loop 
  outcome_env_1_3_1_par<- foreach(i=1:num_env, .packages = c( "truncnorm", "purrr", "SciViews")) %dopar% {
    # set source files 
    setwd("C:/Local_R/BiPhD-ABM/May23")
    source('MOD_1_FuncSource.R')
    source('ModelSource.R')
    # run the model 
    mod_1_3_1(days = days, N = N, env_type = i, th_forage_sc1 = th_forage_sc1, th_forage_sc2 = th_forage_sc2, th_forage_sc3= th_forage_sc3, daylight_h = daylight_h)
  }
  
  # clean up cluster 
  stopImplicitCluster()
  
  # The result we have at this point is 'outcome_env_1_3_1_par' 
  # This is a list with the averages fo each behaviour/usrvival/physiology for each timestep per environment 
  # It will go into the halflife function, so we find out at what point in time half of the birds are alive 
  
  # load any functions 
  setwd("C:/Local_R/BiPhD-ABM/May23")
  source('MOD_1_FuncSource.R')
  source('ModelSource.R')

  # Create the variable called halflife_input
  halflife_input<-outcome_env_1_3_1_par
  
  # run the t_halflife function 
  t_halflife_func(halflife_input)
  # put the output into a dataframe 
  t_HL_df<-map_dfr(t_HL_list, ~as.data.frame(t(.x)))
  t_HL_df$env<-1:18
  
  # Calculate mean 
  t_HL_mean<-mean(t_HL_df$V1)
  t_HL_SD<-sd(t_HL_df$V1)

  # prepare for output of the env_function 
  performance<<-cbind(t_HL_mean, t_HL_SD)
  colnames(performance)<-c('mean', 'SD')
  output_env_func<<-list(performance, outcome_env_1_3_1_par)
  
  # # save the data 
  setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/MOD_1_3_1/env_par")
  save(output_env_func, file=paste0(format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_env_func_out_131', 'd', days, 'N', N, 'th1', th_forage_sc1, 'th2', th_forage_sc2, 'th3', th_forage_sc3, 'dayh', daylight_h,  '.Rda'))
  
  # RETURN IF NEEDED FOR OPTIIZATION/ MAKING GRAPHS 
  return(output_env_func)

} # end environment function loop 

# the one that runs parallel 
env_func_1_3_1_par_hpc<-function(days, N, th_forage_sc1, th_forage_sc2, th_forage_sc3, daylight_h, modelType){
  # For parallel computing 
  require(doParallel)
  require(foreach)
    numCores<-(10)
  registerDoParallel(numCores)
  # set number of environments 
  num_env<-18 
  # start the parallel loop 
  outcome_env_1_3_1_par<- foreach(i=1:num_env, .packages = c( "truncnorm", "purrr", "SciViews")) %dopar% {
    # start running single model 
    mod_1_3_1_hpc(days = days, N = N, env_type = i, th_forage_sc1 = th_forage_sc1, th_forage_sc2 = th_forage_sc2, th_forage_sc3= th_forage_sc3, daylight_h = daylight_h)
  }
  
  # clean up cluster 
  stopImplicitCluster()
  
  # The result we have at this point is 'outcome_env_1_3_1_par' 
  # This is a list with the averages fo each behaviour/usrvival/physiology for each timestep per environment 
  # It will go into the halflife function, so we find out at what point in time half of the birds are alive 

  # Create the variable called halflife_input
  halflife_input<-outcome_env_1_3_1_par
  
  # run the t_halflife function 
  t_halflife_func(halflife_input)
  # put the output into a dataframe 
  t_HL_df<-map_dfr(t_HL_list, ~as.data.frame(t(.x)))
  t_HL_df$env<-1:18
  
  # Calculate mean 
  t_HL_mean<-mean(t_HL_df$V1)
  t_HL_SD<-sd(t_HL_df$V1)

  # prepare for output of the env_function 
  performance<<-cbind(t_HL_mean, t_HL_SD)
  colnames(performance)<-c('mean', 'SD')
  output_env_func<<-list(performance, outcome_env_1_3_1_par)

  # RETURN IF NEEDED FOR OPTIIZATION/ MAKING GRAPHS 
  return(output_env_func)

} # end environment function loop 


# The one that runs parallel 
env_func_1_3_2_par<-function(days, N, th_forage_sc1, th_forage_sc2, th_forage_sc3, daylight_h, modelType){
  # For parallel computing 
  require(doParallel)
  require(foreach)
  numCores<-(detectCores()-1)
  registerDoParallel(numCores)
  # set number of environments 
  num_env<-18 
  # start parallel loop 
  outcome_env_1_3_2_par<- foreach(i=1:num_env, .packages = c( "truncnorm", "purrr", "SciViews")) %dopar% {
    # Set source files 
    setwd("C:/Local_R/BiPhD-ABM/May23")
    source('MOD_1_FuncSource.R')
    source('ModelSource.R')
    # Start running single model 
    mod_1_3_2(days = days, N = N, env_type = i, th_forage_sc1 = th_forage_sc1, th_forage_sc2 = th_forage_sc2, th_forage_sc3= th_forage_sc3, daylight_h = daylight_h)
  }
  
  # clean up cluster 
  stopImplicitCluster()
  
  # The result we have at this point is 'outcome_env_1_3_1_par' 
  # This is a list with the averages fo each behaviour/usrvival/physiology for each timestep per environment 
  # It will go into the halflife function, so we find out at what point in time half of the birds are alive 
  
  # load any functions 
  setwd("C:/Local_R/BiPhD-ABM/May23")
  source('MOD_1_FuncSource.R')
  source('ModelSource.R')
  
  # Create the variable called halflife_input
  halflife_input<-outcome_env_1_3_2_par
  
  # run the t_halflife function 
  t_halflife_func(halflife_input)
  # put the output into a dataframe 
  t_HL_df<-map_dfr(t_HL_list, ~as.data.frame(t(.x)))
  t_HL_df$env<-1:18
  
  # Calculate mean 
  t_HL_mean<-mean(t_HL_df$V1)
  t_HL_SD<-sd(t_HL_df$V1)
  
  # prepare for output of the env_function 
  performance<<-cbind(t_HL_mean, t_HL_SD)
  colnames(performance)<-c('mean', 'SD')
  output_env_func<<-list(performance, outcome_env_1_3_2_par)
  
  # # save the data 
  setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/MOD_1_3_2/env_par")
  save(output_env_func, file=paste0(format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_env_func_out_132', 'd', days, 'N', N, 'th1', th_forage_sc1, 'th2', th_forage_sc2, 'th3', th_forage_sc3, 'dayh', daylight_h,  '.Rda'))
  
  # RETURN IF NEEDED FOR OPTIIZATION/ MAKING GRAPHS 
  return(output_env_func)
  
} # end environment function loop 

# the one that runs parallel 
env_func_1_3_2_par_hpc<-function(days, N, th_forage_sc1, th_forage_sc2, th_forage_sc3, daylight_h, modelType){
  
  
  require(doParallel)
  require(foreach)
  
  numCores<-(10)
  registerDoParallel(numCores)
  
  num_env<-18 
  
  outcome_env_1_3_2_par<- foreach(i=1:num_env, .packages = c( "truncnorm", "purrr")) %dopar% {
    
    # setwd("C:/Local_R/BiPhD-ABM/May23")
    # source('MOD_1_FuncSource.R')
    # source('ModelSource.R')
    
    mod_1_3_2_hpc(days = days, N = N, env_type = i, th_forage_sc1 = th_forage_sc1, th_forage_sc2 = th_forage_sc2, th_forage_sc3= th_forage_sc3, daylight_h = daylight_h)
    
    #print('done')
    
    
  }
  
  # clean up cluster 
  stopImplicitCluster()
  
  # The result we have at this point is 'outcome_env_1_3_1_par' 
  # This is a list with the averages fo each behaviour/usrvival/physiology for each timestep per environment 
  # It will go into the halflife function, so we find out at what point in time half of the birds are alive 
  
  # load any functions 
  # setwd("C:/Local_R/BiPhD-ABM/May23")
  # source('MOD_1_FuncSource.R')
  # source('ModelSource.R')
  
  
  # Create the variable called halflife_input
  halflife_input<-outcome_env_1_3_2_par
  
  # run the t_halflife function 
  t_halflife_func(halflife_input)
  # put the output into a dataframe 
  t_HL_df<-map_dfr(t_HL_list, ~as.data.frame(t(.x)))
  t_HL_df$env<-1:18
  
  # Calculate mean 
  t_HL_mean<-mean(t_HL_df$V1)
  t_HL_SD<-sd(t_HL_df$V1)
  
  
  # prepare for output of the env_function 
  performance<<-cbind(t_HL_mean, t_HL_SD)
  colnames(performance)<-c('mean', 'SD')
  output_env_func<<-list(performance, outcome_env_1_3_2_par)
  
  # # # save the data 
  # setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/MOD_1_3_1/env_par")
  # save(output_env_func, file=paste0(format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_env_func_out_131', 'd', days, 'N', N, 'th1', th_forage_sc1, 'th2', th_forage_sc2, 'th3', th_forage_sc3, 'dayh', daylight_h,  '.Rda'))
  # 
  # RETURN IF NEEDED FOR OPTIIZATION/ MAKING GRAPHS 
  return(output_env_func)
  
  
  
} # end environment function loop 

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------  

#################################################################
##     Model 2.1: Non-hoarding bird, Access to Fat-reserves    ##
#################################################################

# model 2.1 

mod_2_1<-function(days, N, env_type, th_forage_fr, daylight_h){
  # Make the variables global 
  days<<-days
  N<<-N
  env_type<<-env_type
  th_forage_fr<<-th_forage_fr
  daylight_h<<-daylight_h
  modelType<<-21
  
  # load necessary packages for parallel (if needed)
  require(foreach)
  require(doParallel)
  # Start the model 
  
  # Start the model 
  # link to the function file 
  setwd("C:/Local_R/BiPhD-ABM/May23")
  source('MOD_1_FuncSource.R')

  # Set up the general environment 
  # This part is the same for each bird 
  set_up_func_general(days, env_type, daylight_h)

  ################################
  #      individual loops        # 
  ################################
  
  # The individual loops need to start now
  # These should be parallelised 
  
  outcome_2_1<-foreach(icount(N), .packages = "truncnorm", .combine='rbind') %do% {
  
    # Do a setup for the individual bird
    # This includes the individual temperature pattern 
    
    # Run hte temperature function 
    # Running this seperately for each individual brings in some desired stochasticity 
    temp_func(TS, Tmax_range_low, Tmax_range_high, Tmin_range_low, Tmin_range_high, days, daylight_h, n_daylight_timestep)
    
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
        
        # Calculate the current fat loss rate 
        flr_func(t,i)
        
        # Set the current temperature 
        temp_cur<<-total_temp_profile[t]
        # Check if it is night or day 
        if ((t%%72)<= n_daylight_timestep){
          dayOrNight<<-1                       # this means it is day 
        } else {
          dayOrNight<<-0                       # this means it is night 
        }
        
        ####################
        #     SLEEPING     #
        ####################
        
        sleep_func(t,i)
        
        if (sleep_count[i,t]==0){
          
          # Only access to stomach-content & forage under threshold 
          if ((mat_fr[i,t]) < th_forage_fr){
            # If this is the case, the bird is hungry and needs to forage for food 
            
            #################
            #     FORAGE    # 
            #################
            
            # At this point, foraging means to go out and find a NEW food item 
            forage_function(t,i)

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
        
        ####################
        #    PILFERAGE     #
        ####################
        # Pilfer, but only if there are actually caches left 
        if(mat_caches[i,t]>0){
          pilf_func(i=i,t=t,lambda=lambda)
        }
        
        ##################################
        #   PREPARE FOR NEXT TIMESTEP   # 
        ##################################
        
        ts_prep_func(t,i, TS)
        
      } # end of loop for alive individuals 

    } # end timestep loop
    
    # Alternatively, I could try to create lists with the output 
    list(eat_count, eat_hoard_count, forage_count, hoard_count, mat_alive, mat_caches, mat_find_food, mat_fr, mat_sc, mat_flr, mat_mass, predation_count, rest_count, retrieve_count, sleep_count, decay_count,mat_temp)
    
  } # end of the foreach loop (individuals) 
  
  # clean up cluster 
  stopImplicitCluster()
  
  #return(outcome_1_1)
  assign(paste0('outcome_2_1_env', env_type),outcome_2_1, envir=.GlobalEnv)
  
  create_df_func(outputFile = outcome_2_1, modelType = '21', env_type= env_type)

} # end of model 2.1 function local 

# model 2.1 for hpc 
mod_2_1_hpc<-function(days, N, env_type, th_forage_fr, daylight_h){
  # Make the variables global 
  days<<-days
  N<<-N
  env_type<<-env_type
  th_forage_fr<<-th_forage_fr
  daylight_h<<-daylight_h
  modelType<<-21
  
  # load necessary packages for parallel (if needed)
  require(foreach)
  require(doParallel)
  # Start the model 
  
  # Set up the general environment 
  # This part is the same for each bird 
  set_up_func_general(days, env_type, daylight_h)
  
  
  ################################
  #      individual loops        # 
  ################################
  
  # The individual loops need to start now
  # These should be parallelised 
  
  outcome_2_1<-foreach(icount(N), .packages = "truncnorm", .combine='rbind') %do% {

    # Do a setup for the individual bird
    # This includes the individual temperature pattern 
    
    # Run hte temperature function 
    # Running this seperately for each individual brings in some desired stochasticity 
    temp_func(TS, Tmax_range_low, Tmax_range_high, Tmin_range_low, Tmin_range_high, days, daylight_h, n_daylight_timestep)
    
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
        
        # Calculate the current fat loss rate 
        flr_func(t,i)
        
        # Set the current temperature 
        temp_cur<<-total_temp_profile[t]
        # Check if it is night or day 
        if ((t%%72)<= n_daylight_timestep){
          dayOrNight<<-1                       # this means it is day 
        } else {
          dayOrNight<<-0                       # this means it is night 
        }
        
        ####################
        #     SLEEPING     #
        ####################
        
        sleep_func(t,i)
        
        if (sleep_count[i,t]==0){

          # Only access to stomach-content & forage under threshold 
          if ((mat_fr[i,t]) < th_forage_fr){
            # If this is the case, the bird is hungry and needs to forage for food 
            
            #################
            #     FORAGE    # 
            #################
            
            # At this point, foraging means to go out and find a NEW food item 
            forage_function(t,i)
            
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
        
        ####################
        #    PILFERAGE     #
        ####################
        # Pilfer, but only if there are actually caches left 
        if(mat_caches[i,t]>0){
          pilf_func(i=i,t=t,lambda=lambda)
        }
        
        ##################################
        #   PREPARE FOR NEXT TIMESTEP   # 
        ##################################
        
        ts_prep_func(t,i, TS)
        
      } # end of loop for alive individuals 

    } # end timestep loop
    
    # Alternatively, I could try to create lists with the output 
    list(eat_count, eat_hoard_count, forage_count, hoard_count, mat_alive, mat_caches, mat_find_food, mat_fr, mat_sc, mat_flr, mat_mass, predation_count, rest_count, retrieve_count, sleep_count,decay_count, mat_temp)
    
  } # end of the foreach loop (individuals) 
  
  # clean up cluster 
  stopImplicitCluster()
  
  #return(outcome_1_1)
  assign(paste0('outcome_2_1_env', env_type),outcome_2_1, envir=.GlobalEnv)
  
  create_df_func(outputFile = outcome_2_1, modelType = '21', env_type= env_type)

} # end of model 2.1 hpc function 


########################
#   ENVIRONMENT LOOP   #
########################

# Parallel version 
env_func_2_1_par<-function(days, N, th_forage_fr, daylight_h, modelType){
  # For parallel computing 
  require(doParallel)
  require(foreach)
  numCores<-(detectCores()-1)
  registerDoParallel(numCores)
  # Set the number of environments 
  num_env<-18 
  # Start the parallel loop 
  outcome_env_2_1_par<- foreach(i=1:num_env, .packages = c( "truncnorm", "purrr", "SciViews")) %dopar% {
    # Set the source files 
    setwd("C:/Local_R/BiPhD-ABM/May23")
    source('MOD_1_FuncSource.R')
    source('ModelSource.R')
    # Start running single model 
    mod_2_1(days = days, N = N, env_type = i, th_forage_fr = th_forage_fr, daylight_h = daylight_h)
  }
  # clean up cluster 
  stopImplicitCluster()

  # The result we have at this point is 'outcome_env_1_1_par' 
  # These will have the same form for each of the models
  # So I will need to write a function that takes care of this and outputs the t_halflife 
  
  # load any functions 
  setwd("C:/Local_R/BiPhD-ABM/May23")
  source('MOD_1_FuncSource.R')
  source('ModelSource.R')
  
  # Create the variable called halflife_input
  halflife_input<-outcome_env_2_1_par
  
  # run the t_halflife function 
  t_halflife_func(halflife_input)
  # put the output into a dataframe 
  t_HL_df<-map_dfr(t_HL_list, ~as.data.frame(t(.x)))
  t_HL_df$env<-1:18
  
  # Calculate mean 
  t_HL_mean<-mean(t_HL_df$V1)
  t_HL_SD<-sd(t_HL_df$V1)
  
  performance<<-cbind(t_HL_mean, t_HL_SD)
  colnames(performance)<-c('mean', 'SD')
  output_env_func<<-list(performance, outcome_env_2_1_par)
  
  # save the data 
  setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/MOD_2_1/env_par")
  save(output_env_func, file=paste0(format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_env_func_out_', modelType, 'd', days, 'N', N,'th_fr', th_forage_fr, 'dayh', daylight_h,   '.Rda'))

  return(output_env_func)

} # end environment function loop 

# Parallel on environment level - for the HPC
env_func_2_1_par_hpc<-function(days, N, th_forage_fr, daylight_h, modelType){
  # For parallel computing 
  require(doParallel)
  require(foreach)
  numCores<-(10)
  registerDoParallel(numCores)
  # Set number of environments 
  num_env<-18 
  # Start parallel loop 
  outcome_env_2_1_par<- foreach(i=1:num_env, .packages = c( "truncnorm", "purrr", "SciViews")) %dopar% {
    # Start running single model
    mod_2_1_hpc(days = days, N = N, env_type = i, th_forage_fr = th_forage_fr, daylight_h = daylight_h)
  }
  
  # clean up cluster 
  stopImplicitCluster()
  
  # Create the variable called halflife_input
  halflife_input<-outcome_env_2_1_par
  
  # run the t_halflife function 
  t_halflife_func(halflife_input)
  # put the output into a dataframe 
  t_HL_df<-map_dfr(t_HL_list, ~as.data.frame(t(.x)))
  t_HL_df$env<-1:18
  
  # Calculate mean 
  t_HL_mean<-mean(t_HL_df$V1)
  t_HL_SD<-sd(t_HL_df$V1)

  performance<<-cbind(t_HL_mean, t_HL_SD)
  colnames(performance)<-c('mean', 'SD')
  output_env_func<<-list(performance, outcome_env_2_1_par)

  return(output_env_func)

} # end environment function loop for hpc 2.1 parallel 


######################################################################
##   Model 2.2: Leftover-hoarding bird, Access to Fat-Reserves      ##
######################################################################

# model 2.2 
mod_2_2<-function(days, N, env_type, th_forage_fr1, th_forage_fr2, daylight_h){
  
  # make some of the variables global for saving 
  days<<-days
  N<<-N
  env_type<<-env_type
  th_forage_fr1<<-th_forage_fr1
  th_forage_fr2<<-th_forage_fr2
  daylight_h<<-daylight_h
  
  # Voor parralel if needed 
  require(foreach)
  require(doParallel)
  # Start the model 
  # link to the function file 
  setwd("C:/Local_R/BiPhD-ABM/May23")
  source('MOD_1_FuncSource.R')

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
    
    # Run hte temperature function 
    # Running this seperately for each individual brings in some desired stochasticity 
    temp_func(TS, Tmax_range_low, Tmax_range_high, Tmin_range_low, Tmin_range_high, days, daylight_h, n_daylight_timestep)
    
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
        
        # Calculate the current fat loss rate 
        flr_func(t,i)
        
        # Set the current temperature 
        temp_cur<<-total_temp_profile[t]
        # Check if it is night or day 
        if ((t%%72)<= n_daylight_timestep){
          dayOrNight<<-1                       # this means it is day 
        } else {
          dayOrNight<<-0                       # this means it is night 
        }
      
        ####################
        #     SLEEPING     #
        ####################
        
        sleep_func(t,i)
        
        if (sleep_count[i,t]==0){
  
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
            
          } else if ((mat_fr[i,t]) >= th_forage_fr2){
            
            ##################
            #    RESTING     # 
            ##################
            
            # If the bird has a SC above the second threshold, it is not hungry at all and can go rest        
            rest_func(t,i)

          } else {
            # If the SC is not lower than Th1 and not higher then TH2, the bird needs to find a new food item 
            
            #################
            #     FORAGE    # 
            #################
            
            # At this point, foraging means to go out and find a NEW food item 
            forage_function(t,i)

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
        
        ####################
        #    PILFERAGE     #
        ####################
        # Pilfer, but only if there are actually caches left 
        if(mat_caches[i,t]>0){
          pilf_func(i=i,t=t,lambda=lambda)
        }
        
        ##################################
        #   PREPARE FOR NEXT TIMESTEP   # 
        ##################################
        
        ts_prep_func(t,i, TS)
        
      } # end of loop for alive individuals 

    } # end timestep loop
    
    
    # Alternatively, I could try to create lists with the output 
    list(eat_count, eat_hoard_count, forage_count, hoard_count, mat_alive, mat_caches, mat_find_food, mat_fr, mat_sc, mat_flr, mat_mass,  predation_count, rest_count, retrieve_count, sleep_count, decay_count,mat_temp)
    
  } # end of the foreach loop (individuals) 
  
  # clean up cluster 
  stopImplicitCluster()
  
  # assign the dataframe and return it to global environment
  assign(paste0('outcome_2_2_env', env_type),outcome_2_2, envir=.GlobalEnv)
  
  create_df_func(outputFile = outcome_2_2, modelType = '22', env_type= env_type)

} # end of model 2.2 function - local 

# model 2.2 for HPC
mod_2_2_hpc<-function(days, N, env_type, th_forage_fr1, th_forage_fr2, daylight_h){
  
  # make some of the variables global for saving 
  days<<-days
  N<<-N
  env_type<<-env_type
  th_forage_fr1<<-th_forage_fr1
  th_forage_fr2<<-th_forage_fr2
  daylight_h<<-daylight_h
  
  # Voor parralel if needed 
  require(foreach)
  require(doParallel)

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
    
    # Run hte temperature function 
    # Running this seperately for each individual brings in some desired stochasticity 
    temp_func(TS, Tmax_range_low, Tmax_range_high, Tmin_range_low, Tmin_range_high, days, daylight_h, n_daylight_timestep)
    
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
        
        # Calculate the current fat loss rate 
        flr_func(t,i)

        # Set the current temperature 
        temp_cur<<-total_temp_profile[t]
        # Check if it is night or day 
        if ((t%%72)<= n_daylight_timestep){
          dayOrNight<<-1                       # this means it is day 
        } else {
          dayOrNight<<-0                       # this means it is night 
        }

        ####################
        #     SLEEPING     #
        ####################
        
        sleep_func(t,i)
        
        if (sleep_count[i,t]==0){

          # Only access to stomach-content, birds need to retrieve if the stomach content is below the lowest threshold 
          # There also needs to be a minimum number of caches available 
          if ((mat_fr[i,t]<=th_forage_fr1) && (mat_caches[i,t]>retrieve_min)){
            
            ####################
            ##   RETRIEVING   ## 
            #################### 
            
            # The bird will retrieve food items 
            retrieve_func(t,i)
            # End of retrieving statement 

          } else if ((mat_fr[i,t]) >= th_forage_fr2){
            
            ##################
            #    RESTING     # 
            ##################
            
            # If the bird has a SC above the second threshold, it is not hungry at all and can go rest        
            rest_func(t,i)

          } else {
            # If the SC is not lower than Th1 and not higher then TH2, the bird needs to find a new food item 
            
            #################
            #     FORAGE    # 
            #################
            
            # At this point, foraging means to go out and find a NEW food item 
            forage_function(t,i)

            # In model 2.2 the bird will eat and then hoard the leftovers 
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
        
        ####################
        #    PILFERAGE     #
        ####################
        # Pilfer, but only if there are actually caches left 
        if(mat_caches[i,t]>0){
          pilf_func(i=i,t=t,lambda=lambda)
        }
        
        ##################################
        #   PREPARE FOR NEXT TIMESTEP   # 
        ##################################
        
        ts_prep_func(t,i, TS)
        
      } # end of loop for alive individuals 

    } # end timestep loop

    # Alternatively, I could try to create lists with the output 
    list(eat_count, eat_hoard_count, forage_count, hoard_count, mat_alive, mat_caches, mat_find_food, mat_fr, mat_sc, mat_flr, mat_mass,  predation_count, rest_count, retrieve_count, sleep_count, decay_count,mat_temp)
    
  } # end of the foreach loop (individuals) 
  
  # clean up cluster 
  stopImplicitCluster()
  
  # assign the dataframe and return it to global environment
  assign(paste0('outcome_2_2_env', env_type),outcome_2_2, envir=.GlobalEnv)
  
  create_df_func(outputFile = outcome_2_2, modelType = '22', env_type= env_type)
  
} # end of model 2.2 function for HPC 

########################
#   ENVIRONMENT LOOP   #
########################

# environment loop paralelel 
env_func_2_2_par<-function(days, N, th_forage_fr1, th_forage_fr2, daylight_h, modelType){
  # For parallel computing 
  require(doParallel)
  require(foreach)
  numCores<-(detectCores()-1)
  registerDoParallel(numCores)
  # Set number of environments 
  num_env<-18 
  # start parallel loop 
  outcome_env_2_2_par<- foreach(i=1:num_env, .packages = c( "truncnorm", "purrr", "SciViews")) %dopar% {
    # Sourcefile 
    setwd("C:/Local_R/BiPhD-ABM/May23")
    source('MOD_1_FuncSource.R')
    source('ModelSource.R')
    # Run single model 
    mod_2_2(days = days, N = N, env_type = i, th_forage_fr1 = th_forage_fr1, th_forage_fr2 = th_forage_fr2, daylight_h = daylight_h)
  }
  
  # clean up cluster 
  stopImplicitCluster()
  
  # The result we have at this point is 'outcome_env_2_2_par' 
  # This is a list with the averages fo each behaviour/usrvival/physiology for each timestep per environment 
  # It will go into the halflife function, so we find out at what point in time half of the birds are alive 
  
  # load any functions 
  setwd("C:/Local_R/BiPhD-ABM/May23")
  source('MOD_1_FuncSource.R')
  source('ModelSource.R')

  # Create the variable called halflife_input
  halflife_input<-outcome_env_2_2_par
  
  # run the t_halflife function 
  t_halflife_func(halflife_input)
  # put the output into a dataframe 
  t_HL_df<-map_dfr(t_HL_list, ~as.data.frame(t(.x)))
  t_HL_df$env<-1:18
  
  # Calculate mean 
  t_HL_mean<-mean(t_HL_df$V1)
  t_HL_SD<-sd(t_HL_df$V1)

  # prepare for output of the env_function 
  performance<<-cbind(t_HL_mean, t_HL_SD)
  colnames(performance)<-c('mean', 'SD')
  output_env_func<<-list(performance, outcome_env_2_2_par)
  
  # # save the data 
  setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/MOD_2_2/env_par")
  save(output_env_func, file=paste0(format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_env_func_out_', 'd', days, 'N', N, 'th1', th_forage_fr1, 'th2', th_forage_fr2, 'dayh', daylight_h,  '.Rda'))
  
  return(output_env_func)

} # end environment function loop 

# environment loop paralelel at environment level - For the HPC
env_func_2_2_par_hpc<-function(days, N, th_forage_fr1, th_forage_fr2, daylight_h, modelType){
  # parallel computing 
  require(doParallel)
  require(foreach)
  numCores<-(10)
  registerDoParallel(numCores)
  # Set number of environments 
  num_env<-18 
  # start parallel lop 
  outcome_env_2_2_par<- foreach(i=1:num_env, .packages = c( "truncnorm", "purrr", "SciViews")) %dopar% {
    # Start single run 
    mod_2_2_hpc(days = days, N = N, env_type = i, th_forage_fr1 = th_forage_fr1, th_forage_fr2 = th_forage_fr2, daylight_h = daylight_h)
  }
  
  # clean up cluster 
  stopImplicitCluster()
  
  # Create the variable called halflife_input
  halflife_input<-outcome_env_2_2_par
  
  # run the t_halflife function 
  t_halflife_func(halflife_input)
  # put the output into a dataframe 
  t_HL_df<-map_dfr(t_HL_list, ~as.data.frame(t(.x)))
  t_HL_df$env<-1:18
  
  # Calculate mean 
  t_HL_mean<-mean(t_HL_df$V1)
  t_HL_SD<-sd(t_HL_df$V1)

  # prepare for output of the env_function 
  performance<<-cbind(t_HL_mean, t_HL_SD)
  colnames(performance)<-c('mean', 'SD')
  output_env_func<<-list(performance, outcome_env_2_2_par)
  
  return(output_env_func)
  
} # end environment function loop 2.2 hpc environmetn loop

######################################################################
##     Model 2.3: direct-hoarding bird, Access to Fat-reserves      ##
######################################################################

# model 2.3.1 
mod_2_3_1<-function(days, N, env_type, th_forage_fr1, th_forage_fr2, th_forage_fr3, daylight_h){
  
  # make some variables global 
  days<<-days
  N<<-N
  env_type<<-env_type
  th_forage_fr1<<-th_forage_fr1
  th_forage_fr2<<-th_forage_fr2
  th_forage_fr3<<-th_forage_fr3
  daylight_h<<-daylight_h
  
  require(doParallel)
  require(foreach)
  # Start the model 
  # link to the function file 
  setwd("C:/Local_R/BiPhD-ABM/May23")
  source('MOD_1_FuncSource.R')
  
  # Set up the general environment 
  # This part is the same for each bird 
  set_up_func_general(days, env_type, daylight_h)
  
  ################################
  #      individual loops        # 
  ################################
  
  # The individual loops need to start now
  # These should be parallelised 
  
  outcome_2_3_1<- foreach(icount(N), .packages = "truncnorm", .combine='rbind') %do% {
    
    # Do a setup for the individual bird
    # This includes the individual temperature pattern 
    
    # Run hte temperature function 
    # Running this seperately for each individual brings in some desired stochasticity 
    temp_func(TS, Tmax_range_low, Tmax_range_high, Tmin_range_low, Tmin_range_high, days, daylight_h, n_daylight_timestep)
    
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
        
        # Calculate the current fat loss rate 
        flr_func(t,i)
        
        # Set the current temperature 
        temp_cur<<-total_temp_profile[t]
        # Check if it is night or day 
        if ((t%%72)<= n_daylight_timestep){
          dayOrNight<<-1                       # this means it is day 
        } else {
          dayOrNight<<-0                       # this means it is night 
        }
        
        ####################
        #     SLEEPING     #
        ####################
        
        sleep_func(t,i)
        
        if (sleep_count[i,t]==0){

          if ((mat_fr[i,t])> th_forage_fr3){
            # This is above the third threshold, so the bird will do its direct hoarding, without eating at all 
            
            ############################
            #  FORAGE + HOARD DIRECT   #
            ############################

            # At this point, foraging means to go out and find a NEW food item 
            forage_function(t,i)
            
            # Then directly just hoard it
            dir_hoard_func(t,i)
            
            
          } else if (((mat_fr[i,t])> th_forage_fr2)){
            # The bird will be resting 
            
            ##################
            #    RESTING     # 
            ##################
            
            # If the bird has a SC above the second threshold, it is not hungry at all and can go rest        
            rest_func(t,i)
            
          } else if (((mat_fr[i,t])<=th_forage_fr1) && ((mat_caches[i,t])>retrieve_min)){
            
 
            ####################
            ##   RETRIEVING   ## 
            #################### 
            
            # The bird will retrieve food items 
            retrieve_func(t,i)
            # End of retrieving statement 

          }else {
            
            # The the stomach content is larger than TH1 but smaller or equal to TH2, the bird needs to go out to forage and eat 
            # Note that the birds that tried to retrieve (low stomach content) but did not have caches are in here 
            
            ######################
            #     FORAGE  + EAT  # 
            ######################
            # if (mat_fr[i,t]<=th_forage_fr1){
            #   #print('bird tried to retrieve but went to forage and eat ')
            # }
            
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
        
        ####################
        #    PILFERAGE     #
        ####################
        # Pilfer, but only if there are actually caches left 
        if(mat_caches[i,t]>0){
          pilf_func(i=i,t=t,lambda=lambda)
        }
        
        ##################################
        #   PREPARE FOR NEXT TIMESTEP   # 
        ##################################
        
        ts_prep_func(t,i, TS)
        
      } # end of loop for alive individuals 

    } # end timestep loop
    
    
    # Alternatively, I could try to create lists with the output 
    list(eat_count, eat_hoard_count, forage_count, hoard_count, mat_alive, mat_caches, mat_find_food, mat_fr, mat_sc,mat_flr, mat_mass,  predation_count, rest_count, retrieve_count, sleep_count, decay_count,mat_temp)
    
  } # end of the foreach loop (individuals) 
  
  # clean up cluster 
  stopImplicitCluster()
  
  #return(outcome_1_1)
  assign(paste0('outcome_2_3_1_env', env_type),outcome_2_3_1, envir=.GlobalEnv)
  
  create_df_func(outputFile = outcome_2_3_1, modelType = '231', env_type= env_type)
  
} # end of model 2.3.1 function - local 

# model 2.3.1 
mod_2_3_1_hpc<-function(days, N, env_type, th_forage_fr1, th_forage_fr2, th_forage_fr3, daylight_h){
  
  # make some variables global 
  days<<-days
  N<<-N
  env_type<<-env_type
  th_forage_fr1<<-th_forage_fr1
  th_forage_fr2<<-th_forage_fr2
  th_forage_fr3<<-th_forage_fr3
  daylight_h<<-daylight_h
  
  require(doParallel)
  require(foreach)
  
  # Set up the general environment 
  # This part is the same for each bird 
  set_up_func_general(days, env_type, daylight_h)
  
  ################################
  #      individual loops        # 
  ################################
  
  # The individual loops need to start now
  # These should be parallelised 
  
  outcome_2_3_1<- foreach(icount(N), .packages = "truncnorm", .combine='rbind') %do% {
    
    # Do a setup for the individual bird
    # This includes the individual temperature pattern 
    
    # Run hte temperature function 
    # Running this seperately for each individual brings in some desired stochasticity 
    temp_func(TS, Tmax_range_low, Tmax_range_high, Tmin_range_low, Tmin_range_high, days, daylight_h, n_daylight_timestep)
    
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
        
        # Calculate the current fat loss rate 
        flr_func(t,i)
        
        # Set the current temperature 
        temp_cur<<-total_temp_profile[t]
        # Check if it is night or day 
        if ((t%%72)<= n_daylight_timestep){
          dayOrNight<<-1                       # this means it is day 
        } else {
          dayOrNight<<-0                       # this means it is night 
        }
        
        ####################
        #     SLEEPING     #
        ####################
        
        sleep_func(t,i)
        
        if (sleep_count[i,t]==0){

          if ((mat_fr[i,t])> th_forage_fr3){
            # This is above the third threshold, so the bird will do its direct hoarding, without eating at all 
            
            ############################
            #  FORAGE + HOARD DIRECT   #
            ############################
            
            # At this point, foraging means to go out and find a NEW food item 
            forage_function(t,i)
            
            # Then directly just hoard it
            dir_hoard_func(t,i)
            
          } else if (((mat_fr[i,t])> th_forage_fr2)){
            # The bird will be resting 
            
            ##################
            #    RESTING     # 
            ##################
           
            # If the bird has a SC above the second threshold, it is not hungry at all and can go rest        
            rest_func(t,i)
            
          } else if (((mat_fr[i,t])<=th_forage_fr1) && ((mat_caches[i,t])>retrieve_min)){
            
            ####################
            ##   RETRIEVING   ## 
            #################### 
            
            # The bird will retrieve food items 
            retrieve_func(t,i)
            # End of retrieving statement 

          }else {
            
            # The the stomach content is larger than TH1 but smaller or equal to TH2, the bird needs to go out to forage and eat 
            # Note that the birds that tried to retrieve (low stomach content) but did not have caches are in here 
            
            ######################
            #     FORAGE  + EAT  # 
            ######################
            if (mat_fr[i,t]<=th_forage_fr1){
              #print('bird tried to retrieve but went to forage and eat ')
            }

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
        
        ####################
        #    PILFERAGE     #
        ####################
        # Pilfer, but only if there are actually caches left 
        if(mat_caches[i,t]>0){
          pilf_func(i=i,t=t,lambda=lambda)
        }
        
        ##################################
        #   PREPARE FOR NEXT TIMESTEP   # 
        ##################################
        
        ts_prep_func(t,i, TS)
        
      } # end of loop for alive individuals 
      
    } # end timestep loop
    
    # Alternatively, I could try to create lists with the output 
    list(eat_count, eat_hoard_count, forage_count, hoard_count, mat_alive, mat_caches, mat_find_food, mat_fr, mat_sc,mat_flr, mat_mass,  predation_count, rest_count, retrieve_count, sleep_count, decay_count,mat_temp)
    
  } # end of the foreach loop (individuals) 
  
  
  # clean up cluster 
  stopImplicitCluster()
  
  #return(outcome_2_3_1)
  assign(paste0('outcome_2_3_1_env', env_type),outcome_2_3_1, envir=.GlobalEnv)
  
  create_df_func(outputFile = outcome_2_3_1, modelType = '231', env_type= env_type)
  
} # end of model 2.3.1 function - hpc 


# model 2.3.2 
mod_2_3_2<-function(days, N, env_type, th_forage_fr1, th_forage_fr2, th_forage_fr3, daylight_h){
  # make some variables global 
  days<<-days
  N<<-N
  env_type<<-env_type
  th_forage_fr1<<-th_forage_fr1
  th_forage_fr2<<-th_forage_fr2
  th_forage_fr3<<-th_forage_fr3
  daylight_h<<-daylight_h
  
  # load packages if needed 
  require(doParallel)
  require(foreach)
  # Start the model 
  
  # link to the function file 
  setwd("C:/Local_R/BiPhD-ABM/May23")
  source('MOD_1_FuncSource.R')
  
  # Set up the general environment 
  # This part is the same for each bird 
  set_up_func_general(days, env_type, daylight_h)
  
  ################################
  #      individual loops        # 
  ################################
  
  # The individual loops need to start now
  # These should be parallelised 
  
  outcome_2_3_2<- foreach(icount(N), .packages = "truncnorm", .combine='rbind') %do% {
    
    # Do a setup for the individual bird
    # This includes the individual temperature pattern 
    
    # Run hte temperature function 
    # Running this seperately for each individual brings in some desired stochasticity 
    temp_func(TS, Tmax_range_low, Tmax_range_high, Tmin_range_low, Tmin_range_high, days, daylight_h, n_daylight_timestep)
    
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
        
        # Calculate the current fat loss rate 
        flr_func(t,i)
      
        # Set the current temperature 
        temp_cur<<-total_temp_profile[t]
        # Check if it is night or day 
        if ((t%%72)<= n_daylight_timestep){
          dayOrNight<<-1                       # this means it is day 
        } else {
          dayOrNight<<-0                       # this means it is night 
        }
        
        ####################
        #     SLEEPING     #
        ####################
        
        sleep_func(t,i)
        
        if (sleep_count[i,t]==0){
          
          if ((mat_fr[i,t])> th_forage_fr3){
            # This is above the third threshold, so the bird will do its direct hoarding, without eating at all 
            
            # The bird will be resting 
            
            ##################
            #    RESTING     # 
            ##################
            
            # If the bird has a SC above the second threshold, it is not hungry at all and can go rest        
            rest_func(t,i)
            
          } else if (((mat_fr[i,t])> th_forage_fr2)){
            
            ############################
            #  FORAGE + HOARD DIRECT   #
            ############################

            # At this point, foraging means to go out and find a NEW food item 
            forage_function(t,i)
            
            # Then directly just hoard it
            dir_hoard_func(t,i)
    
          } else if (((mat_fr[i,t])<=th_forage_fr1) && ((mat_caches[i,t])>retrieve_min)){
            
            ####################
            ##   RETRIEVING   ## 
            #################### 
            
            # The bird will retrieve food items 
            retrieve_func(t,i)
            # End of retrieving statement 

          }else {
            
            # The the stomach content is larger than TH1 but smaller or equal to TH2, the bird needs to go out to forage and eat 
            # Note that the birds that tried to retrieve (low stomach content) but did not have caches are in here 
            
            ######################
            #     FORAGE  + EAT  # 
            ######################
            
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
        
        ####################
        #    PILFERAGE     #
        ####################
        # Pilfer, but only if there are actually caches left 
        if(mat_caches[i,t]>0){
          pilf_func(i=i,t=t,lambda=lambda)
        }
        
        ##################################
        #   PREPARE FOR NEXT TIMESTEP   # 
        ##################################
        
        ts_prep_func(t,i, TS)
        
      } # end of loop for alive individuals 
      
    } # end timestep loop

    # Alternatively, I could try to create lists with the output 
    list(eat_count, eat_hoard_count, forage_count, hoard_count, mat_alive, mat_caches, mat_find_food, mat_fr, mat_sc, mat_flr,mat_mass,  predation_count, rest_count, retrieve_count, sleep_count,decay_count, mat_temp)
    
  } # end of the foreach loop (individuals) 
  
  
  # clean up cluster 
  stopImplicitCluster()
  
  #return(outcome_1_1)
  assign(paste0('outcome_2_3_2_env', env_type),outcome_2_3_2, envir=.GlobalEnv)
  
  create_df_func(outputFile = outcome_2_3_2, modelType = '232', env_type= env_type)
  
} # end of model 2.3.2 function - Local 

# model 2.3.2 
mod_2_3_2_hpc<-function(days, N, env_type, th_forage_fr1, th_forage_fr2, th_forage_fr3, daylight_h){
  # make some variables global 
  days<<-days
  N<<-N
  env_type<<-env_type
  th_forage_fr1<<-th_forage_fr1
  th_forage_fr2<<-th_forage_fr2
  th_forage_fr3<<-th_forage_fr3
  daylight_h<<-daylight_h
  
  # load packages if needed 
  require(doParallel)
  require(foreach)
  # Start the model 
  
  # Set up the general environment 
  # This part is the same for each bird 
  set_up_func_general(days, env_type, daylight_h)
  
  ################################
  #      individual loops        # 
  ################################
  
  # The individual loops need to start now
  # These should be parallelised 
  
  outcome_2_3_2<- foreach(icount(N), .packages = "truncnorm", .combine='rbind') %do% {
    
    # Do a setup for the individual bird
    # This includes the individual temperature pattern 
    
    # Run hte temperature function 
    # Running this seperately for each individual brings in some desired stochasticity 
    temp_func(TS, Tmax_range_low, Tmax_range_high, Tmin_range_low, Tmin_range_high, days, daylight_h, n_daylight_timestep)
    
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
        
        # Calculate the current fat loss rate 
        flr_func(t,i)

        # Set the current temperature 
        temp_cur<<-total_temp_profile[t]
        # Check if it is night or day 
        if ((t%%72)<= n_daylight_timestep){
          dayOrNight<<-1                       # this means it is day 
        } else {
          dayOrNight<<-0                       # this means it is night 
        }
        
        ####################
        #     SLEEPING     #
        ####################
        
        sleep_func(t,i)
        
        if (sleep_count[i,t]==0){

          if ((mat_fr[i,t])> th_forage_fr3){
            # This is above the third threshold, so the bird will do its direct hoarding, without eating at all 
            
            # The bird will be resting 
            
            ##################
            #    RESTING     # 
            ##################

            # If the bird has a SC above the second threshold, it is not hungry at all and can go rest        
            rest_func(t,i)
            
          } else if (((mat_fr[i,t])> th_forage_fr2)){
            
            ############################
            #  FORAGE + HOARD DIRECT   #
            ############################
            
            # At this point, foraging means to go out and find a NEW food item 
            forage_function(t,i)
            
            # Then directly just hoard it
            dir_hoard_func(t,i)

          } else if (((mat_fr[i,t])<=th_forage_fr1) && ((mat_caches[i,t])>retrieve_min)){
            
            ####################
            ##   RETRIEVING   ## 
            #################### 
            
            # The bird will retrieve food items 
            retrieve_func(t,i)
            # End of retrieving statement 

          }else {
            
            # The the stomach content is larger than TH1 but smaller or equal to TH2, the bird needs to go out to forage and eat 
            # Note that the birds that tried to retrieve (low stomach content) but did not have caches are in here 
            
            ######################
            #     FORAGE  + EAT  # 
            ######################
            
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
        
        ####################
        #    PILFERAGE     #
        ####################
        # Pilfer, but only if there are actually caches left 
        if(mat_caches[i,t]>0){
          pilf_func(i=i,t=t,lambda=lambda)
        }
        
        ##################################
        #   PREPARE FOR NEXT TIMESTEP   # 
        ##################################
        
        ts_prep_func(t,i, TS)
        
      } # end of loop for alive individuals 

    } # end timestep loop

    # Alternatively, I could try to create lists with the output 
    list(eat_count, eat_hoard_count, forage_count, hoard_count, mat_alive, mat_caches, mat_find_food, mat_fr, mat_sc, mat_flr,mat_mass,  predation_count, rest_count, retrieve_count, sleep_count,decay_count, mat_temp)
    
  } # end of the foreach loop (individuals) 
  
  
  # clean up cluster 
  stopImplicitCluster()
  
  #return(outcome_1_1)
  assign(paste0('outcome_2_3_2_env', env_type),outcome_2_3_2, envir=.GlobalEnv)
  
  create_df_func(outputFile = outcome_2_3_2, modelType = '232', env_type= env_type)

} # end of model 2.3.2 function for HPC 

########################
#   ENVIRONMENT LOOP   #
########################

# the one that runs parallel 
env_func_2_3_1_par<-function(days, N, th_forage_fr1, th_forage_fr2, th_forage_fr3, daylight_h, modelType){
  # For parallel computing 
  require(doParallel)
  require(foreach)
  numCores<-(detectCores()-1)
  registerDoParallel(numCores)
  # Set number of environments 
  num_env<-18 
  # Start parallel loop 
  outcome_env_2_3_1_par<- foreach(i=1:num_env, .packages = c( "truncnorm", "purrr", "SciViews")) %dopar% {
    # Set source files 
    setwd("C:/Local_R/BiPhD-ABM/May23")
    source('MOD_1_FuncSource.R')
    source('ModelSource.R')
    # Start single run 
    mod_2_3_1(days = days, N = N, env_type = i, th_forage_fr1 = th_forage_fr1, th_forage_fr2 = th_forage_fr2, th_forage_fr3= th_forage_fr3, daylight_h = daylight_h)
  }
  
  # clean up cluster 
  stopImplicitCluster()
  
  # The result we have at this point is 'outcome_env_2_3_1_par' 
  # This is a list with the averages fo each behaviour/usrvival/physiology for each timestep per environment 
  # It will go into the halflife function, so we find out at what point in time half of the birds are alive 
  
  # load any functions 
  setwd("C:/Local_R/BiPhD-ABM/May23")
  source('MOD_1_FuncSource.R')
  source('ModelSource.R')

  # Create the variable called halflife_input
  halflife_input<-outcome_env_2_3_1_par
  
  # run the t_halflife function 
  t_halflife_func(halflife_input)
  # put the output into a dataframe 
  t_HL_df<-map_dfr(t_HL_list, ~as.data.frame(t(.x)))
  t_HL_df$env<-1:18
  
  # Calculate mean 
  t_HL_mean<-mean(t_HL_df$V1)
  t_HL_SD<-sd(t_HL_df$V1)

  # prepare for output of the env_function 
  performance<<-cbind(t_HL_mean, t_HL_SD)
  colnames(performance)<-c('mean', 'SD')
  output_env_func<<-list(performance, outcome_env_2_3_1_par)
  
  # # save the data 
  setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/MOD_2_3_1/env_par")
  save(output_env_func, file=paste0(format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_env_func_out_231', 'd', days, 'N', N, 'th1', th_forage_fr1, 'th2', th_forage_fr2, 'th3', th_forage_fr3, 'dayh', daylight_h,  '.Rda'))
  
  # RETURN IF NEEDED FOR OPTIIZATION/ MAKING GRAPHS 
  return(output_env_func)

} # end environment function loop 

# the one that runs parallel op HPC
env_func_2_3_1_par_hpc<-function(days, N, th_forage_fr1, th_forage_fr2, th_forage_fr3, daylight_h, modelType){
  # parallel computing
  require(doParallel)
  require(foreach)
  numCores<-(10)
  registerDoParallel(numCores)
  # Set number of environments 
  num_env<-18 
  # Start parallel run 
  outcome_env_2_3_1_par<- foreach(i=1:num_env, .packages = c( "truncnorm", "purrr", "SciViews")) %dopar% {
    # Start single model run 
    mod_2_3_1_hpc(days = days, N = N, env_type = i, th_forage_fr1 = th_forage_fr1, th_forage_fr2 = th_forage_fr2, th_forage_fr3= th_forage_fr3, daylight_h = daylight_h)
  }
  
  # clean up cluster 
  stopImplicitCluster()
  
  # Create the variable called halflife_input
  halflife_input<-outcome_env_2_3_1_par
  
  # run the t_halflife function 
  t_halflife_func(halflife_input)
  # put the output into a dataframe 
  t_HL_df<-map_dfr(t_HL_list, ~as.data.frame(t(.x)))
  t_HL_df$env<-1:18
  
  # Calculate mean 
  t_HL_mean<-mean(t_HL_df$V1)
  t_HL_SD<-sd(t_HL_df$V1)

  # prepare for output of the env_function 
  performance<<-cbind(t_HL_mean, t_HL_SD)
  colnames(performance)<-c('mean', 'SD')
  output_env_func<<-list(performance, outcome_env_2_3_1_par)

  # RETURN IF NEEDED FOR OPTIIZATION/ MAKING GRAPHS 
  return(output_env_func)

} # end environment function loop 

# The one that runs parallel 
env_func_2_3_2_par<-function(days, N, th_forage_fr1, th_forage_fr2, th_forage_fr3, daylight_h, modelType){
  # For parallel computing 
  require(doParallel)
  require(foreach)
  numCores<-(detectCores()-1)
  registerDoParallel(numCores)
  # set number of environments 
  num_env<-18 
  # Start parallel loop 
  outcome_env_2_3_2_par<- foreach(i=1:num_env, .packages = c( "truncnorm", "purrr", "SciViews")) %dopar% {
    # Set source files 
    setwd("C:/Local_R/BiPhD-ABM/May23")
    source('MOD_1_FuncSource.R')
    source('ModelSource.R')
    # Start single model run 
    mod_2_3_2(days = days, N = N, env_type = i, th_forage_fr1 = th_forage_fr1, th_forage_fr2 = th_forage_fr2, th_forage_fr3= th_forage_fr3, daylight_h = daylight_h)
  }
  
  # clean up cluster 
  stopImplicitCluster()
  
  # The result we have at this point is 'outcome_env_2_3_1_par' 
  # This is a list with the averages fo each behaviour/usrvival/physiology for each timestep per environment 
  # It will go into the halflife function, so we find out at what point in time half of the birds are alive 
  
  # load any functions 
  setwd("C:/Local_R/BiPhD-ABM/May23")
  source('MOD_1_FuncSource.R')
  source('ModelSource.R')

  # Create the variable called halflife_input
  halflife_input<-outcome_env_2_3_2_par
  
  # run the t_halflife function 
  t_halflife_func(halflife_input)
  # put the output into a dataframe 
  t_HL_df<-map_dfr(t_HL_list, ~as.data.frame(t(.x)))
  t_HL_df$env<-1:18
  
  # Calculate mean 
  t_HL_mean<-mean(t_HL_df$V1)
  t_HL_SD<-sd(t_HL_df$V1)

  # prepare for output of the env_function 
  performance<<-cbind(t_HL_mean, t_HL_SD)
  colnames(performance)<-c('mean', 'SD')
  output_env_func<<-list(performance, outcome_env_2_3_2_par)
  
  # # save the data 
  setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/MOD_2_3_2/env_par")
  save(output_env_func, file=paste0(format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_env_func_out_232', 'd', days, 'N', N, 'th1', th_forage_fr1, 'th2', th_forage_fr2, 'th3', th_forage_fr3, 'dayh', daylight_h,  '.Rda'))
  
  # RETURN IF NEEDED FOR OPTIIZATION/ MAKING GRAPHS 
  return(output_env_func)
} # end environment function loop 

# the one that runs parallel on the HPC
env_func_2_3_2_par_hpc<-function(days, N, th_forage_fr1, th_forage_fr2, th_forage_fr3, daylight_h, modelType){
  # For parallel computing 
  require(doParallel)
  require(foreach)
  numCores<-(10)
  registerDoParallel(numCores)
  # Set number of environments 
  num_env<-18 
  # Start parallel loop 
  outcome_env_2_3_2_par<- foreach(i=1:num_env, .packages = c( "truncnorm", "purrr", "SciViews")) %dopar% {
      # Start single model run 
      mod_2_3_2_hpc(days = days, N = N, env_type = i, th_forage_fr1 = th_forage_fr1, th_forage_fr2 = th_forage_fr2, th_forage_fr3= th_forage_fr3, daylight_h = daylight_h)
  }
  
  # clean up cluster 
  stopImplicitCluster()
  
  # Create the variable called halflife_input
  halflife_input<-outcome_env_2_3_2_par
  
  # run the t_halflife function 
  t_halflife_func(halflife_input)
  # put the output into a dataframe 
  t_HL_df<-map_dfr(t_HL_list, ~as.data.frame(t(.x)))
  t_HL_df$env<-1:18
  
  # Calculate mean 
  t_HL_mean<-mean(t_HL_df$V1)
  t_HL_SD<-sd(t_HL_df$V1)
  
  # prepare for output of the env_function 
  performance<<-cbind(t_HL_mean, t_HL_SD)
  colnames(performance)<-c('mean', 'SD')
  output_env_func<<-list(performance, outcome_env_2_3_2_par)
  
  # RETURN IF NEEDED FOR OPTIIZATION/ MAKING GRAPHS 
  return(output_env_func)
  
} # end environment function loop 


#-------------------------------------------------------------------------------------------------------------------------------------------------------

#################################################################
##     Model 3.1: Non-hoarding bird, Access to Fat-loss-rate   ##
#################################################################

# model 3.1 
mod_3_1<-function(days, N, env_type, th_forage_flr, daylight_h){
  # Make the variables global 
  days<<-days
  N<<-N
  env_type<<-env_type
  th_forage_flr<<-th_forage_flr
  daylight_h<<-daylight_h
  
  # load necessary packages for parallel (if needed)
  require(foreach)
  require(doParallel)
  # Start the model 
  
  # Start the model 
  # link to the function file 
  setwd("C:/Local_R/BiPhD-ABM/May23")
  source('MOD_1_FuncSource.R')
  
  # Set up the general environment 
  # This part is the same for each bird 
  set_up_func_general(days, env_type, daylight_h)

  ################################
  #      individual loops        # 
  ################################
  
  # The individual loops need to start now
  # These should be parallelised 
  
  outcome_3_1<-foreach(icount(N), .packages = "truncnorm", .combine='rbind') %do% {
    
    # Do a setup for the individual bird
    # This includes the individual temperature pattern 
    
    # Run hte temperature function 
    # Running this seperately for each individual brings in some desired stochasticity 
    temp_func(TS, Tmax_range_low, Tmax_range_high, Tmin_range_low, Tmin_range_high, days, daylight_h, n_daylight_timestep)
    
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
        
        # Calculate the current fat loss rate 
        flr_func(t,i)
        
        # Set the current temperature 
        temp_cur<<-total_temp_profile[t]
        # Check if it is night or day 
        if ((t%%72)<= n_daylight_timestep){
          dayOrNight<<-1                       # this means it is day 
        } else {
          dayOrNight<<-0                       # this means it is night 
        }

        ####################
        #     SLEEPING     #
        ####################
        
        sleep_func(t,i)
        
        if (sleep_count[i,t]==0){

          # Only access to stomach-content & forage under threshold 
          if ((mat_flr[i,t]) < th_forage_flr){
            # If this is the case, the bird is hungry and needs to forage for food 
            
            #################
            #     FORAGE    # 
            #################
            
            # At this point, foraging means to go out and find a NEW food item 
            forage_function(t,i)

            # This means that we have a non-hoarding bird that forages under the fr-threshold 
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
        
        ####################
        #    PILFERAGE     #
        ####################
        # Pilfer, but only if there are actually caches left 
        if(mat_caches[i,t]>0){
          pilf_func(i=i,t=t,lambda=lambda)
        }
        
        ##################################
        #   PREPARE FOR NEXT TIMESTEP   # 
        ##################################
        
        ts_prep_func(t,i, TS)
        
      } # end of loop for alive individuals 
      
    } # end timestep loop
    
    # Alternatively, I could try to create lists with the output 
    list(eat_count, eat_hoard_count, forage_count, hoard_count, mat_alive, mat_caches, mat_find_food, mat_fr, mat_sc, mat_flr, mat_mass, predation_count, rest_count, retrieve_count, sleep_count,decay_count, mat_temp)
    
  } # end of the foreach loop (individuals) 
  
  # clean up cluster 
  stopImplicitCluster()
  
  #return(outcome_1_1)
  assign(paste0('outcome_3_1_env', env_type),outcome_3_1, envir=.GlobalEnv)
  
  create_df_func(outputFile = outcome_3_1, modelType = '31', env_type= env_type)

} # end of model 3.1 function - LOCAL 

# model 3.1 HPC
mod_3_1_hpc<-function(days, N, env_type, th_forage_flr, daylight_h){
  # Make the variables global 
  days<<-days
  N<<-N
  env_type<<-env_type
  th_forage_flr<<-th_forage_flr
  daylight_h<<-daylight_h
  
  # load necessary packages for parallel (if needed)
  require(foreach)
  require(doParallel)
  # Start the model 
  
  # Set up the general environment 
  # This part is the same for each bird 
  set_up_func_general(days, env_type, daylight_h)
  
  ################################
  #      individual loops        # 
  ################################
  
  # The individual loops need to start now
  # These should be parallelised 
  
  outcome_3_1<-foreach(icount(N), .packages = "truncnorm", .combine='rbind') %do% {
    
    # Do a setup for the individual bird
    # This includes the individual temperature pattern 
    
    # Run hte temperature function 
    # Running this seperately for each individual brings in some desired stochasticity 
    temp_func(TS, Tmax_range_low, Tmax_range_high, Tmin_range_low, Tmin_range_high, days, daylight_h, n_daylight_timestep)
    
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
        
        # Calculate the current fat loss rate 
        flr_func(t,i)
        
        # Set the current temperature 
        temp_cur<<-total_temp_profile[t]
        # Check if it is night or day 
        if ((t%%72)<= n_daylight_timestep){
          dayOrNight<<-1                       # this means it is day 
        } else {
          dayOrNight<<-0                       # this means it is night 
        }
        
        ####################
        #     SLEEPING     #
        ####################
        
        sleep_func(t,i)
        
        if (sleep_count[i,t]==0){

          # Only access to stomach-content & forage under threshold 
          if ((mat_flr[i,t]) < th_forage_flr){
            # If this is the case, the bird is hungry and needs to forage for food 
            
            #################
            #     FORAGE    # 
            #################
            
            # At this point, foraging means to go out and find a NEW food item 
            forage_function(t,i)

            # This means that we have a non-hoarding bird that forages under the fr-threshold 
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
        
        ####################
        #    PILFERAGE     #
        ####################
        # Pilfer, but only if there are actually caches left 
        if(mat_caches[i,t]>0){
          pilf_func(i=i,t=t,lambda=lambda)
        }
        
        ##################################
        #   PREPARE FOR NEXT TIMESTEP   # 
        ##################################
        
        ts_prep_func(t,i, TS)
        
      } # end of loop for alive individuals 
      
    } # end timestep loop
    
    # Alternatively, I could try to create lists with the output 
    list(eat_count, eat_hoard_count, forage_count, hoard_count, mat_alive, mat_caches, mat_find_food, mat_fr, mat_sc, mat_flr, mat_mass, predation_count, rest_count, retrieve_count, sleep_count, decay_count,mat_temp)
    
  } # end of the foreach loop (individuals) 
  
  # clean up cluster 
  stopImplicitCluster()
  
  #return(outcome_1_1)
  assign(paste0('outcome_3_1_env', env_type),outcome_3_1, envir=.GlobalEnv)
  
  create_df_func(outputFile = outcome_3_1, modelType = '31', env_type= env_type)
  
} # end of model 3.1 function  - HPC 

########################
#   ENVIRONMENT LOOP   #
########################

env_func_3_1_par<-function(days, N, th_forage_flr, daylight_h, modelType){
  # for parallel computing 
  require(doParallel)
  require(foreach)
  numCores<-(detectCores()-1)
  registerDoParallel(numCores)
  # Set number of environments 
  num_env<-18 
  # Start parallel loop 
  outcome_env_3_1_par<- foreach(i=1:num_env, .packages = c( "truncnorm", "purrr", "SciViews")) %dopar% {
    # Set source files 
    setwd("C:/Local_R/BiPhD-ABM/May23")
    source('MOD_1_FuncSource.R')
    source('ModelSource.R')
    # Start single model run 
    mod_3_1(days = days, N = N, env_type = i, th_forage_flr = th_forage_flr, daylight_h = daylight_h)
  }
  
  # clean up cluster 
  stopImplicitCluster()

  # The result we have at this point is 'outcome_env_3_1_par' 
  # These will have the same form for each of the models
  # So I will need to write a function that takes care of this and outputs the t_halflife 
  
  # load any functions 
  setwd("C:/Local_R/BiPhD-ABM/May23")
  source('MOD_1_FuncSource.R')
  source('ModelSource.R')
  
  # Create the variable called halflife_input
  halflife_input<-outcome_env_3_1_par
  
  # run the t_halflife function 
  t_halflife_func(halflife_input)
  # put the output into a dataframe 
  t_HL_df<-map_dfr(t_HL_list, ~as.data.frame(t(.x)))
  t_HL_df$env<-1:18
  
  # Calculate mean 
  t_HL_mean<-mean(t_HL_df$V1)
  t_HL_SD<-sd(t_HL_df$V1)

  performance<<-cbind(t_HL_mean, t_HL_SD)
  colnames(performance)<-c('mean', 'SD')
  output_env_func<<-list(performance, outcome_env_3_1_par)
  
  # # save the data 
  setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/MOD_3_1/env_par")
  save(output_env_func, file=paste0(format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_env_func_out_', modelType, 'd', days, 'N', N,'th_flr', th_forage_flr, 'dayh', daylight_h,   '.Rda'))

  return(output_env_func)
} # end environment function loop 3.1 

# paralel environment for hpc 
env_func_3_1_par_hpc<-function(days, N, th_forage_flr, daylight_h, modelType){
  # For parallel computing 
  require(doParallel)
  require(foreach)
  numCores<-(10)
  registerDoParallel(numCores)
  # set number of environments 
  num_env<-18 
  # Start parallel loop 
  outcome_env_3_1_par<- foreach(i=1:num_env, .packages = c( "truncnorm", "purrr", "SciViews")) %dopar% {
    # run single model 
    mod_3_1_hpc(days = days, N = N, env_type = i, th_forage_flr = th_forage_flr, daylight_h = daylight_h)
  }
  # clean up cluster 
  stopImplicitCluster()

  # Create the variable called halflife_input
  halflife_input<-outcome_env_3_1_par
  
  # run the t_halflife function 
  t_halflife_func(halflife_input)
  # put the output into a dataframe 
  t_HL_df<-map_dfr(t_HL_list, ~as.data.frame(t(.x)))
  t_HL_df$env<-1:18
  
  # Calculate mean 
  t_HL_mean<-mean(t_HL_df$V1)
  t_HL_SD<-sd(t_HL_df$V1)

  performance<<-cbind(t_HL_mean, t_HL_SD)
  colnames(performance)<-c('mean', 'SD')
  output_env_func<<-list(performance, outcome_env_3_1_par)

  return(output_env_func)
} # end environment function loop 


######################################################################
##   Model 3.2: Leftover-hoarding bird, Access to Fat-Loss-Rate     ##
######################################################################

# model 3.2 
mod_3_2<-function(days, N, env_type, th_forage_flr1, th_forage_flr2, daylight_h){
  
  # make some of the variables global for saving 
  days<<-days
  N<<-N
  env_type<<-env_type
  th_forage_flr1<<-th_forage_flr1
  th_forage_flr2<<-th_forage_flr2
  daylight_h<<-daylight_h
  
  # Voor parralel if needed 
  require(foreach)
  require(doParallel)
  # Start the model 
  # link to the function file 
  setwd("C:/Local_R/BiPhD-ABM/May23")
  source('MOD_1_FuncSource.R')

  # Set up the general environment 
  # This part is the same for each bird 
  set_up_func_general(days, env_type, daylight_h)
  
  ################################
  #      individual loops        # 
  ################################
  
  # The individual loops need to start now
  # These should be parallelised 
  outcome_3_2<- foreach(icount(N), .packages = "truncnorm", .combine='rbind') %do% {
    
    # Do a setup for the individual bird
    # This includes the individual temperature pattern 
    
    # Run hte temperature function 
    # Running this seperately for each individual brings in some desired stochasticity 
    temp_func(TS, Tmax_range_low, Tmax_range_high, Tmin_range_low, Tmin_range_high, days, daylight_h, n_daylight_timestep)
    
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
        
        # Calculate the current fat loss rate 
        flr_func(t,i)
        
        # Set the current temperature 
        temp_cur<<-total_temp_profile[t]
        # Check if it is night or day 
        if ((t%%72)<= n_daylight_timestep){
          dayOrNight<<-1                       # this means it is day 
        } else {
          dayOrNight<<-0                       # this means it is night 
        }
        
        ####################
        #     SLEEPING     #
        ####################
        
        sleep_func(t,i)
        
        if (sleep_count[i,t]==0){
          
          # Only access to stomach-content, birds need to retrieve if the stomach content is below the lowest threshold 
          # There also needs to be a minimum number of caches available 
          if ((mat_flr[i,t]<=th_forage_flr1) && (mat_caches[i,t]>retrieve_min)){
            
            ####################
            ##   RETRIEVING   ## 
            #################### 
            
            # The bird will retrieve food items 
            retrieve_func(t,i)
            # End of retrieving statement 
 
          } else if ((mat_flr[i,t]) >= th_forage_flr2){
            
            ##################
            #    RESTING     # 
            ##################
            
            # If the bird has a SC above the second threshold, it is not hungry at all and can go rest        
            rest_func(t,i)

          } else {
            # If the SC is not lower than Th1 and not higher then TH2, the bird needs to find a new food item 
            
            #################
            #     FORAGE    # 
            #################
            
            # At this point, foraging means to go out and find a NEW food item 
            forage_function(t,i)
            
            # RULE SPECIFIC TO MODEL 3_2. 
            
            # In model 3.2 the bird will eat and then hoard the leftovers 
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
        
        ####################
        #    PILFERAGE     #
        ####################
        # Pilfer, but only if there are actually caches left 
        if(mat_caches[i,t]>0){
          pilf_func(i=i,t=t,lambda=lambda)
        }
        
        ##################################
        #   PREPARE FOR NEXT TIMESTEP   # 
        ##################################
        
        ts_prep_func(t,i, TS)
        
      } # end of loop for alive individuals 
      
    
    } # end timestep loop
    
    # Alternatively, I could try to create lists with the output 
    list(eat_count, eat_hoard_count, forage_count, hoard_count, mat_alive, mat_caches, mat_find_food, mat_fr, mat_sc, mat_flr, mat_mass,  predation_count, rest_count, retrieve_count, sleep_count,decay_count, mat_temp)
    
  } # end of the foreach loop (individuals) 
  
  # clean up cluster 
  stopImplicitCluster()
  
  # assign the dataframe and return it to global environment
  assign(paste0('outcome_3_2_env', env_type),outcome_3_2, envir=.GlobalEnv)
  
  create_df_func(outputFile = outcome_3_2, modelType = '32', env_type= env_type)

} # end of model 3.2 function - LOCAL

# model 3.2 for HPC
mod_3_2_hpc<-function(days, N, env_type, th_forage_flr1, th_forage_flr2, daylight_h){
  
  # make some of the variables global for saving 
  days<<-days
  N<<-N
  env_type<<-env_type
  th_forage_flr1<<-th_forage_flr1
  th_forage_flr2<<-th_forage_flr2
  daylight_h<<-daylight_h
  
  # Voor parralel if needed 
  require(foreach)
  require(doParallel)
  # Start the model 

  # Set up the general environment 
  # This part is the same for each bird 
  set_up_func_general(days, env_type, daylight_h)
  
  ################################
  #      individual loops        # 
  ################################
  
  # The individual loops need to start now
  # These should be parallelised 
  outcome_3_2<- foreach(icount(N), .packages = "truncnorm", .combine='rbind') %do% {
    
    # Do a setup for the individual bird
    # This includes the individual temperature pattern 
    
    # Run hte temperature function 
    # Running this seperately for each individual brings in some desired stochasticity 
    temp_func(TS, Tmax_range_low, Tmax_range_high, Tmin_range_low, Tmin_range_high, days, daylight_h, n_daylight_timestep)
    
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
        
        # Calculate the current fat loss rate 
        flr_func(t,i)
        
        # Set the current temperature 
        temp_cur<<-total_temp_profile[t]
        # Check if it is night or day 
        if ((t%%72)<= n_daylight_timestep){
          dayOrNight<<-1                       # this means it is day 
        } else {
          dayOrNight<<-0                       # this means it is night 
        }
        
        ####################
        #     SLEEPING     #
        ####################
        
        sleep_func(t,i)
        
        if (sleep_count[i,t]==0){

          # Only access to stomach-content, birds need to retrieve if the stomach content is below the lowest threshold 
          # There also needs to be a minimum number of caches available 
          if ((mat_flr[i,t]<=th_forage_flr1) && (mat_caches[i,t]>retrieve_min)){
            
            ####################
            ##   RETRIEVING   ## 
            #################### 
            
            # The bird will retrieve food items 
            retrieve_func(t,i)
            # End of retrieving statement 

          } else if ((mat_flr[i,t]) >= th_forage_flr2){
            
            ##################
            #    RESTING     # 
            ##################
            
            # If the bird has a SC above the second threshold, it is not hungry at all and can go rest        
            rest_func(t,i)

          } else {
            # If the SC is not lower than Th1 and not higher then TH2, the bird needs to find a new food item 
            
            #################
            #     FORAGE    # 
            #################
            
            # At this point, foraging means to go out and find a NEW food item 
            forage_function(t,i)
            
            # In model 3.2 the bird will eat and then hoard the leftovers 
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
        
        ####################
        #    PILFERAGE     #
        ####################
        # Pilfer, but only if there are actually caches left 
        if(mat_caches[i,t]>0){
          pilf_func(i=i,t=t,lambda=lambda)
        }
        
        ##################################
        #   PREPARE FOR NEXT TIMESTEP   # 
        ##################################
        
        ts_prep_func(t,i, TS)
        
      } # end of loop for alive individuals 
      
      
    } # end timestep loop
    
    # Alternatively, I could try to create lists with the output 
    list(eat_count, eat_hoard_count, forage_count, hoard_count, mat_alive, mat_caches, mat_find_food, mat_fr, mat_sc, mat_flr, mat_mass,  predation_count, rest_count, retrieve_count, sleep_count,decay_count, mat_temp)
    
  } # end of the foreach loop (individuals) 
  
  # clean up cluster 
  stopImplicitCluster()
  
  # assign the dataframe and return it to global environment
  assign(paste0('outcome_3_2_env', env_type),outcome_3_2, envir=.GlobalEnv)
  
  create_df_func(outputFile = outcome_3_2, modelType = '32', env_type= env_type)
  
} # end of model 3.2 function - HPC 

########################
#   ENVIRONMENT LOOP   #
########################

# environment loop paralelel 
env_func_3_2_par<-function(days, N, th_forage_flr1, th_forage_flr2, daylight_h, modelType){
  # For parallel computing 
  require(doParallel)
  require(foreach)
  numCores<-(detectCores()-1)
  registerDoParallel(numCores)
  # Set number of environments 
  num_env<-18 
  # start parallel loop 
  outcome_env_3_2_par<- foreach(i=1:num_env, .packages = c( "truncnorm", "purrr", "SciViews")) %dopar% {
    # Set source files 
    setwd("C:/Local_R/BiPhD-ABM/May23")
    source('MOD_1_FuncSource.R')
    source('ModelSource.R')
    # Start single model 
    mod_3_2(days = days, N = N, env_type = i, th_forage_flr1 = th_forage_flr1, th_forage_flr2 = th_forage_flr2, daylight_h = daylight_h)
  }
  # clean up cluster 
  stopImplicitCluster()
  
  # The result we have at this point is 'outcome_env_3_2_par' 
  # This is a list with the averages fo each behaviour/usrvival/physiology for each timestep per environment 
  # It will go into the halflife function, so we find out at what point in time half of the birds are alive 
  
  # load any functions 
  setwd("C:/Local_R/BiPhD-ABM/May23")
  source('MOD_1_FuncSource.R')
  source('ModelSource.R')

  # Create the variable called halflife_input
  halflife_input<-outcome_env_3_2_par
  
  # run the t_halflife function 
  t_halflife_func(halflife_input)
  # put the output into a dataframe 
  t_HL_df<-map_dfr(t_HL_list, ~as.data.frame(t(.x)))
  t_HL_df$env<-1:18
  
  # Calculate mean 
  t_HL_mean<-mean(t_HL_df$V1)
  t_HL_SD<-sd(t_HL_df$V1)

  # prepare for output of the env_function 
  performance<<-cbind(t_HL_mean, t_HL_SD)
  colnames(performance)<-c('mean', 'SD')
  output_env_func<<-list(performance, outcome_env_3_2_par)
  
  # save the data 
  setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/MOD_3_2/env_par")
  save(output_env_func, file=paste0(format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_env_func_out_', 'd', days, 'N', N, 'th1', th_forage_flr1, 'th2', th_forage_flr2, 'dayh', daylight_h,  '.Rda'))
  
  return(output_env_func)
} # end environment function loop 3.2 - local 

# environment loop paralelel 
env_func_3_2_par_hpc<-function(days, N, th_forage_flr1, th_forage_flr2, daylight_h, modelType){
  # For parallel computing 
  require(doParallel)
  require(foreach)
  numCores<-(10)
  registerDoParallel(numCores)
  # Set number of environments 
  num_env<-18 
  # Start parallel loop 
  outcome_env_3_2_par<- foreach(i=1:num_env, .packages = c( "truncnorm", "purrr", "SciViews")) %dopar% {
    # Single model run 
    mod_3_2_hpc(days = days, N = N, env_type = i, th_forage_flr1 = th_forage_flr1, th_forage_flr2 = th_forage_flr2, daylight_h = daylight_h)
  }
  # clean up cluster 
  stopImplicitCluster()
 
  halflife_input<-outcome_env_3_2_par
  
  # run the t_halflife function 
  t_halflife_func(halflife_input)
  # put the output into a dataframe 
  t_HL_df<-map_dfr(t_HL_list, ~as.data.frame(t(.x)))
  t_HL_df$env<-1:18
  
  # Calculate mean 
  t_HL_mean<-mean(t_HL_df$V1)
  t_HL_SD<-sd(t_HL_df$V1)

  # prepare for output of the env_function 
  performance<<-cbind(t_HL_mean, t_HL_SD)
  colnames(performance)<-c('mean', 'SD')
  output_env_func<<-list(performance, outcome_env_3_2_par)
  
  return(output_env_func)
} # end environment function loop 


######################################################################
##     Model 3.3: direct-hoarding bird, Access to Fat-loss-rates    ##
######################################################################

# model 3.3.1 
mod_3_3_1<-function(days, N, env_type, th_forage_flr1, th_forage_flr2, th_forage_flr3, daylight_h){
  
  # make some variables global 
  days<<-days
  N<<-N
  env_type<<-env_type
  th_forage_flr1<<-th_forage_flr1
  th_forage_flr2<<-th_forage_flr2
  th_forage_flr3<<-th_forage_flr3
  daylight_h<<-daylight_h
  
  require(doParallel)
  require(foreach)
  # Start the model 
  # link to the function file 
  setwd("C:/Local_R/BiPhD-ABM/May23")
  source('MOD_1_FuncSource.R')

  # Set up the general environment 
  # This part is the same for each bird 
  set_up_func_general(days, env_type, daylight_h)
  
  ################################
  #      individual loops        # 
  ################################
  
  # The individual loops need to start now
  # These should be parallelised 
  
  outcome_3_3_1<- foreach(icount(N), .packages = "truncnorm", .combine='rbind') %do% {
    
    # Do a setup for the individual bird
    # This includes the individual temperature pattern 
    
    # Run hte temperature function 
    # Running this seperately for each individual brings in some desired stochasticity 
    temp_func(TS, Tmax_range_low, Tmax_range_high, Tmin_range_low, Tmin_range_high, days, daylight_h, n_daylight_timestep)
    
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
        
        # Calculate the current fat loss rate 
        flr_func(t,i)
        
        # Set the current temperature 
        temp_cur<<-total_temp_profile[t]
        # Check if it is night or day 
        if ((t%%72)<= n_daylight_timestep){
          dayOrNight<<-1                       # this means it is day 
        } else {
          dayOrNight<<-0                       # this means it is night 
        }
        
        ####################
        #     SLEEPING     #
        ####################
        
        sleep_func(t,i)
        
        if (sleep_count[i,t]==0){

          if ((mat_flr[i,t])> th_forage_flr3){
            # This is above the third threshold, so the bird will do its direct hoarding, without eating at all 
            
            ############################
            #  FORAGE + HOARD DIRECT   #
            ############################
            
            # At this point, foraging means to go out and find a NEW food item 
            forage_function(t,i)
            
            # Then directly just hoard it
            dir_hoard_func(t,i)
            
          } else if (((mat_flr[i,t])> th_forage_flr2)){
            # The bird will be resting 
            
            ##################
            #    RESTING     # 
            ##################
          
            # If the bird has a SC above the second threshold, it is not hungry at all and can go rest        
            rest_func(t,i)
            
          } else if (((mat_flr[i,t])<=th_forage_flr1) && ((mat_caches[i,t])>retrieve_min)){
            
            #print('a bird retrieves')
            
            ####################
            ##   RETRIEVING   ## 
            #################### 
            
            # The bird will retrieve food items 
            retrieve_func(t,i)
            # End of retrieving statement 

          }else {
            
            # The the stomach content is larger than TH1 but smaller or equal to TH2, the bird needs to go out to forage and eat 
            # Note that the birds that tried to retrieve (low stomach content) but did not have caches are in here 
            
            ######################
            #     FORAGE  + EAT  # 
            ######################

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
        
        ####################
        #    PILFERAGE     #
        ####################
        # Pilfer, but only if there are actually caches left 
        if(mat_caches[i,t]>0){
          pilf_func(i=i,t=t,lambda=lambda)
        }
        
        ##################################
        #   PREPARE FOR NEXT TIMESTEP   # 
        ##################################
        
        ts_prep_func(t,i, TS)
        
      } # end of loop for alive individuals 

    } # end timestep loop
    
    
    # Alternatively, I could try to create lists with the output 
    list(eat_count, eat_hoard_count, forage_count, hoard_count, mat_alive, mat_caches, mat_find_food, mat_fr, mat_sc,mat_flr, mat_mass,  predation_count, rest_count, retrieve_count, sleep_count, decay_count,mat_temp)
    
  } # end of the foreach loop (individuals) 

  # clean up cluster 
  stopImplicitCluster()
  
  #return(outcome_1_1)
  assign(paste0('outcome_3_3_1_env', env_type),outcome_3_3_1, envir=.GlobalEnv)
  
  create_df_func(outputFile = outcome_3_3_1, modelType = '331', env_type= env_type)
  
} # end of model 3.3.1 function  - LOCAL 

# model 3.3.1 for hpc
mod_3_3_1_hpc<-function(days, N, env_type, th_forage_flr1, th_forage_flr2, th_forage_flr3, daylight_h){
  
  # make some variables global 
  days<<-days
  N<<-N
  env_type<<-env_type
  th_forage_flr1<<-th_forage_flr1
  th_forage_flr2<<-th_forage_flr2
  th_forage_flr3<<-th_forage_flr3
  daylight_h<<-daylight_h
  
  require(doParallel)
  require(foreach)
  # Start the model 
  
  # Set up the general environment 
  # This part is the same for each bird 
  set_up_func_general(days, env_type, daylight_h)
  
  ################################
  #      individual loops        # 
  ################################
  
  # The individual loops need to start now
  # These should be parallelised 
  
  outcome_3_3_1<- foreach(icount(N), .packages = "truncnorm", .combine='rbind') %do% {
    
    # Do a setup for the individual bird
    # This includes the individual temperature pattern 
    
    # Run hte temperature function 
    # Running this seperately for each individual brings in some desired stochasticity 
    temp_func(TS, Tmax_range_low, Tmax_range_high, Tmin_range_low, Tmin_range_high, days, daylight_h, n_daylight_timestep)
    
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
        
        # Calculate the current fat loss rate 
        flr_func(t,i)
        
        # Set the current temperature 
        temp_cur<<-total_temp_profile[t]
        # Check if it is night or day 
        if ((t%%72)<= n_daylight_timestep){
          dayOrNight<<-1                       # this means it is day 
        } else {
          dayOrNight<<-0                       # this means it is night 
        }
        
        ####################
        #     SLEEPING     #
        ####################
        
        sleep_func(t,i)
        
        if (sleep_count[i,t]==0){
 
          if ((mat_flr[i,t])> th_forage_flr3){
            # This is above the third threshold, so the bird will do its direct hoarding, without eating at all 
            
            ############################
            #  FORAGE + HOARD DIRECT   #
            ############################
 
            # At this point, foraging means to go out and find a NEW food item 
            forage_function(t,i)
            
            # Then directly just hoard it
            dir_hoard_func(t,i)
            
            
          } else if (((mat_flr[i,t])> th_forage_flr2)){
            # The bird will be resting 
            
            ##################
            #    RESTING     # 
            ##################
   
            # If the bird has a SC above the second threshold, it is not hungry at all and can go rest        
            rest_func(t,i)
            
          } else if (((mat_flr[i,t])<=th_forage_flr1) && ((mat_caches[i,t])>retrieve_min)){

            
            ####################
            ##   RETRIEVING   ## 
            #################### 
            
            # The bird will retrieve food items 
            retrieve_func(t,i)
            # End of retrieving statement 
 
          }else {
            
            # The the stomach content is larger than TH1 but smaller or equal to TH2, the bird needs to go out to forage and eat 
            # Note that the birds that tried to retrieve (low stomach content) but did not have caches are in here 
            
            ######################
            #     FORAGE  + EAT  # 
            ######################

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
        
        ####################
        #    PILFERAGE     #
        ####################
        # Pilfer, but only if there are actually caches left 
        if(mat_caches[i,t]>0){
          pilf_func(i=i,t=t,lambda=lambda)
        }
        
        ##################################
        #   PREPARE FOR NEXT TIMESTEP   # 
        ##################################
        
        ts_prep_func(t,i, TS)
        
      } # end of loop for alive individuals 

    } # end timestep loop

    # Alternatively, I could try to create lists with the output 
    list(eat_count, eat_hoard_count, forage_count, hoard_count, mat_alive, mat_caches, mat_find_food, mat_fr, mat_sc,mat_flr, mat_mass,  predation_count, rest_count, retrieve_count, sleep_count, decay_count,mat_temp)
    
  } # end of the foreach loop (individuals) 
  
  # clean up cluster 
  stopImplicitCluster()
  
  #return(outcome_1_1)
  assign(paste0('outcome_3_3_1_env', env_type),outcome_3_3_1, envir=.GlobalEnv)
  
  create_df_func(outputFile = outcome_3_3_1, modelType = '331', env_type= env_type)
  
} # end of model 3.3.1 function - HPC 



# model 3.3.2 
mod_3_3_2<-function(days, N, env_type, th_forage_flr1, th_forage_flr2, th_forage_flr3, daylight_h){
  # make some variables global 
  days<<-days
  N<<-N
  env_type<<-env_type
  th_forage_flr1<<-th_forage_flr1
  th_forage_flr2<<-th_forage_flr2
  th_forage_flr3<<-th_forage_flr3
  daylight_h<<-daylight_h
  
  # load packages if needed 
  require(doParallel)
  require(foreach)
  # Start the model 
  
  # link to the function file 
  setwd("C:/Local_R/BiPhD-ABM/May23")
  source('MOD_1_FuncSource.R')
  
  # Set up the general environment 
  # This part is the same for each bird 
  set_up_func_general(days, env_type, daylight_h)
  
  ################################
  #      individual loops        # 
  ################################
  
  # The individual loops need to start now
  # These should be parallelised 
  
  outcome_3_3_2<- foreach(icount(N), .packages = "truncnorm", .combine='rbind') %do% {
    
    # Do a setup for the individual bird
    # This includes the individual temperature pattern 
    
    # Run hte temperature function 
    # Running this seperately for each individual brings in some desired stochasticity 
    temp_func(TS, Tmax_range_low, Tmax_range_high, Tmin_range_low, Tmin_range_high, days, daylight_h, n_daylight_timestep)
    
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
        
        # Calculate the current fat loss rate 
        flr_func(t,i)
        
        # Set the current temperature 
        temp_cur<<-total_temp_profile[t]
        # Check if it is night or day 
        if ((t%%72)<= n_daylight_timestep){
          dayOrNight<<-1                       # this means it is day 
        } else {
          dayOrNight<<-0                       # this means it is night 
        }
        
        ####################
        #     SLEEPING     #
        ####################
        
        sleep_func(t,i)
        
        if (sleep_count[i,t]==0){
          
          # RULE SPECIFIC FOR MODEL 1_3 
          
          if ((mat_flr[i,t])> th_forage_flr3){
            # This is above the third threshold, so the bird will do its direct hoarding, without eating at all 
            
            # The bird will be resting 
            
            ##################
            #    RESTING     # 
            ##################
         
            # If the bird has a SC above the second threshold, it is not hungry at all and can go rest        
            rest_func(t,i)
            
          } else if (((mat_flr[i,t])> th_forage_flr2)){
            
            ############################
            #  FORAGE + HOARD DIRECT   #
            ############################

            # At this point, foraging means to go out and find a NEW food item 
            forage_function(t,i)
            
            # Then directly just hoard it
            dir_hoard_func(t,i)
            
          } else if (((mat_flr[i,t])<=th_forage_flr1) && ((mat_caches[i,t])>retrieve_min)){
            
            ####################
            ##   RETRIEVING   ## 
            #################### 
            
            # The bird will retrieve food items 
            retrieve_func(t,i)
            # End of retrieving statement 

          }else {
            
            # The the stomach content is larger than TH1 but smaller or equal to TH2, the bird needs to go out to forage and eat 
            # Note that the birds that tried to retrieve (low stomach content) but did not have caches are in here 
            
            ######################
            #     FORAGE  + EAT  # 
            ######################
            
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
        
        ####################
        #    PILFERAGE     #
        ####################
        # Pilfer, but only if there are actually caches left 
        if(mat_caches[i,t]>0){
          pilf_func(i=i,t=t,lambda=lambda)
        }
        
        ##################################
        #   PREPARE FOR NEXT TIMESTEP   # 
        ##################################
        
        ts_prep_func(t,i, TS)
        
      } # end of loop for alive individuals 

    } # end timestep loop
    
    # Alternatively, I could try to create lists with the output 
    list(eat_count, eat_hoard_count, forage_count, hoard_count, mat_alive, mat_caches, mat_find_food, mat_fr, mat_sc, mat_flr,mat_mass,  predation_count, rest_count, retrieve_count, sleep_count, decay_count,mat_temp)
    
  } # end of the foreach loop (individuals) 
  
  # clean up cluster 
  stopImplicitCluster()
  
  #return(outcome_1_1)
  assign(paste0('outcome_3_3_2_env', env_type),outcome_3_3_2, envir=.GlobalEnv)
  
  create_df_func(outputFile = outcome_3_3_2, modelType = '332', env_type= env_type)
  
} # end of model 3.3.2 function - LOCAL 

# model 3.3.2 
mod_3_3_2_hpc<-function(days, N, env_type, th_forage_flr1, th_forage_flr2, th_forage_flr3, daylight_h){
  # make some variables global 
  days<<-days
  N<<-N
  env_type<<-env_type
  th_forage_flr1<<-th_forage_flr1
  th_forage_flr2<<-th_forage_flr2
  th_forage_flr3<<-th_forage_flr3
  daylight_h<<-daylight_h
  
  # load packages if needed 
  require(doParallel)
  require(foreach)
  # Start the model 
  
  # Set up the general environment 
  # This part is the same for each bird 
  set_up_func_general(days, env_type, daylight_h)
  
  ################################
  #      individual loops        # 
  ################################
  
  # The individual loops need to start now
  # These should be parallelised 
  
  outcome_3_3_2<- foreach(icount(N), .packages = "truncnorm", .combine='rbind') %do% {
    
    # Do a setup for the individual bird
    # This includes the individual temperature pattern 
    
    # Run hte temperature function 
    # Running this seperately for each individual brings in some desired stochasticity 
    temp_func(TS, Tmax_range_low, Tmax_range_high, Tmin_range_low, Tmin_range_high, days, daylight_h, n_daylight_timestep)
    
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
        
        # Calculate the current fat loss rate 
        flr_func(t,i)

        # Set the current temperature 
        temp_cur<<-total_temp_profile[t]
        # Check if it is night or day 
        if ((t%%72)<= n_daylight_timestep){
          dayOrNight<<-1                       # this means it is day 
        } else {
          dayOrNight<<-0                       # this means it is night 
        }
        
        ####################
        #     SLEEPING     #
        ####################
        
        sleep_func(t,i)
        
        if (sleep_count[i,t]==0){
          
          # RULE SPECIFIC FOR MODEL 1_3 
          
          if ((mat_flr[i,t])> th_forage_flr3){
            # This is above the third threshold, so the bird will do its direct hoarding, without eating at all 
            
            # The bird will be resting 
            
            ##################
            #    RESTING     # 
            ##################
          
            # If the bird has a SC above the second threshold, it is not hungry at all and can go rest        
            rest_func(t,i)
            
          } else if (((mat_flr[i,t])> th_forage_flr2)){
            
            ############################
            #  FORAGE + HOARD DIRECT   #
            ############################

            # At this point, foraging means to go out and find a NEW food item 
            forage_function(t,i)
            
            # Then directly just hoard it
            dir_hoard_func(t,i)
            
          } else if (((mat_flr[i,t])<=th_forage_flr1) && ((mat_caches[i,t])>retrieve_min)){
            
            ####################
            ##   RETRIEVING   ## 
            #################### 
            
            # The bird will retrieve food items 
            retrieve_func(t,i)
            # End of retrieving statement 

          }else {
            
            # The the stomach content is larger than TH1 but smaller or equal to TH2, the bird needs to go out to forage and eat 
            # Note that the birds that tried to retrieve (low stomach content) but did not have caches are in here 
            
            ######################
            #     FORAGE  + EAT  # 
            ######################

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
        
        ####################
        #    PILFERAGE     #
        ####################
        # Pilfer, but only if there are actually caches left 
        if(mat_caches[i,t]>0){
          pilf_func(i=i,t=t,lambda=lambda)
        }
        
        ##################################
        #   PREPARE FOR NEXT TIMESTEP   # 
        ##################################
        
        ts_prep_func(t,i, TS)
        
      } # end of loop for alive individuals 

    } # end timestep loop

    # Alternatively, I could try to create lists with the output 
    list(eat_count, eat_hoard_count, forage_count, hoard_count, mat_alive, mat_caches, mat_find_food, mat_fr, mat_sc, mat_flr,mat_mass,  predation_count, rest_count, retrieve_count, sleep_count, decay_count,mat_temp)
    
  } # end of the foreach loop (individuals) 
  
  # clean up cluster 
  stopImplicitCluster()
  
  #return(outcome_1_1)
  assign(paste0('outcome_3_3_2_env', env_type),outcome_3_3_2, envir=.GlobalEnv)
  
  create_df_func(outputFile = outcome_3_3_2, modelType = '332', env_type= env_type)
  
} # end of model 3.3.2 function - HPC 


########################
#   ENVIRONMENT LOOP   #
########################

# the one that runs parallel 
env_func_3_3_1_par<-function(days, N, th_forage_flr1, th_forage_flr2, th_forage_flr3, daylight_h, modelType){
  # For parallel computing 
  require(doParallel)
  require(foreach)
  numCores<-(detectCores()-1)
  registerDoParallel(numCores)
  # Set number of environments 
  num_env<-18 
  # Start parallel loop 
  outcome_env_3_3_1_par<- foreach(i=1:num_env, .packages = c( "truncnorm", "purrr", "SciViews")) %dopar% {
    # set source files 
    setwd("C:/Local_R/BiPhD-ABM/May23")
    source('MOD_1_FuncSource.R')
    source('ModelSource.R')
    # single model run 
    mod_3_3_1(days = days, N = N, env_type = i, th_forage_flr1 = th_forage_flr1, th_forage_flr2 = th_forage_flr2, th_forage_flr3= th_forage_flr3, daylight_h = daylight_h)
  }
  
  # clean up cluster 
  stopImplicitCluster()
  
  # The result we have at this point is 'outcome_env_3_3_1_par' 
  # This is a list with the averages fo each behaviour/usrvival/physiology for each timestep per environment 
  # It will go into the halflife function, so we find out at what point in time half of the birds are alive 
  
  # load any functions 
  setwd("C:/Local_R/BiPhD-ABM/May23")
  source('MOD_1_FuncSource.R')
  source('ModelSource.R')
 
  # Create the variable called halflife_input
  halflife_input<-outcome_env_3_3_1_par
  
  # run the t_halflife function 
  t_halflife_func(halflife_input)
  # put the output into a dataframe 
  t_HL_df<-map_dfr(t_HL_list, ~as.data.frame(t(.x)))
  t_HL_df$env<-1:18
  
  # Calculate mean 
  t_HL_mean<-mean(t_HL_df$V1)
  t_HL_SD<-sd(t_HL_df$V1)
  
  # prepare for output of the env_function 
  performance<<-cbind(t_HL_mean, t_HL_SD)
  colnames(performance)<-c('mean', 'SD')
  output_env_func<<-list(performance, outcome_env_3_3_1_par)
  
  #  save the data 
  setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/MOD_3_3_1/env_par")
  save(output_env_func, file=paste0(format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_env_func_out_331', 'd', days, 'N', N, 'th1', th_forage_flr1, 'th2', th_forage_flr2, 'th3', th_forage_flr3, 'dayh', daylight_h,  '.Rda'))
  
  # RETURN IF NEEDED FOR OPTIIZATION/ MAKING GRAPHS 
  return(output_env_func)
  
} # end environment function loop for 3.3.1 

# the one that runs parallel 
env_func_3_3_1_par_hpc<-function(days, N, th_forage_flr1, th_forage_flr2, th_forage_flr3, daylight_h, modelType){
  # For parallel computing
  require(doParallel)
  require(foreach)
  numCores<-(10)
  registerDoParallel(numCores)
  # Set number of environments 
  num_env<-18 
  # Start parallel loop 
  outcome_env_3_3_1_par<- foreach(i=1:num_env, .packages = c( "truncnorm", "purrr", "SciViews")) %dopar% {
    # Single model run 
    mod_3_3_1_hpc(days = days, N = N, env_type = i, th_forage_flr1 = th_forage_flr1, th_forage_flr2 = th_forage_flr2, th_forage_flr3= th_forage_flr3, daylight_h = daylight_h)
  }
  
  # clean up cluster 
  stopImplicitCluster()

  # Create the variable called halflife_input
  halflife_input<-outcome_env_3_3_1_par
  
  # run the t_halflife function 
  t_halflife_func(halflife_input)
  # put the output into a dataframe 
  t_HL_df<-map_dfr(t_HL_list, ~as.data.frame(t(.x)))
  t_HL_df$env<-1:18
  
  # Calculate mean 
  t_HL_mean<-mean(t_HL_df$V1)
  t_HL_SD<-sd(t_HL_df$V1)

  # prepare for output of the env_function 
  performance<<-cbind(t_HL_mean, t_HL_SD)
  colnames(performance)<-c('mean', 'SD')
  output_env_func<<-list(performance, outcome_env_3_3_1_par)

  # RETURN IF NEEDED FOR OPTIIZATION/ MAKING GRAPHS 
  return(output_env_func)

} # end environment function loop 

# The one that runs parallel 
env_func_3_3_2_par<-function(days, N, th_forage_flr1, th_forage_flr2, th_forage_flr3, daylight_h, modelType){
  # For parallel computing 
  require(doParallel)
  require(foreach)
  numCores<-(detectCores()-1)
  registerDoParallel(numCores)
  # set number of environments 
  num_env<-18 
  # Start parallel loop 
  outcome_env_3_3_2_par<- foreach(i=1:num_env, .packages = c( "truncnorm", "purrr", "SciViews")) %dopar% {
    # Source files 
    setwd("C:/Local_R/BiPhD-ABM/May23")
    source('MOD_1_FuncSource.R')
    source('ModelSource.R')
    # Single model run 
    mod_3_3_2(days = days, N = N, env_type = i, th_forage_flr1 = th_forage_flr1, th_forage_flr2 = th_forage_flr2, th_forage_flr3= th_forage_flr3, daylight_h = daylight_h)
    }
  
  # clean up cluster 
  stopImplicitCluster()
  
  # The result we have at this point is 'outcome_env_3_3_2_par' 
  # This is a list with the averages fo each behaviour/usrvival/physiology for each timestep per environment 
  # It will go into the halflife function, so we find out at what point in time half of the birds are alive 
  
  # load any functions 
  setwd("C:/Local_R/BiPhD-ABM/May23")
  source('MOD_1_FuncSource.R')
  source('ModelSource.R')

  # Create the variable called halflife_input
  halflife_input<-outcome_env_3_3_2_par
  
  # run the t_halflife function 
  t_halflife_func(halflife_input)
  # put the output into a dataframe 
  t_HL_df<-map_dfr(t_HL_list, ~as.data.frame(t(.x)))
  t_HL_df$env<-1:18
  
  # Calculate mean 
  t_HL_mean<-mean(t_HL_df$V1)
  t_HL_SD<-sd(t_HL_df$V1)

  # prepare for output of the env_function 
  performance<<-cbind(t_HL_mean, t_HL_SD)
  colnames(performance)<-c('mean', 'SD')
  output_env_func<<-list(performance, outcome_env_3_3_2_par)
  
  # # save the data 
  setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/MOD_3_3_2/env_par")
  save(output_env_func, file=paste0(format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_env_func_out_332', 'd', days, 'N', N, 'th1', th_forage_flr1, 'th2', th_forage_flr2, 'th3', th_forage_flr3, 'dayh', daylight_h,  '.Rda'))
  
  # RETURN IF NEEDED FOR OPTIIZATION/ MAKING GRAPHS 
  return(output_env_func)
  
} # end environment function loop for 3.3.2 

# the one that runs parallel 
env_func_3_3_2_par_hpc<-function(days, N, th_forage_flr1, th_forage_flr2, th_forage_flr3, daylight_h, modelType){
  # For parallel computing 
  require(doParallel)
  require(foreach)
  numCores<-(10)
  registerDoParallel(numCores)
  # Set number of environments 
  num_env<-18 
  # Start parallel loop 
  outcome_env_3_3_2_par<- foreach(i=1:num_env, .packages = c( "truncnorm", "purrr", "SciViews")) %dopar% {
    # Single run 
    mod_3_3_2_hpc(days = days, N = N, env_type = i, th_forage_flr1 = th_forage_flr1, th_forage_flr2 = th_forage_flr2, th_forage_flr3= th_forage_flr3, daylight_h = daylight_h)
  }
  
  # clean up cluster 
  stopImplicitCluster()

  # Create the variable called halflife_input
  halflife_input<-outcome_env_3_3_2_par
  
  # run the t_halflife function 
  t_halflife_func(halflife_input)
  # put the output into a dataframe 
  t_HL_df<-map_dfr(t_HL_list, ~as.data.frame(t(.x)))
  t_HL_df$env<-1:18
  
  # Calculate mean 
  t_HL_mean<-mean(t_HL_df$V1)
  t_HL_SD<-sd(t_HL_df$V1)

  # prepare for output of the env_function 
  performance<<-cbind(t_HL_mean, t_HL_SD)
  colnames(performance)<-c('mean', 'SD')
  output_env_func<<-list(performance, outcome_env_3_3_2_par)

  # RETURN IF NEEDED FOR OPTIIZATION/ MAKING GRAPHS 
  return(output_env_func)

} # end environment function loop 

#-------------------------------------------------------------------------------------------------------------------------------------------------------

#################################################################
##     Model 4.1: Non-hoarding bird, Access to FR and FLR      ##
#################################################################

# model 1.1 
mod_4_1<-function(days, N, env_type, th_forage_fr, th_forage_flr, daylight_h){
  # Make the variables global 
  days<<-days
  N<<-N
  env_type<<-env_type
  th_forage_fr<<-th_forage_fr
  th_forage_flr<<-th_forage_flr
  daylight_h<<-daylight_h
  
  # load necessary packages for parallel (if needed)
  require(foreach)
  require(doParallel)
  # Start the model 
  
  # Start the model 
  # link to the function file 
  setwd("C:/Local_R/BiPhD-ABM/May23")
  source('MOD_1_FuncSource.R')
  
  # Set up the general environment 
  # This part is the same for each bird 
  set_up_func_general(days, env_type, daylight_h)
  
  ################################
  #      individual loops        # 
  ################################
  
  # The individual loops need to start now
  # These should be parallelised 
  
  outcome_4_1<-foreach(icount(N), .packages = "truncnorm", .combine='rbind') %do% {
    
    # Run hte temperature function 
    # Running this seperately for each individual brings in some desired stochasticity 
    temp_func(TS, Tmax_range_low, Tmax_range_high, Tmin_range_low, Tmin_range_high, days, daylight_h, n_daylight_timestep)
    
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
        
        # Calculate the current fat loss rate 
        flr_func(t,i)
        
        # Set the current temperature 
        temp_cur<<-total_temp_profile[t]
        
        # Check if it is night or day 
        if ((t%%72)<= n_daylight_timestep){
          dayOrNight<<-1                       # this means it is day 
        } else {
          dayOrNight<<-0                       # this means it is night 
        }
        
        ####################
        #     SLEEPING     #
        ####################
        
        sleep_func(t,i)
        
        if (sleep_count[i,t]==0){
          
          # RULE SPECIFIC FOR MODEL 4_1 
          
          # Access to FR and FLR and base the decision on this 
          if ((mat_fr[i,t] > th_forage_fr) && (mat_flr[i,t] > th_forage_flr)){
            
            ##################
            #    RESTING     # 
            ##################
        
            rest_func(t,i)
     
          } else {
            
            #################
            #     FORAGE    # 
            #################
            
            # At this point, foraging means to go out and find a NEW food item 
            forage_function(t,i)

            # This means that we have a non-hoarding bird that forages under the fr-threshold or flr-threshold
            # It can only eat or rest. Eat-hoarding and hoarding are not possible 
            eat_func(t,i)
            
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
        
        ####################
        #    PILFERAGE     #
        ####################
        # Pilfer, but only if there are actually caches left 
        if(mat_caches[i,t]>0){
          pilf_func(i=i,t=t,lambda=lambda)
        }
        
        
        ##################################
        #   PREPARE FOR NEXT TIMESTEP   # 
        ##################################
        ts_prep_func(t,i, TS)
        
      } # end of loop for alive individuals 
      
    } # end timestep loop
    
    # Alternatively, I could try to create lists with the output 
    list(eat_count, eat_hoard_count, forage_count, hoard_count, mat_alive, mat_caches, mat_find_food, mat_fr, mat_sc, mat_flr, mat_mass, predation_count, rest_count, retrieve_count, sleep_count, decay_count,mat_temp)
    
  } # end of the foreach loop (individuals) 
  
  # clean up cluster 
  stopImplicitCluster()
  
  #return(outcome_4_1)
  assign(paste0('outcome_4_1_env', env_type),outcome_4_1, envir=.GlobalEnv)
  
  create_df_func(outputFile = outcome_4_1, modelType = '41', env_type= env_type)

} # end of model 4.1 function - LOCAL 

########################
#   ENVIRONMENT LOOP   #
########################

# environment loop paralelel 
env_func_4_1_par<-function(days, N, th_forage_fr, th_forage_flr, daylight_h, modelType){
  # For parallel computing 
  require(doParallel)
  require(foreach)
  numCores<-(detectCores()-1)
  registerDoParallel(numCores)
  # Set number of environments 
  num_env<-18 
  # Parallel loop 
  outcome_env_4_1_par<- foreach(i=1:num_env, .packages = c( "truncnorm", "purrr", "SciViews")) %dopar% {
    # Source files 
    setwd("C:/Local_R/BiPhD-ABM/May23")
    source('MOD_1_FuncSource.R')
    source('ModelSource.R')
    # Single model 
    mod_4_1(days = days, N = N, env_type = i, th_forage_fr = th_forage_fr, th_forage_flr = th_forage_flr, daylight_h = daylight_h)
  }
  
  # clean up cluster 
  stopImplicitCluster()

  # The result we have at this point is 'outcome_env_4_1_par' 
  # These will have the same form for each of the models
  # So I will need to write a function that takes care of this and outputs the t_halflife 
  
  # load any functions 
  setwd("C:/Local_R/BiPhD-ABM/May23")
  source('MOD_1_FuncSource.R')
  source('ModelSource.R')
  
  # Create the variable called halflife_input
  halflife_input<-outcome_env_4_1_par
  
  # run the t_halflife function 
  t_halflife_func(halflife_input)
  # put the output into a dataframe 
  t_HL_df<-map_dfr(t_HL_list, ~as.data.frame(t(.x)))
  t_HL_df$env<-1:18
  
  # Calculate mean 
  t_HL_mean<-mean(t_HL_df$V1)
  t_HL_SD<-sd(t_HL_df$V1)

  performance<<-cbind(t_HL_mean, t_HL_SD)
  colnames(performance)<-c('mean', 'SD')
  output_env_func<<-list(performance, outcome_env_4_1_par)
  
  # # save the data 
  setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/MOD_4_1/env_par")
  save(output_env_func, file=paste0(format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_env_func_out_', modelType, 'd', days, 'N', N,'th_fr', th_forage_fr, 'th_flr', th_forage_flr, 'dayh', daylight_h,   '.Rda'))

  return(output_env_func)

} # end environment function loop 

#########################################################################
##   Model 4.2: Leftover-hoarding bird, Access to Fat-reserve and FLR  ##
#########################################################################

# model 4.2 
mod_4_2<-function(days, N, env_type, th_forage_fr1, th_forage_fr2, th_forage_flr1, th_forage_flr2, daylight_h){
  # make some of the variables global for saving 
  days<<-days
  N<<-N
  env_type<<-env_type
  th_forage_fr1<<-th_forage_fr1
  th_forage_fr2<<-th_forage_fr2
  th_forage_flr1<<-th_forage_flr1
  th_forage_flr2<<-th_forage_flr2
  daylight_h<<-daylight_h
  
  # load packages for parallel if needed 
  require(foreach)
  require(doParallel)
  # Start the model 
  # link to the function file 
  setwd("C:/Local_R/BiPhD-ABM/May23")
  source('MOD_1_FuncSource.R')

  # Set up the general environment 
  # This part is the same for each bird 
  set_up_func_general(days, env_type, daylight_h)
  
  ################################
  #      individual loops        # 
  ################################
  
  # The individual loops need to start now
  # These should be parallelised 
  outcome_4_2<- foreach(icount(N), .packages = "truncnorm", .combine='rbind') %do% {
    
    # Do a setup for the individual bird
    # This includes the individual temperature pattern 
    
    # Run hte temperature function 
    # Running this seperately for each individual brings in some desired stochasticity 
    temp_func(TS, Tmax_range_low, Tmax_range_high, Tmin_range_low, Tmin_range_high, days, daylight_h, n_daylight_timestep)
    
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

        # Calculate the current fat loss rate 
        flr_func(t,i)
        
        # Set the current temperature 
        temp_cur<<-total_temp_profile[t]
        
        # Check if it is night or day 
        if ((t%%72)<= n_daylight_timestep){
          dayOrNight<<-1                       # this means it is day 
        } else {
          dayOrNight<<-0                       # this means it is night 
        }
        
        ####################
        #     SLEEPING     #
        ####################
        
        sleep_func(t,i)
        
        if (sleep_count[i,t]==0){
          
          # RULE SPECIFIC FOR MODEL 4.2 
          
          # Only access to stomach-content, birds need to retrieve if the stomach content is below the lowest threshold 
          # There also needs to be a minimum number of caches available 
          if ((mat_fr[i,t]<=th_forage_fr1) && (mat_flr[i,t]<=th_forage_flr1) && (mat_caches[i,t]>retrieve_min)){
            
            ####################
            ##   RETRIEVING   ## 
            #################### 
            
            # The bird will retrieve food items 
            retrieve_func(t,i)
            # End of retrieving statement 

          } else if ((mat_fr[i,t]) > th_forage_fr2 && (mat_flr[i,t] > th_forage_flr2)){
            
            ##################
            #    RESTING     # 
            ##################
            
            rest_func(t,i)
 
          } else {
            
            #################
            #     FORAGE    # 
            #################
            
            # At this point, foraging means to go out and find a NEW food item 
            forage_function(t,i)

            # In model 4.2 the bird will eat and then hoard the leftovers 
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
        
        ####################
        #    PILFERAGE     #
        ####################
        # Pilfer, but only if there are actually caches left 
        if(mat_caches[i,t]>0){
          pilf_func(i=i,t=t,lambda=lambda)
        }
        
        ##################################
        #   PREPARE FOR NEXT TIMESTEP   # 
        ##################################
        
        ts_prep_func(t,i, TS)
        
      } # end of loop for alive individuals 

    } # end timestep loop

    # Alternatively, I could try to create lists with the output 
    list(eat_count, eat_hoard_count, forage_count, hoard_count, mat_alive, mat_caches, mat_find_food, mat_fr, mat_sc, mat_flr, mat_mass,  predation_count, rest_count, retrieve_count, sleep_count, decay_count,mat_temp)
    
  } # end of the foreach loop (individuals) 
  
  # clean up cluster 
  stopImplicitCluster()
  
  #return(outcome_1_1)
  assign(paste0('outcome_4_2_env', env_type),outcome_4_2, envir=.GlobalEnv)
  
  create_df_func(outputFile = outcome_4_2, modelType = '42', env_type= env_type)

} # end of model 4.2 function - LOCAL 

########################
#   ENVIRONMENT LOOP   #
########################

# environment loop paralelel 
env_func_4_2_par<-function(days, N, th_forage_fr1, th_forage_fr2, th_forage_flr1, th_forage_flr2, daylight_h, modelType){
  # For parallel computing
  require(doParallel)
  require(foreach)
  numCores<-(detectCores()-1)
  registerDoParallel(numCores)
  # Set number of environments 
  num_env<-18 
  # parallel loop
  outcome_env_4_2_par<- foreach(i=1:num_env, .packages = c( "truncnorm", "purrr", "SciViews")) %dopar% {
    # Source file 
    setwd("C:/Local_R/BiPhD-ABM/May23")
    source('MOD_1_FuncSource.R')
    source('ModelSource.R')
    # single model 
    mod_4_2(days = days, N = N, env_type = i, th_forage_fr1 = th_forage_fr1, th_forage_fr2 = th_forage_fr2,th_forage_flr1 = th_forage_flr1, th_forage_flr2 = th_forage_flr2, daylight_h = daylight_h)
  }
  
  # clean up cluster 
  stopImplicitCluster()
  
  # The result we have at this point is 'outcome_env_4_2_par' 
  # This is a list with the averages fo each behaviour/survival/physiology for each timestep per environment 
  # It will go into the halflife function, so we find out at what point in time half of the birds are alive 
  
  # load any functions 
  setwd("C:/Local_R/BiPhD-ABM/May23")
  source('MOD_1_FuncSource.R')
  source('ModelSource.R')
  
  # Create the variable called halflife_input
  halflife_input<-outcome_env_4_2_par
  
  # run the t_halflife function 
  t_halflife_func(halflife_input)
  # put the output into a dataframe 
  t_HL_df<-map_dfr(t_HL_list, ~as.data.frame(t(.x)))
  t_HL_df$env<-1:18
  
  # Calculate mean 
  t_HL_mean<-mean(t_HL_df$V1)
  t_HL_SD<-sd(t_HL_df$V1)
  
  # prepare for output of the env_function 
  performance<<-cbind(t_HL_mean, t_HL_SD)
  colnames(performance)<-c('mean', 'SD')
  output_env_func<<-list(performance, outcome_env_4_2_par)
  
  # # save the data 
  setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/MOD_4_2/env_par")
  save(output_env_func, file=paste0(format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_env_func_out_', 'd', days, 'N', N, 'th_fr1', th_forage_fr1, 'th_fr2', th_forage_fr2, 'th_flr1', th_forage_flr1, 'th_flr2', th_forage_flr2, 'dayh', daylight_h,  '.Rda'))
  
  # Return the output
  return(output_env_func)
  
} # end environment function loop for model 4.2 

#########################################################################
##   Model 4.3: Direct-hoarding bird, Access to Fat-reserve and FLR  ##
#########################################################################

# model 4.3.1 : direct hoarding bird with foraging up top  
mod_4_3_1<-function(days, N, env_type, th_forage_fr1, th_forage_fr2, th_forage_fr3, th_forage_flr1, th_forage_flr2, th_forage_flr3,daylight_h){
  # make some of the variables global for saving 
  days<<-days
  N<<-N
  env_type<<-env_type
  th_forage_fr1<<-th_forage_fr1
  th_forage_fr2<<-th_forage_fr2
  th_forage_fr3<<-th_forage_fr3
  th_forage_flr1<<-th_forage_flr1
  th_forage_flr2<<-th_forage_flr2
  th_forage_flr3<<-th_forage_flr3
  daylight_h<<-daylight_h
  
  # load packages for parallel if needed 
  require(foreach)
  require(doParallel)
  # Start the model 
  # link to the function file 
  setwd("C:/Local_R/BiPhD-ABM/May23")
  source('MOD_1_FuncSource.R')

  # Set up the general environment 
  # This part is the same for each bird 
  set_up_func_general(days, env_type, daylight_h)
  
  ################################
  #      individual loops        # 
  ################################
  
  # The individual loops need to start now
  # These should be parallelised 
  outcome_4_3_1<- foreach(icount(N), .packages = "truncnorm", .combine='rbind') %do% {
    # Run hte temperature function 
    # Running this seperately for each individual brings in some desired stochasticity 
    temp_func(TS, Tmax_range_low, Tmax_range_high, Tmin_range_low, Tmin_range_high, days, daylight_h, n_daylight_timestep)
    
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

        # Calculate the current fat loss rate 
        flr_func(t,i)
        
        # Set the current temperature 
        temp_cur<<-total_temp_profile[t]
        
        # Check if it is night or day 
        if ((t%%72)<= n_daylight_timestep){
          dayOrNight<<-1                       # this means it is day 
        } else {
          dayOrNight<<-0                       # this means it is night 
        }
        
        ####################
        #     SLEEPING     #
        ####################
        
        sleep_func(t,i)
        
        if (sleep_count[i,t]==0){

          # If both FR and FLR are above the 3 threhold: the bird will forage and hoard 
        
          if ((mat_fr[i,t]>th_forage_fr3) && (mat_flr[i,t]>th_forage_flr3)){

            ############################
            #  FORAGE + HOARD DIRECT   #
            ############################
 
            # At this point, foraging means to go out and find a NEW food item 
            forage_function(t,i)
            
            # Then directly just hoard it
            dir_hoard_func(t,i)

          } else if ((mat_fr[i,t]) > th_forage_fr2 | (mat_flr[i,t] > th_forage_flr2)){
            
            ##################
            #    RESTING     # 
            ##################

            rest_func(t,i)

          } else if ((mat_fr[i,t]<= th_forage_fr1) && (mat_flr[i,t] <= th_forage_flr1) && (mat_caches[i,t]>retrieve_min)){

            ####################
            ##   RETRIEVING   ## 
            #################### 
            
            # The bird will retrieve food items 
            retrieve_func(t,i)
            # End of retrieving statement 
            
          } else {
            
            #################
            #     FORAGE    # 
            #################

            # At this point, foraging means to go out and find a NEW food item 
            forage_function(t,i)
            
            # The bird can then eat the food item 
            # Hoarding happens directly in this model, so foraging here just leads to eating
            eat_func(t,i)
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
        
        ####################
        #    PILFERAGE     #
        ####################
        # Pilfer, but only if there are actually caches left 
        if(mat_caches[i,t]>0){
          pilf_func(i=i,t=t,lambda=lambda)
        }
        
        ##################################
        #   PREPARE FOR NEXT TIMESTEP   # 
        ##################################
        
        ts_prep_func(t,i, TS)
        
      } # end of loop for alive individuals 

    } # end timestep loop

    # Alternatively, I could try to create lists with the output 
    list(eat_count, eat_hoard_count, forage_count, hoard_count, mat_alive, mat_caches, mat_find_food, mat_fr, mat_sc, mat_flr, mat_mass,  predation_count, rest_count, retrieve_count, sleep_count,decay_count, mat_temp)
    
  } # end of the foreach loop (individuals) 
  
  # clean up cluster 
  stopImplicitCluster()
  
  #return(outcome_1_1)
  assign(paste0('outcome_4_3_1_env', env_type),outcome_4_3_1, envir=.GlobalEnv)
  
  create_df_func(outputFile = outcome_4_3_1, modelType = '431', env_type= env_type)

} # end of model 4.3.1 function - LOCAL 


# model 4.3.1 : direct hoarding bird with foraging up top  
mod_4_3_2<-function(days, N, env_type, th_forage_fr1, th_forage_fr2, th_forage_fr3, th_forage_flr1, th_forage_flr2, th_forage_flr3,daylight_h){
  # make some of the variables global for saving 
  days<<-days
  N<<-N
  env_type<<-env_type
  th_forage_fr1<<-th_forage_fr1
  th_forage_fr2<<-th_forage_fr2
  th_forage_fr3<<-th_forage_fr3
  th_forage_flr1<<-th_forage_flr1
  th_forage_flr2<<-th_forage_flr2
  th_forage_flr3<<-th_forage_flr3
  daylight_h<<-daylight_h
  
  # load packages for parallel if needed 
  require(foreach)
  require(doParallel)
  # Start the model 
  # link to the function file 
  setwd("C:/Local_R/BiPhD-ABM/May23")
  source('MOD_1_FuncSource.R')
  
  # Set up the general environment 
  # This part is the same for each bird 
  set_up_func_general(days, env_type, daylight_h)
  
  ################################
  #      individual loops        # 
  ################################
  
  # The individual loops need to start now
  # These should be parallelised 
  outcome_4_3_2<- foreach(icount(N), .packages = "truncnorm", .combine='rbind') %do% {
    # Run hte temperature function 
    # Running this seperately for each individual brings in some desired stochasticity 
    temp_func(TS, Tmax_range_low, Tmax_range_high, Tmin_range_low, Tmin_range_high, days, daylight_h, n_daylight_timestep)
    
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
        
        # Calculate the current fat loss rate 
        flr_func(t,i)
        
        # Set the current temperature 
        temp_cur<<-total_temp_profile[t]
        
        # Check if it is night or day 
        if ((t%%72)<= n_daylight_timestep){
          dayOrNight<<-1                       # this means it is day 
        } else {
          dayOrNight<<-0                       # this means it is night 
        }
        
        ####################
        #     SLEEPING     #
        ####################
        
        sleep_func(t,i)
        
        if (sleep_count[i,t]==0){

          # If both FR and FLR are above the 3 threhold: the bird will forage and hoard 
          
          if ((mat_fr[i,t]>th_forage_fr3) && (mat_flr[i,t]>th_forage_flr3)){

            ##################
            #    RESTING     # 
            ##################

            rest_func(t,i)
            
          } else if ((mat_fr[i,t]) > th_forage_fr2 | (mat_flr[i,t] > th_forage_flr2)){

            ############################
            #  FORAGE + HOARD DIRECT   #
            ############################

            # At this point, foraging means to go out and find a NEW food item 
            forage_function(t,i)
            
            # Then directly just hoard it
            dir_hoard_func(t,i)

          } else if ((mat_fr[i,t]<= th_forage_fr1) && (mat_flr[i,t] <= th_forage_flr1) && (mat_caches[i,t]>retrieve_min)){

            ####################
            ##   RETRIEVING   ## 
            #################### 
            
            # The bird will retrieve food items 
            retrieve_func(t,i)
            # End of retrieving statement 
            
          } else {
            
            #################
            #     FORAGE    # 
            #################
            
            # At this point, foraging means to go out and find a NEW food item 
            forage_function(t,i)
            
            # The bird can then eat the food item 
            # Hoarding happens directly in this model, so foraging here just leads to eating
            eat_func(t,i)
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
        
        ####################
        #    PILFERAGE     #
        ####################
        # Pilfer, but only if there are actually caches left 
        if(mat_caches[i,t]>0){
          pilf_func(i=i,t=t,lambda=lambda)
        }
        
        ##################################
        #   PREPARE FOR NEXT TIMESTEP   # 
        ##################################
        
        ts_prep_func(t,i, TS)
        
      } # end of loop for alive individuals 
      
    } # end timestep loop

    # Alternatively, I could try to create lists with the output 
    list(eat_count, eat_hoard_count, forage_count, hoard_count, mat_alive, mat_caches, mat_find_food, mat_fr, mat_sc, mat_flr, mat_mass,  predation_count, rest_count, retrieve_count, sleep_count, decay_count,mat_temp)
    
  } # end of the foreach loop (individuals) 
  
  # clean up cluster 
  stopImplicitCluster()
  
  #return(outcome_1_1)
  assign(paste0('outcome_4_3_2_env', env_type),outcome_4_3_2, envir=.GlobalEnv)
  
  create_df_func(outputFile = outcome_4_3_2, modelType = '432', env_type= env_type)
  
} # end of model 4.3.2 function - LOCAL 


########################
#   ENVIRONMENT LOOP   #
########################

# the one that runs parallel 
env_func_4_3_1_par<-function(days, N, th_forage_fr1, th_forage_fr2, th_forage_fr3,th_forage_flr1, th_forage_flr2, th_forage_flr3, daylight_h, modelType){
  # parallel computing 
  require(doParallel)
  require(foreach)
  numCores<-(detectCores()-1)
  registerDoParallel(numCores)
  # Number of environments 
  num_env<-18 
  # paralel loop 
  outcome_env_4_3_1_par<- foreach(i=1:num_env, .packages = c( "truncnorm", "purrr", "SciViews")) %dopar% {
    # Source files
    setwd("C:/Local_R/BiPhD-ABM/May23")
    source('MOD_1_FuncSource.R')
    source('ModelSource.R')
    # single model run 
    mod_4_3_1(days = days, N = N, env_type = i,th_forage_fr1 = th_forage_fr1, th_forage_fr2 = th_forage_fr2, th_forage_fr3= th_forage_fr3, th_forage_flr1 = th_forage_flr1, th_forage_flr2 = th_forage_flr2, th_forage_flr3= th_forage_flr3, daylight_h = daylight_h)
    
  }
  
  # clean up cluster 
  stopImplicitCluster()
  
  # The result we have at this point is 'outcome_env_4_3_1_par' 
  # This is a list with the averages fo each behaviour/usrvival/physiology for each timestep per environment 
  # It will go into the halflife function, so we find out at what point in time half of the birds are alive 
  
  # load any functions 
  setwd("C:/Local_R/BiPhD-ABM/May23")
  source('MOD_1_FuncSource.R')
  source('ModelSource.R')

  # Create the variable called halflife_input
  halflife_input<-outcome_env_4_3_1_par
  
  # run the t_halflife function 
  t_halflife_func(halflife_input)
  # put the output into a dataframe 
  t_HL_df<-map_dfr(t_HL_list, ~as.data.frame(t(.x)))
  t_HL_df$env<-1:18
  
  # Calculate mean 
  t_HL_mean<-mean(t_HL_df$V1)
  t_HL_SD<-sd(t_HL_df$V1)
  
  # prepare for output of the env_function 
  performance<<-cbind(t_HL_mean, t_HL_SD)
  colnames(performance)<-c('mean', 'SD')
  output_env_func<<-list(performance, outcome_env_4_3_1_par)
  
  #  save the data 
  setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/MOD_4_3_1/env_par")
  save(output_env_func, file=paste0(format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_env_func_out_431', 'd', days, 'N', N, 'th_fr1', round(th_forage_fr1, digits = 3), 'th_fr2', round(th_forage_fr2, digits=3), 'th_fr3', round(th_forage_fr3, digits=3),'th_flr1', round(th_forage_flr1, digits=3), 'th_flr2', round(th_forage_flr2, digits=3), 'th_flr3', round(th_forage_flr3, digits=3), 'dayh', daylight_h,  '.Rda'))
  
  # RETURN IF NEEDED FOR OPTIIZATION/ MAKING GRAPHS 
  return(output_env_func)
  
} # end environment function loop for 4.3.1 

# The one that runs parallel 
env_func_4_3_2_par<-function(days, N, th_forage_fr1, th_forage_fr2, th_forage_fr3,th_forage_flr1, th_forage_flr2, th_forage_flr3, daylight_h, modelType){
  # for parallel computing 
  require(doParallel)
  require(foreach)
  numCores<-(detectCores()-1)
  registerDoParallel(numCores)
  # Set number of environments 
  num_env<-18 
  # parallel loop 
  outcome_env_4_3_2_par<- foreach(i=1:num_env, .packages = c( "truncnorm", "purrr", "SciViews")) %dopar% {
    # Source files 
    setwd("C:/Local_R/BiPhD-ABM/May23")
    source('MOD_1_FuncSource.R')
    source('ModelSource.R')
    # single model run 
    mod_4_3_2(days = days, N = N, env_type = i, th_forage_fr1 = th_forage_fr1, th_forage_fr2 = th_forage_fr2, th_forage_fr3= th_forage_fr3,th_forage_flr1 = th_forage_flr1, th_forage_flr2 = th_forage_flr2, th_forage_flr3= th_forage_flr3, daylight_h = daylight_h)
  }
  
  # clean up cluster 
  stopImplicitCluster()
  
  # The result we have at this point is 'outcome_env_4_3_2_par' 
  # This is a list with the averages fo each behaviour/usrvival/physiology for each timestep per environment 
  # It will go into the halflife function, so we find out at what point in time half of the birds are alive 
  
  # load any functions 
  setwd("C:/Local_R/BiPhD-ABM/May23")
  source('MOD_1_FuncSource.R')
  source('ModelSource.R')

  # Create the variable called halflife_input
  halflife_input<-outcome_env_4_3_2_par
  
  # run the t_halflife function 
  t_halflife_func(halflife_input)
  # put the output into a dataframe 
  t_HL_df<-map_dfr(t_HL_list, ~as.data.frame(t(.x)))
  t_HL_df$env<-1:18
  
  # Calculate mean 
  t_HL_mean<-mean(t_HL_df$V1)
  t_HL_SD<-sd(t_HL_df$V1)

  # prepare for output of the env_function 
  performance<<-cbind(t_HL_mean, t_HL_SD)
  colnames(performance)<-c('mean', 'SD')
  output_env_func<<-list(performance, outcome_env_4_3_2_par)
  
  # # save the data 
  setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/MOD_4_3_2/env_par")
  save(output_env_func, file=paste0(format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_env_func_out_432', 'd', days, 'N', N, 'th_fr1', round(th_forage_fr1, digits = 3), 'th_fr2', round(th_forage_fr2, digits=3), 'th_fr3', round(th_forage_fr3, digits=3),'th_flr1', round(th_forage_flr1, digits=3), 'th_flr2', round(th_forage_flr2, digits=3), 'th_flr3', round(th_forage_flr3, digits=3), 'dayh', daylight_h,  '.Rda'))
  
  # RETURN IF NEEDED FOR OPTIIZATION/ MAKING GRAPHS 
  return(output_env_func)
  
} # end environment function loop for 4.3.2 - LOCAL 
