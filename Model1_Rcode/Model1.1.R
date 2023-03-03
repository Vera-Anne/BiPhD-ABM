#################################################################
##   Model 1.1: Non-hoarding bird, Access to Stomach Content   ##
#################################################################


setwd('H:/Desktop/GIT_March23/BiPhD-ABM/Model1_Rcode/Restructured_code')

source('hoarding_model_functions.R')




################################# 
#   set up directories 1.1     # 
################################
# # Set up the main directory for where you want the figures saved 
# # This can be replaced by any folder you have on your computer (just make sure you have continuous connection if its a webfolder)
# mainDir<-'C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Figures/5-combi_model/MOD_1_1'
# setwd(mainDir)
# # Run the following if doing this for the first time on devide: 
# # create list of folders that we want present 
# folders<-c('1-run_model', '2-run_opt', '3-env_loop', '4-opt_loop', '5-beh_loop')
# # Check if they exist and if not, create them 
# # Note that this code will warn you if it already existed 
# for (folder in folders){
#   dir.create(file.path(mainDir, folder ), showWarnings = TRUE)
# }
# 


###############################
#    Functions & running 1.1  #
###############################
MOD_1_1_func<-function(days, N, env_type, th_forage_sc, th_forage_fr, daylight_h, sim_type){
  
  # Set up the environment 
  set_up_env(days,N, env_type, daylight_h)
  
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
    
    ################################
    #      individual loops        # 
    ################################
    
    # now start a loop for every individual 
    for (i in (1:N)){
      
      # Check if individual is alive? 
      
      
      # in step 1 all birds are alive 
      if (t==1){
        mat_alive[i,t]<<-1
      } else if (mat_alive[i,(t-1)]==0){
        # if not step 1, check if bird was previously dead
        # if previously dead, it needs to be dead now 
        mat_alive[i,t]<<-0
      } else if (mat_fr[i,t]==0){
        # if not step 1 and not previously dead 
        # check if the bird should die now 
        mat_alive[i,t]<<-0
      } else {
        # in all other cases the bird is alive 
        mat_alive[i,t]<<-1
      }
      
      ################
      #  DEAD BIRDS  #
      ################
      if(mat_alive[i,t]==0){
        # these are the dead birds 
        # Set the matrices to 'NA' for dead birds 
        # For the fr matrix 
        mat_fr[i,t]<<-NA
        # For the mass matrix 
        mat_mass[i,t]<<-NA
        # For the sc matrix 
        mat_sc[i,t]<<-NA
        # for the caches matrix 
        mat_caches[i,t]<<-NA
      } else {
        
        #################
        #  ALIVE BIRDS  #
        #################
        
        # Set the current BMR 
        # Note: I have made the decision to calculate this at the start of the tick. 
        # So this is before any behaviour, or food is moved around 
        # set the current mass 
        mass_cur<<-mat_mass[i,t]
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
          sleep_count[i,t]<<-1
          # set the forage to 0
          forage_count[i, t]<<-0
          # set the resting matrix to 0
          rest_count[i,t]<<-0
          # set the retrieval matrix to 0 
          retrieve_count[i,t]<<-0
          # set the eat-hoarding matrix to 0
          eat_hoard_count[i,t]<<-0
          # set the eating matrix to 0 
          eat_count[i,t]<<-0
          
          # set the BMR-multi
          BMR_multi<<-1
          #set the predation risk 
          Patt_cur<<-Patt_sleep
          
          # Food will be moved from the stomach
          # Into the fat reserves 
          # and be burned depending on BMR-multi
          # in the ' Everyone '  part of the code below
          # end of birds that are asleep   
        } else {
          
          # NON SLEEPING BIRDS START HERE : >>>>>>>>>
          # set the sleeping matrix to 0 
          sleep_count[i,t]<<-0
          
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
            forage_count[i, t]<<-1
            # set the resting matrix to 0
            rest_count[i,t]<<-0
            
            #set the predation risk 
            # Note: this is currently the same for all types of foraging
            Patt_cur<<-Patt_for
            
            # The non-hoarding birds can only 'eat' food they find. 
            
            # Run the forage function 
            # The outcome here is 'food_item_found'
            forage_function(num_food_mean, prob_b_forage, b_size)
            
            # Pop this in the matrix (this is in number of items found)
            mat_find_food[i,t]<<-food_item_found
            # convert to grams
            food_item_found_gram<<-(food_item_found*food_item)
            
            # Now, increase the stomach content
            mat_sc[i,(t)]<<-(mat_sc[i,t])+(food_item_found_gram)
            # now check if this doesn't exceed the stomach size
            # if so, set the stomach content to stomach size 
            if (mat_sc[i,(t)]>stom_size){
              mat_sc[i,(t)]<<-stom_size
            }
            # Set the BMR to the right level: cost of foraging
            # BMR-multi is not a global variable: stays local with the agent
            BMR_multi<<-8
            #set the predation risk 
            Patt_cur<<-Patt_for
            
            # update the eating-matrix 
            # update the global counters 
            eat_hoard_count[i,t]<<-0
            retrieve_count[i,t]<<-0
            eat_count[i,t]<<-1
            
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
            forage_count[i,t]<<-0
            retrieve_count[i,t]<<-0
            eat_hoard_count[i,t]<<-0
            eat_count[i,t]<<-0
            # set the rest matrix to 1
            rest_count[i,t]<<-1
            
            # SET AGENT OWNED VARIABLES 
            BMR_multi<<-1.95                    # resting BMR 
            # the stomach content stays the same (initial value)
            # or at least for now 
            
            #set the predation risk 
            Patt_cur<<-Patt_rest
            
          } # end resting statement 
        } # end of 'Time of day = day ' statement 
        
        
        ###################
        #    EVERYONE     # 
        ###################
        # No matter what behaviour you've done, these need updating for all alive birds
        
        # PREDATION 
        # first check if the bird actually survived the behaviour it did 
        mass_cur<<-mat_mass[i,t]                                            # find out the current mass of the bird 
        Pcap_cur<<-(0.78+(0.5*(10^-8)*exp(1.4*mass_cur)))                  # calculate the current Pcapture 
        Pkill_cur<<-Pcap_cur*Patt_cur                                       # calculate the current Pkill 
        mat_Pkill[i,t]<<-Pkill_cur                                         # put in the matrix 
        # now check if the bird dies or not 
        Psurv_cur<-runif(1)                                                 # Random number between 0 and 1 for survival chance 
        if(Psurv_cur<(mat_Pkill[i,t])){                                            # if the prob for survival < prob to die 
          mat_alive[i,t]<<-0                                                # Set the matrix to 'dead' 
          predation_count[i,t]<<-1
          #print(paste0('a bird ', 'i=', i , ' got eaten at t=', t))
        } else {
          # Surviving birds should update their values: 
          predation_count[i,t]<<-0
          
          # UPDATE THE FAT RESERVES AND STOMACH CONTENT
          # SC down and FR up 
          # first check if stomach has enough to actually move
          # move food out of stomach into fat 
          if (mat_sc[i,(t)]>= stom_to_fat){
            # new sc from resting/foraging can be used
            mat_sc[i,(t)]<<-(mat_sc[i,(t)]-stom_to_fat)
            # the new fat reserve has not been determined yet
            mat_fr[i,(t)]<<-(mat_fr[i,t]+stom_to_fat)
          } else {
            mat_fr[i,t]<<-(mat_fr[i,t]+mat_sc[i,t])   # move whatever is left in the stomach to fat 
            mat_sc[i,t]<<-0                           # set the stomach content to 0 
          }
          
          # ENERGY EXPENDITURE 
          # Set the fat reserves down depending on bmr-multi
          
          # first subtract the amount
          mat_fr[i,(t)]<<-(mat_fr[i,t]-(bmr_cur*BMR_multi))
          # then make sure if this doesnt go below 0 
          if((mat_fr[i,(t)]<0)){
            mat_fr[i,(t)]<<-0
          }
          # or above the maximum for fat-reserves 
          if((mat_fr[i,(t)]>fat_max)){
            mat_fr[i,(t)]<<-fat_max
          }
          # check if the stomach content is above 0 
          if((mat_sc[i, (t)]<0)){
            mat_sc[i, (t)]<<-0
          }
          # check if it is not above the stomach size either
          if((mat_sc[i,t]>stom_size)){
            mat_sc[i,t]<<-stom_size
          }
          
          # SET MASS 
          # set the new mass for all individuals 
          mat_mass[i,t]<<-((mass_init[i])+(mat_fr[i,t])+(mat_sc[i,t]))
          
          
          # MOVE ALL VARAIBLES TO T+1 
          # Note that this should only happen if youre not in the last timestep 
          if(t<TS){
            # For the fr matrix 
            mat_fr[,(t+1)]<<-mat_fr[,t]
            # For the mass matrix 
            mat_mass[,(t+1)]<<-mat_mass[,t]
            # For the sc matrix 
            mat_sc[,(t+1)]<<-mat_sc[,t]
            # for the caches matrix 
            mat_caches[,(t+1)]<<-mat_caches[,t]
          }
          
        } # end of statement for birds that survived predation 
        
      } # end of loop for alive individuals 
      
    } # end of loop for each individual 
    
    ##########################
    #    wrap up timestep    # 
    ##########################
    
    # code testing 
    #print(paste('timestep ', t, 'done'))
    
    # COUNT WHAT HAPPENED 
    # For each timestep, count what the birds are doing 
    # These are global now, can be changed if not necessary 
    total_forage[1,t]<<-sum(forage_count[,t], na.rm=TRUE)             # counts how many birds foraged this timestep
    total_rest[1,t]<<-sum(rest_count[,t], na.rm=TRUE)                 # counts how many birds rested this timestep 
    total_alive[1,t]<<-sum(mat_alive[,t], na.rm=TRUE)                 # counts how many birds are alive this timestep
    total_retrieve[1,t]<<-sum(retrieve_count[,t], na.rm=TRUE)         # counts how many birds are retrieving in this timestep
    total_eat_hoard[1,t]<<-sum(eat_hoard_count[,t], na.rm=TRUE)       # counts how many birds are eat-hoarding in this timestep
    total_eat[1,t]<<-sum(eat_count[,t], na.rm=TRUE)                   # counts how many birds are eating this timestep
    total_predated[1,t]<<-sum(predation_count[,t], na.rm=TRUE) # how many birds were killed by predation in this timestep 
    total_sleep[1,t]<<-sum(sleep_count[,t], na.rm=TRUE)
    
    # CALCULATE MEANS 
    sc_mean[t]<<-mean(mat_sc[,t], na.rm = TRUE)        # adds mean stomach content for this timestep to matrix
    fr_mean[t]<<-mean(mat_fr[,t], na.rm = TRUE)        # adds mean fat reserve for this timestep to matrix 
    mass_mean[t]<<-mean(mat_mass[,t], na.rm = TRUE)    # adds mean mass for this timestep to mean-matrix
    alive_mean[t]<<-mean(mat_alive[,t], na.rm= TRUE)
    
    ####################
    #      PLOT        #
    ####################
    
    # Make sure to plot every so often 
    # plots are local for now, this can be changed later 
    if (t == 1){
      #dev.new()
    }
    if ((t/plot_interval)==floor(t/plot_interval) && sim_type=='run_model' ){
      par(mfrow=c(5,2))
      Sys.sleep(0.05)          # forces an update to the plotting window 
      # 1 MEAN STOMACH CONTENT
      plot1<<-plot(1:t, sc_mean[1,(1:t)], ylim=c(0,(stom_size+0.1)), ylab='Mean stomach content', xlab='timestep', main='Mean Sc', type='l')
      abline(h=stom_size, col='red')
      # 2 MEAN FAT RESERVE 
      plot2<<-plot(1:t, fr_mean[1,(1:t)], ylim=c(0,(fat_max+0.5)), ylab='Mean fat reserve', xlab='timestep', main='Mean Fr', type='l')
      abline(h=fat_max, col='red')
      # 3 MEAN MASS 
      plot3<<-plot(1:t, mass_mean[1,(1:t)], ylim=c(0,(20)), ylab='Mean mass', xlab='timestep', main='Mean mass', type='l')
      # 4 NUMBER OF BIRDS ALIVE 
      plot4<<-plot(1:t, total_alive[1,(1:t)], ylim=c(0, N), ylab='Number of birds alive', xlab='Timestep', main='Number birds alive', type='l')
      # 5
      plot5<<-plot(1:t, ((total_eat[1,(1:t)])/(total_alive[1,(1:t)])*100), ylim=c(0, 100), ylab='%', xlab='Timestep', main='Percentage of alive birds eating', type='l')
      
      # 6
      plot6<<-plot(1:t, ((total_rest[1,(1:t)])/(total_alive[1,(1:t)])*100), ylim=c(0, 100), ylab='%', xlab='Timestep', main='Percentage of alive birds resting', type='l')
      
      # 7
      plot7<<-plot(1:t, ((total_retrieve[1,(1:t)])/(total_alive[1,(1:t)])*100), ylim=c(0, 100), ylab='%', xlab='Timestep', main='Percentage of alive birds retrieving', type='l')
      
      # 8
      plot8<<-plot(1:t, ((total_eat_hoard[1,(1:t)])/(total_alive[1,(1:t)])*100), ylim=c(0, 100), ylab='%', xlab='Timestep', main='Percentage of alive birds eat-hoarding', type='l')
      
      # 7: To show predation
      plot9<<-plot(1:t, (total_predated[1,(1:t)]), ylim=c(0, 5), ylab='# killed by predation', xlab='Timestep', main='Number of birds killed by predation', type='l')
      
      # 10 total forage 
      plot10<<-plot(1:t, ((total_forage[1,(1:t)])/(total_alive[1,(1:t)])*100), ylim=c(0, 100), ylab='%', xlab='Timestep', main='Percentage of alive birds foraging', type='l')
      
      
      mtext((paste('Days=', days, '_N=', N, 'Daylight_h=', daylight_h, 'MaxT-range=', Tmax_range_low, '-', Tmax_range_high, ' MinT-range=', Tmin_range_high, '-', Tmin_range_low, '_th-fr=', th_forage_fr, '_food-m=',num_food_mean, 'sim_type=', sim_type)), side=3, cex=0.8,line=-2, outer=TRUE)
      Sys.sleep(0)             # turns that back off 
    }# end if statement for plots
    
  } # end of big timestep loop 
  
  # create variable with the number of the last timesstep done 
  last_T<<-TS
  
  # Plot some initial distributions if wanted 
  if(plot_init_value==1){
    par(mfrow=c(2,3))
    hist(mass_init, xlim=c(8,9),ylim=c(0,40), breaks=5)
    hist(fr_init, xlim=c(0,4),ylim=c(0,40), breaks=5)
    hist(sc_init, xlim=c(0,0.3),ylim=c(0,40), breaks=5)
    hist(mat_mass[,last_T], main='mass at last T', xlim=c(8,9),ylim=c(0,40), breaks=5)
    hist(mat_fr[,last_T], main='Fr at last T', xlim=c(0,4),ylim=c(0,40), breaks=5)
    hist(mat_sc[,last_T], main='Sc at last T',xlim=c(0,0.3),ylim=c(0,40), breaks=5)
  }
  
  # This variable is needed for optimisations 
  # Calculates the mean probability of being alive in the last timestep for the current conditions 
  birds_alive_at_end<<-alive_mean[last_T]
  
  # Print some text to keep track of the simulation 
  if(sim_type=='run_opt_sc'){                                                 
    print(paste0('1.1 function did run for th:', current_th_sc ))
  }
  if(sim_type=='run_opt_fr'){                                                     
    print(paste0('1.1 function did run for fr:', current_th_fr ))
  }
  
  # print the graph with 10 panels (beh & sc & fr) if you are just running the model 
  if(sim_type=='run_model'){
    print(paste0('ready to save MOD 1.1 simulation plots'))
    setwd(paste0(mainDir, '/1-run_model//')) # set current wd 
    dev.print(pdf, (paste0('Simulation_Days=', days, '_N=', N, 'env=', env_type, 'MaxT-range=', Tmax_range_low, '-', Tmax_range_high, ' MinT-range=', Tmin_range_high, '-', Tmin_range_low, '_th-fr=', th_forage_fr, '_th-sc=', th_forage_sc, '_food-m=',num_food_mean, '_', 'Daylight_h=', daylight_h, '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))
    
  }
  
  
  
  
} # end the mod1.1 function 

# Run it 
dev.new()
MOD_1_1_func(days=30, N=100, env_type=8, th_forage_sc=0.2, th_forage_fr=1, daylight_h=8, sim_type='run_model')
MOD_1_1_func(days=30, N=100, env_type=8, th_forage_sc=0.2, th_forage_fr=1, daylight_h=8, sim_type='run_model')