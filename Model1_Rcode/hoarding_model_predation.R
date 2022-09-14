#################################
# Small bird in winter - ABM 
# Start date: 07/09/2022
# Vera Vinken 

#################################

##    Description     ##

# copied from 'hoarding_model_optimisation' on 7/09/2022 
# Added predation to the model 

# Days: This model has a 24 hour structure. Every hour is devided in 3 20min blocks
#       This gives a total of 72 timesteps per 'day'. 
#       Daylight start and daylight end are hardcoded in the 'setup' function
#       These can be changed later if needed. 



##    Steps for next model      ## 

# need to see if the cut off values for the retrieval threshold make sense now in new model --> 0.12 needs to be the minimum to survive is foraging is unsuccesful. 0.75 is what is needed for night survival
# cost of retrieval and foraging could rely on the number of items found 
# When rounding up/down the food items to grams and the other way around. should ceiling/floor be used? or the 'neutral' way as it is now? 
# Check if the initial caches distributuion makes sense 
# Check if the retrieval_min makes sense --> this should be based on how much the cost is of retrieving? 
# Check if the cost for retrieval needs to depend on number of caches retrieved
# Why does stomach content not drop to 0? 
# There is no predation in this model 
# fix the bmr multi for retrieval 
# check if all the bmr funxtions are correct 
# have some sort of automated/easier directory settings. Currently very messy. 
# do we need it to run for 30 days before doing anything? 
# change name of the model (forage or hoard is confusing )
# what about pilferage ? 
# should teh individual and timestep loop be the other way around? 


##    addressed in this version  ## 

# Adding predation as it was described in the original netlogo model 


##############################
#      load packages         #
##############################
library(usethis)
library(devtools)
# install_github("olafmersmann/truncnorm")
library(truncnorm)
library(pracma)
library(ggplot2)
library(plotly) # for 3D surface plot 
library(rgl)
library(plot3D)
library(htmlwidgets)
library(webshot)

##############################
#       input parameters    # 
##############################

# Use these if not running as function 

T<-100              # number of timesteps
N<-100              # number of individuals
temp<-(-5)          # Temperature 
temp_day<<--5
temp_night<<--5
th_forage_sc<-0.2   # threshold: if SC below this you forage 
th_forage_fr<-1     # threshold: if Fr below this you forage (AND above is true)
num_food<-1         # number of food items found (this should be a distribution)
num_food_max<-6
num_food_mean<-3
num_cache_min<-50
num_cache_max<-100
noplot<-1 

##################################
#  set-up environment function   #
##################################

set_up_env<-function(T,N, temp_cur, num_food_mean, num_food_max){
  
  # Want to plot some initial value graphs? 
  # 1 for yes, 0 for no 
  plot_init_value<<-0
  
  # Set up some parameters (Global)
  food_item<<-0.064    # value of a food item 
  stom_size<<-0.4      # stomach size of the bird 
  stom_to_fat<<-0.132  # variable that determines how many grams of sc go to fat
  fat_max<<-4          # maximum fat reserve in gram (Pravosudov & Lucas, 2001)
  plot_interval<<-50   # every x timestep a dot on the graph is added 
  start_day<<-27      # set the start of the day. Example: 27/3= 9.00 am 
  end_day<<-45        # set the end of the day.   Example: 45/3= 15.00 
  num_cache_min<<-50  # minimum number of caches that each bird has initially 
  num_cache_max<<-100 # maximum number of caches each bird has initially 
  retrieve_min<<-5    # minimum number of caches needed to make retrieval worth it 
  
  # Do some calculations for food distributions: 
  gram_food_mean<<-num_food_mean*food_item        # Sets the grams of fat found on average per time step
  gram_food_max<<-num_food_max*food_item          # sets the maximum grams of fat found per time step 
  
  # create individual matrices (Global)
  mat_alive<<-matrix(NA, N, (T))            # matrix to keep track of who's alive 
  mat_sc<<-matrix(NA, N, (T))               # matrix to keep track of stomach contents
  mat_fr<<-matrix(NA, N, (T))               # matrix to keep track of fat reserves 
  mat_mass<<-matrix(NA,N,(T))               # matrix to keep track of mass of birds 
  mat_caches<<-matrix(NA,N,(T))             # matrix to keep track of the number of caches each bird has at each timestep
  mat_Pkill<<-matrix(NA,N,(T))              # matrix to keep track of what Pkill every bird had at each timestep
  
  
  # fill in some initial values for agent variables  (global)
  mass_init<<-8+(rtruncnorm(N, a=0.01, b=0.2, mean=0.1, sd=0.01))             # Gives initial mass from normal distribution (Polo et al. 2007)
  sc_init<<-0+(rtruncnorm(N, a=0, b=stom_size, mean=(stom_size/2), sd=0.01))  # gives initial stomach content from equal distribution
  fr_init<<-0+(rtruncnorm(N, a=0, b=fat_max, mean=(fat_max/2), sd=1))         # gives initial fat reserves for random number between 0-4
  alive_init<<-rep(1, N )                                                     # all birds are alive at the start 
  caches_init<<-round(0+(rtruncnorm(N, a=num_cache_min, b=num_cache_max, mean=((num_cache_min+num_cache_max)/2), sd=25))) # initial cache numbers for birds rounded to closest integer
  
  # Put these in first column of the matrices  
  mat_alive [,1]<<-alive_init
  mat_sc[,1]<<-sc_init
  mat_fr[,1]<<-fr_init
  mat_mass[,1]<<-mass_init
  mat_caches[,1]<<-caches_init
  
  # Create empty matrices to keep track of numbers (Global)
  # keep track of means 
  sc_mean<<-matrix(NA, 1, (T))
  fr_mean<<-matrix(NA, 1, (T))
  mass_mean<<-matrix(NA,1, (T))
  alive_mean<<-matrix(NA,1, T)
  caches_mean<<-matrix(NA,1,(T))
  # count what the birds are up to (Global)
  forage_count<<-matrix(NA, N, (T))
  rest_count<<-matrix(NA, N, (T))
  sleep_count<<-matrix(NA, N,T)
  retrieve_count<<-matrix(NA, N, T)
  eat_hoard_count<<-matrix(NA, N, T)
  eat_count<<-matrix(NA, N, T)
  predation_count<<-matrix(NA,N,(T))                               # Keep track of how many birds have actually been killed by predation
  
  # total number of birds doing behaviours (Global)
  total_forage<<-matrix(NA, 1, T)                  # total number of birds foraging each timestep
  total_rest<<-matrix(NA,1, T)                     # total number of birds resting each timestep 
  total_alive<<-matrix(NA,1,T)                     # total number of birds alive each timestep 
  total_retrieve<<-matrix(NA, 1, T)                # total number of birds retrieving each timestep
  total_eat_hoard<<-matrix(NA, 1, T)              # total number of birds eat-hoarding in each timestep
  total_eat<<-matrix(NA, 1, T)                    # total number of birds eating in each timestep 
  total_predated<<-matrix(NA, 1, T)
  
  # Set up the food distribution 
  # this doesnt work cause it gets negative values
  # food_distr<-rnorm(100, mean=0.192, sd=0.4)
  
  # set metabolic rates (function )
  mr_function<<-function(temp_cur){                 # see pravosudov & lucas (2001) and Lucas & Walter (1991) for details 
    mr_cur<<-45.65-(1.33*temp_cur)
  }
  
  bmr_function<<-function(mr_cur, mass_cur){        # see pravosudov & lucas (2001) and Lucas & Walter (1991) for details
    bmr_cur<<-0.00616*mr_cur*((mass_cur/1000)^0.66)   # please note that the 0.00616 is for 20 min intervals 
  }
  
  # Do some calculations for the daylength etc. 
  daylength<<-(end_day-start_day-1)
  
  # PREDATION  
    # Pattack: 
    # In the current version Pattack for rest and sleep is 0 
    # Pattack for any foraging behavior (retrieve, eat-hoard, eat) 
    # This is as it is set in Pravosudov and Lucas 2001, citing Lima 1986
    Patt_for<<-0.000667
    Patt_sleep<<-0
    Patt_rest<<-0

  
} # end set-up function 

################################################################################
##################     NON-HOARDING BIRD MODEL STARTS HERE    ##################
################################################################################

###############################
#   Rest or Forage function   #     # As in rest_or_forage (don't touch right now)
###############################

rest_or_forage<-function(T, N, temp_day, temp_night, th_forage_sc, th_forage_fr, num_food_mean, num_food_max, noplot){
  
  # Set up the environment: run environment function 
  set_up_env(T, N, temp_cur, num_food_mean, num_food_max)              # could be a problem that temp-cur is not known atm. 
  
  ###################################
  #   start the for loop  timesteps # 
  ###################################
  
  # Start a for loop for each timestep 
  for (t in 1:T){
    
    # Check if it is night or day 
    if ((t%%72)>=9 && (t%%72<45)){
      timeOfDay<<-1                       # this means it is day 
      temp_cur<<-temp_day
    }else{
      timeOfDay<<-0                       # this means it is night 
      temp_cur<<-temp_night
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
      }
      # if not step 1, check if bird was previously dead
      # if previously dead, it needs to be dead now 
      else if (mat_alive[i,(t-1)]==0){
        mat_alive[i,t]<<-0
      }
      # if not step 1 and not previously dead 
      # check if the bird should die now 
      else if (mat_fr[i,t]==0){
        mat_alive[i,t]<<-0
      }
      # in all other cases the bird is alive 
      else{
        mat_alive[i,t]<<-1
      }
      
      
      ################
      #  DEAD BIRDS  #
      ################
      if(mat_alive[i,t]==0){
        # these are the dead birds 
        # print(paste0(' bird ', i, 'is dead'))
      }else{
        
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
        if(timeOfDay==0){
          
          ################
          #   SLEEPING   # 
          ################
          
          # set the sleeping matrix to 1 
          sleep_count[i,t]<<-1
          # set the forage to 0
          forage_count[i, t]<<-0
          # set the resting matrix to 0
          rest_count[i,t]<<-0
          
          # set the BMR-multi
          BMR_multi<<-1
          #set the predation risk 
          Patt_cur<<-Patt_sleep
          
          # Food will be moved from the stomach
          # Into the fat reserves 
          # and be burned depending on BMR-multi
          # in the ' Everyone '  part of the code below
          
        } else{
          
          # Check what behavior the bird should do 
          
          #################
          #     FORAGE    # 
          #################
          
          # CHECK IF FORAGING 
          if (((mat_sc[i,t]) < th_forage_sc) && (mat_fr[i,t]<th_forage_fr)){
            # SET COUNTING MATRICES 
            # In this case the bird should forage
            # set the forage to 1
            forage_count[i, t]<<-1
            # set the resting matrix to 0
            rest_count[i,t]<<-0
            
            # UPDATE AGENT OWNED VARIABLES 
            # First, calculate how much food the bird finds 
            food_cur<<-rtruncnorm(1, a=0, b=gram_food_max, mean=gram_food_mean, sd=0.1)
            # now round this up/down to the closest number of items (a bird cannot find half items)
            # then move this back to grams 
            food_cur<<-((round(food_cur/food_item))*food_item)
            # Now, increase the stomach content
            mat_sc[i,(t)]<<-(mat_sc[i,t])+(food_cur)
            # now check if this doesnt exceed the stomach size 
            # if so, set the stomach content to stomach size 
            if (mat_sc[i,(t)]>stom_size){
              mat_sc[i,(t)]<<-stom_size
            }
            # Set the BMR to the right level: cost of foraging
            # BMR-multi is not a global variable: stays local with the agent
            BMR_multi<<-8
            #set the predation risk 
            Patt_cur<<-Patt_for
          } # ends the foraging statement
          
          ##################
          #    RESTING     # 
          ##################
          
          # CHECK IF RESTING 
          else{
            # SET COUNTING MATRICES 
            # in the other case the bird should rest 
            # set the forage matrix to 0
            forage_count[i,t]<<-0
            # set the rest matrix to 1
            rest_count[i,t]<<-1
            
            # SET AGENT OWNED VARIABLES 
            # Set the BMR to the right level: cost of foraging
            # BMR multi is local and stays with the agent 
            BMR_multi<<-1.95
            #set the predation risk 
            Patt_cur<<-Patt_rest
            # the stomach content stays the same (initial value)
            
          } # end resting statement 
        } # end of 'Time of day = day ' statement 
        
        ###################
        #    EVERYONE     # 
        ###################
        # No matter what behaviour you've done, these need updating for all alive birds
        
        # PREDATION 
            # first check if the bird actually survived the behaviour it did 
            mass_cur<<-mat_mass[i,t]                                            # find out the current mass of the bird 
            Pcap_cur<<-(0.78+(0.5*10^(-8*exp(1.4*mass_cur))))                   # calculate the current Pcapture 
            Pkill_cur<<-Pcap_cur*Patt_cur                                       # calculate the current Pkill 
            mat_Pkill[i,t]<<-Pkill_cur                                          # put in the matrix 
            # now check if the bird dies or not 
            Psurv_cur<-runif(1)                                                 # Random number between 0 and 1 for survival chance 
            if(Psurv_cur<Pkill_cur){                                            # if the prob for survival < prob to die 
              mat_alive[i,t]<<-0                                                # Set the matrix to 'dead' 
              predation_count[i,t]<<-1
            }
            else{
            # Surviving birds should update their values: 
                predation_count[i,t]<<-0
            # UPDATE THE FAT RESERVES 
                # SC down and FR up 
                # first check if stomach has enough to actually move
                # move food out of stomach into fat 
                if (mat_sc[i,(t)]>= stom_to_fat){
                  # new sc from resting/foraging can be used
                  mat_sc[i,(t)]<<-(mat_sc[i,(t)]-stom_to_fat)
                  # the new fat reserve has not been determined yet
                  mat_fr[i,(t)]<<-(mat_fr[i,t]+stom_to_fat)
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
                # Note that this should only happen if you're not in the last timestep 
                if(t<T){
                  # For the fr matrix 
                  mat_fr[,(t+1)]<<-mat_fr[,t]
                  # For the mass matrix 
                  mat_mass[,(t+1)]<<-mat_mass[,t]
                  # For the sc matrix 
                  mat_sc[,(t+1)]<<-mat_sc[,t]
                }
        } # end of else-stat for birds surviving predation
          
      } # end of loop for (originally) alive individuals 
      
    } # end of loop for each individual 
    
    ##########################
    #    wrap up timestep    # 
    ##########################
    
    
    # COUNT WHAT HAPPENED 
    # For each timestep, count what the birds are doing 
    # These are global now, can be changed if not necessary 
    total_forage[1,t]<<-sum(forage_count[,t], na.rm=TRUE)  # counts how many birds foraged this timestep
    total_rest[1,t]<<-sum(rest_count[,t], na.rm=TRUE)      # counts how many birds rested this timestep 
    total_alive[1,t]<<-sum(mat_alive[,t], na.rm=TRUE)      # counts how many birds are alive this timestep
    total_predated[1,]<<-sum(predation_count[,t], na.rm=TRUE) # how many birds were killed by predation in this timestep 
    
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
    
    if ((t/plot_interval)==floor(t/plot_interval) && noplot==0 ){
      par(mfrow=c(4,2))
      Sys.sleep(0.05)          # forces an update to the plotting window 
      # 1
      plot1<-plot(1:t, sc_mean[1,(1:t)], ylim=c(0,(stom_size+0.1)), ylab='Mean stomach content', xlab='timestep', main='Mean Sc', type='l')
      abline(h=stom_size, col='red')
      # 2
      plot2<-plot(1:t, fr_mean[1,(1:t)], ylim=c(0,(fat_max+0.5)), ylab='Mean fat reserve', xlab='timestep', main='Mean Fr', type='l')
      abline(h=fat_max, col='red')
      # 3
      plot3<-plot(1:t, mass_mean[1,(1:t)], ylim=c(0,(20)), ylab='Mean mass', xlab='timestep', main='Mean mass', type='l')
      # 4
      plot4<-plot(1:t, total_alive[1,(1:t)], ylim=c(0, N), ylab='Number of birds alive', xlab='Timestep', main='Number birds alive', type='l')
      # 5
      plot5<-plot(1:t, ((total_forage[1,(1:t)])/(total_alive[1,(1:t)])*100), ylim=c(0, 100), ylab='%', xlab='Timestep', main='Percentage of alive birds foraging', type='l')
      
      # Code to plot total number of birds foraging (omitted)
      #plot5<-plot(1:t, (total_forage[1,(1:t)]), ylim=c(0, N), ylab='#', xlab='Timestep', main='Number of birds foraging', type='l')
      
      # 6
      plot6<-plot(1:t, ((total_rest[1,(1:t)])/(total_alive[1,(1:t)])*100), ylim=c(0, 100), ylab='%', xlab='Timestep', main='Percentage of alive birds resting', type='l')
      
      # Code to plot total number of birds resting (omitted)
      # plot6<-plot(1:t, total_rest[1,(1:t)], ylim=c(0, N), ylab='Number of birds resting', xlab='Timestep', main='Nuber birds resting', type='l')
      
      # 7: To show predation
      plot7<-plot(1:t, (total_predated[1,(1:t)]), ylim=c(0, 100), ylab='# killed by predation', xlab='Timestep', main='Number of birds killed by predation', type='l')
      
      Sys.sleep(0)             # turns that back off 
    }# end if statement 
    
    
  } # end of big timestep loop 
  
  # create variable with the number of the last timesstep done 
  last_T<<-T
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
  
  birds_alive_at_end<<-alive_mean[last_T]
  
  # Print some text to keep track of the simulations 
  if (exists('opt_type')){
    if(opt_type=='th_sc'){
      print(paste0('rest_forage function did run for sc:', current_th_sc ))
    }
    if(opt_type=='th_fr'){
      print(paste0('rest_forage function did run for fr:', current_th_fr ))
    }
    if(opt_type=='th_sc_and_fr'){
      print(paste0('rest_forage function did run for sc:', current_th_sc ))
      print(paste0('rest_forage function did run for fr:', current_th_fr ))
    }
  }
  
  if(!exists('opt_type')){
    print(paste0('ready to save plots'))
    # SAVE LINE PLOTS 
    # only needs to happen at the end of the timeloop
    # Will also save all the parameter settings
    
    # for uni desktop 
    # dev.print(pdf, (paste0('\\\\webfolders.ncl.ac.uk@SSL/DavWWWRoot/rdw/ion02/02/smulderslab/VeraVinken/1-PHD_PROJECT/Modelling/R/Figures/rest_or_forage_or_hoard/','Plot_Rest_retrieve_eat_hoard_T=', T, '_N=', N, '_dayT=', temp_day, '_nightT=', temp_night, '_th-fr=', th_forage_fr, '_food-mean=',num_food_mean, '_foodMax=',num_food_max, '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))
    # for uni laptop 
    # dev.print(pdf, (paste0('//campus/rdw/ion02/02/smulderslab/VeraVinken/1-PHD_PROJECT/Modelling/R/Figures/rest_or_forage/','Plot_Rest_forage_T=', T, '_N=', N, '_dayT=', temp_day, '_nightT=', temp_night, '_th-fr=', th_forage_fr, '_th-sc=', th_forage_sc, '_food-mean=',num_food_mean, '_foodMax=',num_food_max, '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))
    
    # local on VERA account - works on both laptop and desktop 
    #dev.print(pdf, (paste0('//campus/home/home2019/c0070955/Vera/NCLU/1-PHD_PROJECT/Modelling/R/Figures/hoarding_model_predation/', 'Plot_Rest_forage_T=', T, '_N=', N, '_dayT=', temp_day, '_nightT=', temp_night, '_th-fr=', th_forage_fr, '_th-sc=', th_forage_sc, '_food-mean=',num_food_mean, '_foodMax=',num_food_max, '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))
    
    # Save in smulders folder 
    dev.print(pdf, (paste0('//campus/rdw/ion02/02/smulderslab/VeraVinken/1-PHD_PROJECT/Modelling/R/Figures/hoarding_model_predation/', 'Plot_Rest_forage_T=', T, '_N=', N, '_dayT=', temp_day, '_nightT=', temp_night, '_th-fr=', th_forage_fr, '_th-sc=', th_forage_sc, '_food-mean=',num_food_mean, '_foodMax=',num_food_max, '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))

    
  }
  
  
} # end the rest/forage function 

##############################
#    testing the function    # 
#############################

# Some testing values, varying the food items found 

# REST_OR_FORAGE (timesteps, individuals, daytemp, nighttemp, th_forage_sc, th_forage_fr, mean-food, max-food)

# vary with number of food found 
# rest_or_forage(50, 50, -5,-5, 0.1, 1,3,6)
# rest_or_forage(1000,100,-5,-5, 0.2,1,2,4)
# rest_or_forage(1000,100,-5, -5, 0.2,1,3,6)
# rest_or_forage(1000,100,-5, -5, 0.2,1,1,2)
# 
# # vary temperature 
# rest_or_forage(1000,100,-5, -5,0.2,1,3,6)
# rest_or_forage(1000,100,-5, -15,0.2,1,3,6) # at -15 we see some birds start to die at night
# rest_or_forage(1000,100,-5, -10,0.2,1,3,6)
# rest_or_forage(1000,100,-5, -5,0.2,1,3,6)
# rest_or_forage(1000,100,-5, -7,0.2,1,3,6)
# 
# rest_or_forage(1000,100,-15, -5,0.2,1,3,6)
# 
# # run for 30 days 
rest_or_forage(2160,100,-5,-5,0.2,1,3,6, 0)
# rest_or_forage(2160,100,-5,-5,0.2,1,2,4)
# rest_or_forage(2160,100,-5,-5,0.2,1,2.5,6)
# 
# # vary the forage sc threshold 
rest_or_forage(2160,100,-5,-5,0.2,1,3,6, 0)
# rest_or_forage(2160,100,-5,-5,0.4,1,3,6, 0)
# rest_or_forage(2160,100,-5,-5,0.3,1,3,6, 0)
rest_or_forage(2160,100,-5,-5,0.1,1,3,6, 0)

rest_or_forage(2160,100,-15,-15,0.2,3,3,6, 0)
rest_or_forage(2160,100,-15,-15,0.2,2,3,6, 0)
rest_or_forage(2160,100,-15,-15,0.2,1,3,6, 0)


###########################################
#  optimization th-sc function  visually  # 
###########################################
opt_foraging_th_sc<-function(T, N, temp_day, temp_night, th_forage_fr, num_food_mean, num_food_max, noplot, th_sc_min, th_sc_max){
  # show that optimizatio started 
  print(paste0('optimization th_sc start' ))
  
  # select optimization type 
  opt_type<<-c('th_sc')
  # creates 100 values between 0 and 0.4, evenly spaced 
  th_forage_sc<<-linspace(th_sc_min, th_sc_max, n=100)
  
  # now create a space to save the survival for each different value fo th_forage_sc 
  survival_end<<-matrix(NA, 1, length(th_forage_sc))
  
  
  for (th in 1:length(th_forage_sc)){
    
    
    # Run the rest_forage function for each th_forage_sc that you have created. 
    # keep the number of days ,individuals, day temp, night temp, fat-reserve threshold, food distributuion the same
    # determine the current threshold for each loop 
    current_th_sc<<-th_forage_sc[th]
    current_th<<-current_th_sc            # needs to have a general name for the rest-forage function printing
    # now run 
    rest_or_forage(T, N, temp_day, temp_night, current_th_sc, th_forage_fr, num_food_mean, num_food_max, noplot)
    # add to the previously created matrix
    
    
    
    # PROBLEM IS LOCATED HERE 
    survival_end[1,th]<<-birds_alive_at_end
  } # end of optimization for loop 
  
  # in the end, plot the whole thing 
  par(mfrow=c(1,1))
  #jpeg('plot_opt_forage_th_sc.jpg')
  fig_opt_forage_th_sc<<-plot(th_forage_sc, survival_end, main = paste0('Opt th_sc for:T=', T, ', N=', N, ', dayT=', temp_day, ', nightT=', temp_night, ', th-fr=', th_forage_fr, ', food-mean=',num_food_mean, ', foodMax=',num_food_max ), ylim = c(0,1) )
  fig_opt_forage_th_sc
  #dev.off()
  
  # If I want to save the figures, run: 
  # path to save figures 
  # save_path<-file.path('\\\\webfolders.ncl.ac.uk@SSL/DavWWWRoot/rdw/ion02/02/smulderslab/VeraVinken/1-PHD_PROJECT/Modelling/R/Figures/rest_or_forage', paste(fig_opt_forage_th_sc, '.jpg', sep=''))
  # jpeg(file=save_path)
  # fig_opt_forage_th_sc
  # dev.off()
  # for checking during coding 
  print(paste0('optimization th_sc function did run' ))
  
  # save the figure 
  # for uni desktop 
  # dev.print(pdf, (paste0('\\\\webfolders.ncl.ac.uk@SSL/DavWWWRoot/rdw/ion02/02/smulderslab/VeraVinken/1-PHD_PROJECT/Modelling/R/Figures/rest_or_forage_or_hoard/','Plot_Rest_retrieve_eat_hoard_T=', T, '_N=', N, '_dayT=', temp_day, '_nightT=', temp_night, '_th-fr=', th_forage_fr, '_food-mean=',num_food_mean, '_foodMax=',num_food_max, '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))
  # for uni laptop 
  # dev.print(pdf, (paste0('//campus/rdw/ion02/02/smulderslab/VeraVinken/1-PHD_PROJECT/Modelling/R/Figures/rest_or_forage/opt_th_sc/','Plot_opt_th_sc_T=', T, '_N=', N, '_dayT=', temp_day, '_nightT=', temp_night, '_th-fr=', th_forage_fr, '_food-mean=',num_food_mean, '_foodMax=',num_food_max, '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))
  # local on VERA account 
  dev.print(pdf, (paste0('//campus/home/home2019/c0070955/Vera/NCLU/1-PHD_PROJECT/Modelling/R/Figures/hoarding_model_predation/opt_th_sc/', 'Plot_opt_th_sc_T=', T, '_N=', N, '_dayT=', temp_day, '_nightT=', temp_night, '_th-fr=', th_forage_fr, '_food-mean=',num_food_mean, '_foodMax=',num_food_max, '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))
  
  # Save in smulders folder 
  dev.print(pdf, (paste0('//campus/rdw/ion02/02/smulderslab/VeraVinken/1-PHD_PROJECT/Modelling/R/Figures/hoarding_model_predation/opt_th_sc/', 'Plot_opt_th_sc_T=', T, '_N=', N, '_dayT=', temp_day, '_nightT=', temp_night, '_th-fr=', th_forage_fr, '_food-mean=',num_food_mean, '_foodMax=',num_food_max, '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))
  
  
  
} # end of optimization function 

##################################
#  testing optimization th-sc    # 
##################################

# now run the optimization function 
opt_foraging_th_sc(2160,100,-5,-5,1,3,6,1, 0, 0.4)         # full version 
opt_foraging_th_sc(360,50,-5,-5,1,3,6,1, 0, 0.4)           # 5 day version with 50 birds (quicker to run)

# What if there isnt as much food? 
opt_foraging_th_sc(2160,100,-5,-5,1,2,4,1, 0, 0.4)         # everybody dies :'( 
opt_foraging_th_sc(2160,100,-5,-5,1,2.5,5,1, 0, 0.4)       # in between result? 

# what if we vary the temperatures 
opt_foraging_th_sc(2160,100,-10,-10,1,3,6,1, 0, 0.4)       # colder
opt_foraging_th_sc(2160,100,10,10,1,3,6,1, 0, 0.4)         # warmer 
opt_foraging_th_sc(2160,100,-15,-15,1,3,6,1, 0, 0.4)       # very cold  
opt_foraging_th_sc(2160,100,-12,-12,1,3,6,1, 0, 0.4)        
# notice that when temperatures get colder, the higher thresholds (just always forage)
# dont work as well anymore 

# just out of interest: at -10 there might be something happening 
# so plot with a smaller threshold range to see
opt_foraging_th_sc(2160,100,-10,-10,1,3,6,1, 0.1, 0.3) 
opt_foraging_th_sc(2160,100,-12,-12,1,3,6,1, 0.13, 0.27) 


###########################################
#      optimize visually for th-fr        # 
###########################################
opt_foraging_th_fr<-function(T, N, temp_day, temp_night, th_forage_sc, num_food_mean, num_food_max, noplot, th_fr_min, th_fr_max){
  # show start of optimization 
  print(paste0('optimization th_fr start' ))
  # set the optimization 
  opt_type<<-'th_fr'
  # creates 100 values between 0 and 0.4, evenly spaced 
  th_forage_fr<<-linspace(th_fr_min, th_fr_max, n=100)
  
  # now create a space to save the survival for each different value fo th_forage_sc 
  survival_end<<-matrix(NA, 1, length(th_forage_fr))
  
  
  for (th in 1:length(th_forage_fr)){
    
    
    # Run the rest_forage function for each th_forage_sc that you have created. 
    # keep the number of days ,individuals, day temp, night temp, fat-reserve threshold, food distributuion the same
    # determine the current threshold for each loop 
    current_th_fr<<-th_forage_fr[th]
    
    # now run 
    rest_or_forage(T, N, temp_day, temp_night, th_forage_sc, current_th_fr, num_food_mean, num_food_max, noplot)
    # add to the previously created matrix
    
    survival_end[1,th]<<-birds_alive_at_end
  } # end of optimization for loop 
  
  # in the end, plot the whole thing 
  par(mfrow=c(1,1))
  plot(th_forage_fr, survival_end, main = paste0('Opt th_fr for:T=', T, ', N=', N, ', dayT=', temp_day, ', nightT=', temp_night, ', th-sc=', th_forage_sc, ', food-mean=',num_food_mean, ', foodMax=',num_food_max ), ylim = c(0,1) )
  
  # for checking during coding 
  print(paste0('optimization th_fr function finished' ))
  
  # save the figure 
  # for uni desktop 
  # dev.print(pdf, (paste0('\\\\webfolders.ncl.ac.uk@SSL/DavWWWRoot/rdw/ion02/02/smulderslab/VeraVinken/1-PHD_PROJECT/Modelling/R/Figures/rest_or_forage_or_hoard/','Plot_Rest_retrieve_eat_hoard_T=', T, '_N=', N, '_dayT=', temp_day, '_nightT=', temp_night, '_th-fr=', th_forage_fr, '_food-mean=',num_food_mean, '_foodMax=',num_food_max, '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))
  # for uni laptop 
  # dev.print(pdf, (paste0('//campus/rdw/ion02/02/smulderslab/VeraVinken/1-PHD_PROJECT/Modelling/R/Figures/rest_or_forage/opt_th_fr/','Plot_opt_th_fr_T=', T, '_N=', N, '_dayT=', temp_day, '_nightT=', temp_night, '_th-sc=', th_forage_sc, '_food-mean=',num_food_mean, '_foodMax=',num_food_max, '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))
  # local on VERA account 
  dev.print(pdf, (paste0('//campus/home/home2019/c0070955/Vera/NCLU/1-PHD_PROJECT/Modelling/R/Figures/hoarding_model_predation/opt_th_fr/', 'Plot_opt_th_fr_T=', T, '_N=', N, '_dayT=', temp_day, '_nightT=', temp_night, '_th-sc=', th_forage_sc, '_food-mean=',num_food_mean, '_foodMax=',num_food_max, '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))
  # Save in smulders folder 
  dev.print(pdf, (paste0('//campus/rdw/ion02/02/smulderslab/VeraVinken/1-PHD_PROJECT/Modelling/R/Figures/hoarding_model_predation/opt_th_fr/', 'Plot_opt_th_fr_T=', T, '_N=', N, '_dayT=', temp_day, '_nightT=', temp_night, '_th-sc=', th_forage_sc, '_food-mean=',num_food_mean, '_foodMax=',num_food_max, '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))
  
} # end of optimization function 

##################################
#  testing optimization th-fr    # 
##################################
opt_foraging_th_fr(2160,100,-5,-5,0.2,3,6,1, 0, 4)         # full version 
opt_foraging_th_fr(360,10,-5,-5,0.2,3,6,1, 0, 4)         # fast version 
opt_foraging_th_fr(2160,100,-5,-5,0.2,3,6,1, 0, 2)         # smaller range 

# play with temperatures a bit
opt_foraging_th_fr(2160,100,-10,-10,0.2,3,6,1, 0, 4)  
opt_foraging_th_fr(2160,100,-12,-12,0.2,3,6,1, 0, 4)  
opt_foraging_th_fr(2160,100,-15,-15,0.2,3,6,1, 0, 4) 
# again, it looks like the colder temperatures mostly prevent from foraging too much 

# how about food 
opt_foraging_th_fr(2160,100,-5,-5,0.2,2, 4,1, 0, 4) 
opt_foraging_th_fr(2160,100,-5,-5,0.2,2.5, 5,1, 0, 4) 




########################################
#   3D visual optimization fr & sc     # 
########################################
opt_th_sc_and_fr<-function(T, N, temp_day, temp_night, num_food_mean, num_food_max, noplot, th_sc_min, th_sc_max, th_fr_min, th_fr_max){
  # show that optimizatio started 
  print(paste0('optimization th_sc AND th_fr start' ))
  
  # specify optimization type 
  opt_type<<-'th_sc_and_fr'
  # creates 100 values between min and max, evenly spaced 
  th_forage_sc<<-linspace(th_sc_min, th_sc_max, n=100)
  th_forage_fr<<-linspace(th_fr_min, th_fr_max, n=100)
  
  # now create a space to save the survival for each different value fo th_forage_sc and th_forage_fr 
  survival_end<<-matrix(NA, length(th_forage_sc), length(th_forage_fr))
  
  # Outside for loop that goes through all values of forage_sc 
  for (th_sc in 1:length(th_forage_sc)){
    
    # determine the current threshold for each loop 
    current_th_sc<<-th_forage_sc[th_sc]
    
    # now run through all the possible fr values for this specific sc
    for (th_fr in 1:length(th_forage_fr)){
      # set the current fat reserve threshold 
      current_th_fr<<-th_forage_fr[th_fr]
      # run the forage or rest function: 
      rest_or_forage(T, N, temp_day, temp_night, current_th_sc, current_th_fr, num_food_mean, num_food_max, noplot)
      # add to the previously created matrix 
      survival_end[th_sc,th_fr]<<-birds_alive_at_end
    } # end of loop for fat reserve thresholds 
  } # end of loop for stomach content thesholds 
  
  # The matrix should be completely filled in now and ready to do 
  
  
  # for checking during coding 
  print(paste0('optimization th_sc_and_fr function did run' ))
  
  # plot it so you can visualise
  persp3D(z=survival_end, xlab='th_sc', ylab='th_fr', zlab='survival', main='optimal survival for th_sc and th_fr')
  
  # i want a better graphic 
  #as.numeric(survival_end)
  fig<<-plot_ly(
    x=as.numeric(th_forage_sc), 
    y=as.numeric(th_forage_fr), 
    z=survival_end
  ) %>% 
    add_surface() %>%
    layout(
      title=list(text=paste0('Optimised th_Sc and th_fr for:T=', T, ', N=', N, ', dayT=', temp_day, ', nightT=', temp_night, 'food-mean=',num_food_mean, ', foodMax=',num_food_max ), y=0.95),
      scene=list(
        xaxis=list(title= 'Threshold Sc  (gram)'),
        yaxis=list(title= 'Threshold Fr (gram)'),
        zaxis=list(title= 'Survival prob')
      )
    )
  fig
  
  # solve this later: 
  saveWidget(fig, file=(paste0('//campus/home/home2019/c0070955/Vera/NCLU/1-PHD_PROJECT/Modelling/R/Figures/rest_or_forage/opt_th_sc_and_fr/', 'Plot_opt_th_sc_and_fr_T=', T, '_N=', N, '_dayT=', temp_day, '_nightT=', temp_night, '_food-mean=',num_food_mean, '_foodMax=',num_food_max, '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.html')))
  #  saveWidget(fig, file='//campus\home\home2019\c0070955\Vera\NCLU\1-PHD_PROJECT\Modelling\R\Figures\rest_or_forage\opt_th_sc_and_fr\test.html')
  
  
  #saveWidget(fig, 'temp.html')
  
  
} # end of optimization function 

##################################
# testing optimization th-sc-fr  # 
##################################
opt_th_sc_and_fr(2160,100,-5,-5,3,6,1, 0, 0.4, 0, 4)         # full version 
opt_th_sc_and_fr(1080,100,-5,-5,3,6,1, 0, 0.4, 0, 4)           # fast version 
opt_th_sc_and_fr(360,10,-10,-10,3,6,1, 0, 0.4, 0, 4)         # fast version 
opt_th_sc_and_fr(50,10,-10,-10,3,6,1, 0, 0.4, 0, 4)          # half a day 

# what if it is a bit colder
opt_th_sc_and_fr(2160,100,-10,-10,3,6,1, 0, 0.4, 0, 4)         # colder
opt_th_sc_and_fr(2160,100,-15,-15,3,6,1, 0, 0.4, 0, 4)         # full version 

# what if we have fewer food? 
opt_th_sc_and_fr(2160,100,-5,-5,2,4,1, 0, 0.4, 0, 4)         # full version 



################################################################################
###################    MODEL 2 WITH HOARDING STARTS HERE     ###################
################################################################################

#######################################
#   Rest or Forage or Hoard function  #     
#######################################

rest_or_eat_or_eatHoard<-function(T, N, temp_day, temp_night, th_forage_sc, th_forage_fr, num_food_mean, num_food_max, noplot){
  
  # Set up the environment: run environment function 
  set_up_env(T, N, temp_cur, num_food_mean, num_food_max)              # could be a problem that temp-cur is not known atm. 
  
  ###################################
  #   start the for loop  timesteps # 
  ###################################
  
  # Start a for loop for each timestep 
  for (t in 1:T){
    
    # Check if it is night or day 
    if ((t%%72)>=start_day && (t%%72<end_day)){
      dayOrNight<<-1                       # this means it is day 
      temp_cur<<-temp_day
    }else{
      dayOrNight<<-0                       # this means it is night 
      temp_cur<<-temp_night
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
      }
      # if not step 1, check if bird was previously dead
      # if previously dead, it needs to be dead now 
      else if (mat_alive[i,(t-1)]==0){
        mat_alive[i,t]<<-0
      }
      # if not step 1 and not previously dead 
      # check if the bird should die now 
      else if (mat_fr[i,t]==0){
        mat_alive[i,t]<<-0
      }
      # in all other cases the bird is alive 
      else{
        mat_alive[i,t]<<-1
      }
      
      ################
      #  DEAD BIRDS  #
      ################
      if(mat_alive[i,t]==0){
        # these are the dead birds 
        # print(paste0(' bird ', i, 'is dead'))
      }else{
        
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
          
          # Food will be moved from the stomach
          # Into the fat reserves 
          # and be burned depending on BMR-multi
          # in the ' Everyone '  part of the code below
          
        } else{
          
          # set the sleeping matrix to 0 
          sleep_count[i,t]<<-0
          
          # Check what behavior the bird should do if it is day 
          # CHECK IF BIRD IS HUNGRY AND NEEDS FOOD
          
          if (((mat_sc[i,t]) < th_forage_sc) && (mat_fr[i,t]<th_forage_fr)){
            # If this is the case, the bird is hungry and needs to forage for food 
            
            #################
            #     FORAGE    # 
            #################
            
            # SET COUNTING MATRICES 
            # In this case the bird should forage
            # set the forage to 1
            forage_count[i, t]<<-1
            # set the resting matrix to 0
            rest_count[i,t]<<-0
            
            # WHAT KIND OF FORAGING IS HAPPENING? 
            # 3 kinds of foraging are possible: 
            #       1. Retrieve stored item 
            #       2. Find new item and eat it (done after this)
            #       3. Find new item and eat it till full, hoard after this 
            
            ###################
            #    RETRIEVE     # 
            ###################
            
            # DETERMINE CURRENT RETRIEVAL THRESHOLD 
            # What is the time of day?
            cur_timestep_in72<<-t%%72                                 # this gives the timestep within the 24 hour (72 timesteps)
            cur_timestep_inDaylight<<-(cur_timestep_in72-start_day)   # gives the timestep within the time that there is daylight 
            add_each_step<<-(0.63/(end_day-start_day))                # determines how much to add for each hour of daylight (0.63 is total adding for a day)
            # The threshold is supposed to increase from 0.12 to 0.75
            # 0.12 is the minimum needed to survive if foraging wouldnt be succesful 
            # 0.75 is the minimum needed to survive the night 
            # note that these are based on the netlogo values. Talk to tom to see if these need adapting 
            cur_th_retrieval<<-(cur_timestep_inDaylight*add_each_step)+0.12
            
            # CHECK IF BIRD IS RETRIEVING 
            if ((mat_fr[i,t]<cur_th_retrieval)&& (mat_caches[i,t]>retrieve_min)){
              
              # The bird will retrieve: update global counters
              retrieve_count[i, t]<<-1
              eat_count[i,t]<<-0
              eat_hoard_count[i,t]<<-0 
              
              # determine how many caches are retrieved
              cur_stomach_space<<-(stom_size-mat_sc[i,t])                     # What is the space left in the stomach?
              cur_caches_retrieved<<-((round(cur_stomach_space/food_item)))   # how many caches to fill this back up 
              mat_caches[i,t]<<-(mat_caches[i, (t)]-cur_caches_retrieved)     # update the number of cahches that are left 
              
              # update the stomach content 
              food_g_retrieved<<-cur_caches_retrieved*food_item               # retrieved food in grams 
              mat_sc[i,t]<<-((mat_sc[i,t])+food_g_retrieved)                  # Add the food to the stomach content 
              
              
              # set 'food_cur' to correct value in grams 
              # set new BMR multi for retriaval behaviour 
              # I need to check if this should be depending on the number of caches that are retrieved
              BMR_multi<<-8
              
            }
            
            else{
              # If the bird is foraging, but not retrieving, it will eat-hoard or eat the food 
              # Either way, it will need to find food first 
              
              # update the global counting variable
              retrieve_count[i,t]<<-0
              
              # FIND FOOD FROM NORMAL DISTRIBUTION AND DECIDE BEHAVIOUR 
              # First, calculate how much food the bird finds 
              food_g_found<<-rtruncnorm(1, a=0, b=gram_food_max, mean=gram_food_mean, sd=0.1)
              # now round this up/down to the closest number of items (a bird cannot find half items)
              # then move this back to grams 
              food_g_found<<-((round(food_g_found/food_item))*food_item)
              # Food is found, we need to check how much it is and if the bird will hoard the surpluss 
              
              # First, increase the stomach content with whatever food is found
              mat_sc[i,(t)]<<-(mat_sc[i,t])+(food_g_found)
              # now check if this exceeds the stomach size 
              if (mat_sc[i,(t)]>stom_size){
                # This means the bird found more than it can eat
                # It will hoard the surpluss 
                
                ######################
                #    EAT-HOARD       # 
                ######################
                
                # update the global counters 
                eat_hoard_count[i,t]<<-1
                eat_count[i,t]<<-0
                
                # update agent-owned variables
                hoard_surplus<<-floor((mat_sc[i,(t)]-stom_size)/food_item)  # Determine The surplus available in whole food items       
                mat_sc[i,(t)]<<-stom_size                                   # The stomach is set to the stomach size (full)
                mat_caches[i,t]<<-(mat_caches[i,t])+hoard_surplus
                
                # update BMR multi
                BMR_multi<<-8
                
              } # end of eat-hoard if-statement 
              
              else{
                # This means the food eaten does not exceed teh stomach size 
                # no hoarding required, the bird will just eat 
                
                #############
                #   EAT     # 
                #############
                
                # update the global counters 
                eat_hoard_count[i,t]<<-0
                eat_count[i,t]<<-1
                
                # Stomach content is already updated 
                
                # Update BMR multi
                BMR_multi<<-8
                
              } # end of the eat statement
              
            } # end of forage but not retrieving statement 
            
          } # ends the foraging statement
          
          # CHECK IF RESTING 
          else{
            ##################
            #    RESTING     # 
            ##################
            
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
            
          } # end resting statement 
        } # end of 'Time of day = day ' statement 
        
        
        ###################
        #    EVERYONE     # 
        ###################
        # No matter what behaviour you've done, these need updating 
        
        # UPDATE THE FAT RESERVES AND STOMACH CONTENT
        # SC down and FR up 
        # first check if stomach has enough to actually move
        # move food out of stomach into fat 
        if (mat_sc[i,(t)]>= stom_to_fat){
          # new sc from resting/foraging can be used
          mat_sc[i,(t)]<<-(mat_sc[i,(t)]-stom_to_fat)
          # the new fat reserve has not been determined yet
          mat_fr[i,(t)]<<-(mat_fr[i,t]+stom_to_fat)
        }
        else{
          mat_fr[i,t]<<-(mat_fr[i,t]+mat_sc[i,t])    # move whatever is left in the stomach to fat 
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
        if(t<T){
          # For the fr matrix 
          mat_fr[,(t+1)]<<-mat_fr[,t]
          # For the mass matrix 
          mat_mass[,(t+1)]<<-mat_mass[,t]
          # For the sc matrix 
          mat_sc[,(t+1)]<<-mat_sc[,t]
          # for the caches matrix 
          mat_caches[,(t+1)]<<-mat_caches[,t]
        }
        
      } # end of loop for alive individuals 
      
    } # end of loop for each individual 
    
    ##########################
    #    wrap up timestep    # 
    ##########################
    
    # COUNT WHAT HAPPENED 
    # For each timestep, count what the birds are doing 
    # These are global now, can be changed if not necessary 
    total_forage[1,t]<<-sum(forage_count[,t], na.rm=TRUE)             # counts how many birds foraged this timestep
    total_rest[1,t]<<-sum(rest_count[,t], na.rm=TRUE)                 # counts how many birds rested this timestep 
    total_alive[1,t]<<-sum(mat_alive[,t], na.rm=TRUE)                 # counts how many birds are alive this timestep
    total_retrieve[1,t]<<-sum(retrieve_count[,t], na.rm=TRUE)         # counts how many birds are retrieving in this timestep
    total_eat_hoard[1,t]<<-sum(eat_hoard_count[,t], na.rm=TRUE)       # counts how many birds are eat-hoarding in this timestep
    total_eat[1,t]<<-sum(eat_count[,t], na.rm=TRUE)                   # counts how many birds are eating this timestep
    
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
    
    if ((t/plot_interval)==floor(t/plot_interval) && noplot==0 ){
      par(mfrow=c(4,2))
      Sys.sleep(0.05)          # forces an update to the plotting window 
      
      # 1
      plot1<<-plot(1:t, sc_mean[1,(1:t)], ylim=c(0,(stom_size+0.1)), ylab='Mean stomach content', xlab='timestep', main='Mean Sc', type='l')
      abline(h=stom_size, col='red')
      # 2
      plot2<<-plot(1:t, fr_mean[1,(1:t)], ylim=c(0,(fat_max+0.5)), ylab='Mean fat reserve', xlab='timestep', main='Mean Fr', type='l')
      abline(h=fat_max, col='red')
      # 3
      plot3<<-plot(1:t, mass_mean[1,(1:t)], ylim=c(0,(20)), ylab='Mean mass', xlab='timestep', main='Mean mass', type='l')
      # 4
      plot4<<-plot(1:t, total_alive[1,(1:t)], ylim=c(0, N), ylab='Number of birds alive', xlab='Timestep', main='Number birds alive', type='l')
      
      # 5 foraging plot omited for now
      #plot5<-plot(1:t, ((total_forage[1,(1:t)])/(total_alive[1,(1:t)])*100), ylim=c(0, 100), ylab='%', xlab='Timestep', main='Percentage of alive birds foraging', type='l')
      
      # 5
      plot5<<-plot(1:t, ((total_eat[1,(1:t)])/(total_alive[1,(1:t)])*100), ylim=c(0, 100), ylab='%', xlab='Timestep', main='Percentage of alive birds eating', type='l')
      
      # Code to plot total number of birds foraging (omitted)
      #plot5<-plot(1:t, (total_forage[1,(1:t)]), ylim=c(0, N), ylab='#', xlab='Timestep', main='Number of birds foraging', type='l')
      
      # 6
      plot6<<-plot(1:t, ((total_rest[1,(1:t)])/(total_alive[1,(1:t)])*100), ylim=c(0, 100), ylab='%', xlab='Timestep', main='Percentage of alive birds resting', type='l')
      
      # Code to plot total number of birds resting (omitted)
      # plot6<-plot(1:t, total_rest[1,(1:t)], ylim=c(0, N), ylab='Number of birds resting', xlab='Timestep', main='Nuber birds resting', type='l')
      
      # 7
      plot7<<-plot(1:t, ((total_retrieve[1,(1:t)])/(total_alive[1,(1:t)])*100), ylim=c(0, 100), ylab='%', xlab='Timestep', main='Percentage of alive birds retrieving', type='l')
      
      # 8
      plot8<<-plot(1:t, ((total_eat_hoard[1,(1:t)])/(total_alive[1,(1:t)])*100), ylim=c(0, 100), ylab='%', xlab='Timestep', main='Percentage of alive birds eat-hoarding', type='l')
      
      mtext((paste('T=', T, '_N=', N, '_dayT=', temp_day, '_nightT=', temp_night, '_th-fr=', th_forage_fr, '_food-mean=',num_food_mean, '_foodMax=',num_food_max)), side=3, cex=0.8,line=-2, outer=TRUE)
      Sys.sleep(0)             # turns that back off 
    }# end if statement for plots
    
  } # end of big timestep loop 
  
  # create variable with the number of the last timesstep done 
  last_T<<-T
  
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
  if (exists('opt_type')){                                                      # Check if you are optimising or just running 
    if(opt_type=='th_sc'){                                                      # If so, are you optimising th_sc? 
      print(paste0('rest_forage function did run for sc:', current_th_sc ))
    }
    if(opt_type=='th_fr'){                                                      # Or are you optimising th_fr? 
      print(paste0('rest_forage function did run for fr:', current_th_fr ))
    }
    if(opt_type=='th_sc_and_fr'){                                               # or both? 
      print(paste0('rest_forage function did run for sc:', current_th_sc ))
      print(paste0('rest_forage function did run for fr:', current_th_fr ))
    }
  }
  
  if(!exists('opt_type')){
    # SAVE LINE PLOTS 
    # only needs to happen at the end of the timeloop
    # Will also save all the parameter settings
    # for uni desktop 
    # dev.print(pdf, (paste0('\\\\webfolders.ncl.ac.uk@SSL/DavWWWRoot/rdw/ion02/02/smulderslab/VeraVinken/1-PHD_PROJECT/Modelling/R/Figures/rest_or_forage_or_hoard/','Plot_Rest_retrieve_eat_hoard_T=', T, '_N=', N, '_dayT=', temp_day, '_nightT=', temp_night, '_th-fr=', th_forage_fr, '_food-mean=',num_food_mean, '_foodMax=',num_food_max, '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))
    # for uni laptop 
    # dev.print(pdf, (paste0('//campus/rdw/ion02/02/smulderslab/VeraVinken/1-PHD_PROJECT/Modelling/R/Figures/rest_or_forage_or_hoard/','Plot_Rest_retrieve_eat_hoard_T=', T, '_N=', N, '_dayT=', temp_day, '_nightT=', temp_night, '_th-fr=', th_forage_fr, '_th-sc=', th_forage_sc, '_food-mean=',num_food_mean, '_foodMax=',num_food_max, '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))
    # local on VERA account 
    
    # Save in smulders folder 
    dev.print(pdf, (paste0('//campus/rdw/ion02/02/smulderslab/VeraVinken/1-PHD_PROJECT/Modelling/R/Figures/hoarding_model_predation/', 'Plot_Rest_retrieve_eat_hoard_T=', T, '_N=', N, '_dayT=', temp_day, '_nightT=', temp_night, '_th-fr=', th_forage_fr, '_th-sc=', th_forage_sc, '_food-mean=',num_food_mean, '_foodMax=',num_food_max, '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))
    
    # save in vera folder 
    dev.print(pdf, (paste0('//campus/home/home2019/c0070955/Vera/NCLU/1-PHD_PROJECT/Modelling/R/Figures/hoarding_model_predation/', 'Plot_Rest_retrieve_eat_hoard_T=', T, '_N=', N, '_dayT=', temp_day, '_nightT=', temp_night, '_th-fr=', th_forage_fr, '_th-sc=', th_forage_sc, '_food-mean=',num_food_mean, '_foodMax=',num_food_max, '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))
    
    
    }
  
  # workign directory for uni laptop: 
  # '//campus/rdw/ion02/02/smulderslab/VeraVinken/1-PHD_PROJECT/Modelling/R/Figures/rest_or_forage_or_hoard/'
} # end the rest/forage/hoard function 

#######################################
#   Test rest/forage/hoard function   # 
####################################### 

rest_or_eat_or_eatHoard(2160, 100, -5, -5, 0.2, 1, 3, 6, 0)

# 60 days instead 
rest_or_eat_or_eatHoard(4320, 100, -5, -5, 0.2, 1, 3, 6, 0)
# colder
rest_or_eat_or_eatHoard(2160, 100, -5, -10, 0.2, 1, 3, 6, 0)
# colder
rest_or_eat_or_eatHoard(2160, 100, -10, -5, 0.2, 1, 3, 6, 0)

# colder
rest_or_eat_or_eatHoard(2160, 100, -10, -10, 0.2, 1, 3, 6, 0)

# short version 
rest_or_eat_or_eatHoard(102, 10, -5, -5, 0.2, 1, 3, 6, 0)





######################################################
#  optimization th-sc function  visually - hoarding  # 
######################################################
opt_hoarding_th_sc<-function(T, N, temp_day, temp_night, th_forage_fr, num_food_mean, num_food_max, noplot, th_sc_min, th_sc_max){
  # show that optimizatio started 
  print(paste0('opt hoarding th_sc start' ))
  
  # select optimization type 
  opt_type<<-c('th_sc')
  # creates 100 values between 0 and 0.4, evenly spaced between minimum and maximum inputs 
  th_forage_sc<<-linspace(th_sc_min, th_sc_max, n=100)
  
  # now create a space to save the survival for each different value of th_forage_sc 
  survival_end<<-matrix(NA, 1, length(th_forage_sc))
  
  for (th in 1:length(th_forage_sc)){
    # Run the rest_forage function for each th_forage_sc that you have created. 
    # keep the number of days ,individuals, day temp, night temp, fat-reserve threshold, food distribution the same
    # determine the current threshold for each loop 
    current_th_sc<<-th_forage_sc[th]
    current_th<<-current_th_sc            # needs to have a general name for the rest-forage function printing (works for both sc and fr optimisations)
    # now run 
    rest_or_forage_or_hoard(T, N, temp_day, temp_night, current_th_sc, th_forage_fr, num_food_mean, num_food_max, noplot)
    # add to the previously created matrix
    survival_end[1,th]<<-birds_alive_at_end
  } # end of optimization for loop 
  
  # in the end, plot the whole thing 
  par(mfrow=c(1,1))
  #jpeg('plot_opt_forage_th_sc.jpg')
  fig_opt_hoard_th_sc<<-plot(th_forage_sc, survival_end, main = paste0('Opt hoarding th_sc for:T=', T, ', N=', N, ', dayT=', temp_day, ', nightT=', temp_night, ', th-fr=', th_forage_fr, ', food-mean=',num_food_mean, ', foodMax=',num_food_max ), ylim = c(0,1) )
  fig_opt_hoard_th_sc
  #dev.off()
  
  # for checking during coding 
  print(paste0('opt hoard th_sc function did run' ))
  
  # save the figure 
  # for uni desktop 
  # dev.print(pdf, (paste0('\\\\webfolders.ncl.ac.uk@SSL/DavWWWRoot/rdw/ion02/02/smulderslab/VeraVinken/1-PHD_PROJECT/Modelling/R/Figures/rest_or_forage_or_hoard/','Plot_Rest_retrieve_eat_hoard_T=', T, '_N=', N, '_dayT=', temp_day, '_nightT=', temp_night, '_th-fr=', th_forage_fr, '_food-mean=',num_food_mean, '_foodMax=',num_food_max, '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))
  # for uni laptop 
  # dev.print(pdf, (paste0('//campus/rdw/ion02/02/smulderslab/VeraVinken/1-PHD_PROJECT/Modelling/R/Figures/rest_or_forage/opt_th_sc/','Plot_opt_th_sc_T=', T, '_N=', N, '_dayT=', temp_day, '_nightT=', temp_night, '_th-fr=', th_forage_fr, '_food-mean=',num_food_mean, '_foodMax=',num_food_max, '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))
  
  # saving to smulders lab folder 
  dev.print(pdf, (paste0('//campus/rdw/ion02/02/smulderslab/VeraVinken/1-PHD_PROJECT/Modelling/R/Figures/hoarding_model_predation/opt_th_sc/', 'Plot_hoard_opt_th_sc_T=', T, '_N=', N, '_dayT=', temp_day, '_nightT=', temp_night, '_th-fr=', th_forage_fr, '_food-mean=',num_food_mean, '_foodMax=',num_food_max, '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))
  # Local vera account- should work for both 
  dev.print(pdf, (paste0('//campus/home/home2019/c0070955/Vera/NCLU/1-PHD_PROJECT/Modelling/R/Figures/hoarding_model_predation/opt_th_sc/', 'Plot_hoard_opt_th_sc_T=', T, '_N=', N, '_dayT=', temp_day, '_nightT=', temp_night, '_th-fr=', th_forage_fr, '_food-mean=',num_food_mean, '_foodMax=',num_food_max, '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))
  
} # end of optimization function 

############################################
#  testing optimization th-sc - hoarding   # 
############################################

# now run the optimization function 
opt_hoarding_th_sc(2160,100,-5,-5,1,3,6,1, 0, 0.4)         # full version 
opt_hoarding_th_sc(360,50,-5,-5,1,3,6,1, 0, 0.4)           # 5 day version with 50 birds (quicker to run)

# What if there isnt as much food? 
opt_hoarding_th_sc(2160,100,-5,-5,1,2,4,1, 0, 0.4)         # mean of 2, maximum of 4
opt_hoarding_th_sc(2160,100,-5,-5,1,2.5,5,1, 0, 0.4)       # mean of 2.5, maximum of 5  

# what if we vary the temperatures 
opt_hoarding_th_sc(2160,100,-10,-10,1,3,6,1, 0, 0.4)       # colder
opt_hoarding_th_sc(2160,100,10,10,1,3,6,1, 0, 0.4)         # warmer 
opt_hoarding_th_sc(2160,100,-15,-15,1,3,6,1, 0, 0.4)       # very cold  
opt_hoarding_th_sc(2160,100,-12,-12,1,3,6,1, 0, 0.4)        
# notice that when temperatures get colder, the higher thresholds (just always forage)
# dont work as well anymore 


##################################################
#      optimize visually for th-fr - hoarding    # 
##################################################

opt_hoarding_th_fr<-function(T, N, temp_day, temp_night, th_forage_sc, num_food_mean, num_food_max, noplot, th_fr_min, th_fr_max){
  # show start of optimization 
  print(paste0('opt hoarding th_fr start' ))
  # set the optimization 
  opt_type<<-'th_fr'
  # creates 100 values between 0 and 0.4, evenly spaced 
  th_forage_fr<<-linspace(th_fr_min, th_fr_max, n=100)
  # now create a space to save the survival for each different value fo th_forage_sc 
  survival_end<<-matrix(NA, 1, length(th_forage_fr))
  
  for (th in 1:length(th_forage_fr)){
    # Run the rest_forage function for each th_forage_sc that you have created. 
    # keep the number of days ,individuals, day temp, night temp, fat-reserve threshold, food distributuion the same
    # determine the current threshold for each loop 
    current_th_fr<<-th_forage_fr[th]
    # now run 
    rest_or_forage_or_hoard(T, N, temp_day, temp_night, th_forage_sc, current_th_fr, num_food_mean, num_food_max, noplot)
    # add to the previously created matrix
    survival_end[1,th]<<-birds_alive_at_end
  } # end of optimization for loop 
  
  # in the end, plot the whole thing 
  par(mfrow=c(1,1))
  plot(th_forage_fr, survival_end, main = paste0('Opt hoarding th_fr for:T=', T, ', N=', N, ', dayT=', temp_day, ', nightT=', temp_night, ', th-sc=', th_forage_sc, ', food-mean=',num_food_mean, ', foodMax=',num_food_max ), ylim = c(0,1) )
  
  # for checking during coding 
  print(paste0('opt hoarding th_fr function finished' ))
  
  # save the figure 
  # for uni desktop 
  # dev.print(pdf, (paste0('\\\\webfolders.ncl.ac.uk@SSL/DavWWWRoot/rdw/ion02/02/smulderslab/VeraVinken/1-PHD_PROJECT/Modelling/R/Figures/rest_or_forage_or_hoard/','Plot_Rest_retrieve_eat_hoard_T=', T, '_N=', N, '_dayT=', temp_day, '_nightT=', temp_night, '_th-fr=', th_forage_fr, '_food-mean=',num_food_mean, '_foodMax=',num_food_max, '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))
  # for uni laptop 
  # dev.print(pdf, (paste0('//campus/rdw/ion02/02/smulderslab/VeraVinken/1-PHD_PROJECT/Modelling/R/Figures/rest_or_forage/opt_th_fr/','Plot_opt_th_fr_T=', T, '_N=', N, '_dayT=', temp_day, '_nightT=', temp_night, '_th-sc=', th_forage_sc, '_food-mean=',num_food_mean, '_foodMax=',num_food_max, '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))
  
  # local on VERA account 
  dev.print(pdf, (paste0('//campus/home/home2019/c0070955/Vera/NCLU/1-PHD_PROJECT/Modelling/R/Figures/hoarding_model_predation/opt_th_fr/', 'Plot_opt_th_fr_T=', T, '_N=', N, '_dayT=', temp_day, '_nightT=', temp_night, '_th-sc=', th_forage_sc, '_food-mean=',num_food_mean, '_foodMax=',num_food_max, '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))
  # To smulders labfolder 
  dev.print(pdf, (paste0('//campus/rdw/ion02/02/smulderslab/VeraVinken/1-PHD_PROJECT/Modelling/R/Figures/hoarding_model_predation/opt_th_fr/', 'Plot_opt_th_fr_T=', T, '_N=', N, '_dayT=', temp_day, '_nightT=', temp_night, '_th-sc=', th_forage_sc, '_food-mean=',num_food_mean, '_foodMax=',num_food_max, '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))
} # end of optimisation th_fr for hoarding model 

###########################################
#  testing optimization th-fr - hoarding  # 
###########################################
opt_hoarding_th_fr(2160,100,-5,-5,0.2,3,6,1, 0, 4)          # full version 
opt_hoarding_th_fr(360,10,-5,-5,0.2,3,6,1, 0, 4)            # fast version 

# play with temperatures a bit
opt_hoarding_th_fr(2160,100,-10,-10,0.2,3,6,1, 0, 4)  
opt_hoarding_th_fr(2160,100,-12,-12,0.2,3,6,1, 0, 4)  
opt_hoarding_th_fr(2160,100,-15,-15,0.2,3,6,1, 0, 4) 
# again, it looks like the colder temperatures mostly prevent from foraging too much 

# how about food 
opt_hoarding_th_fr(2160,100,-5,-5,0.2,2, 4,1, 0, 4) 
opt_hoarding_th_fr(2160,100,-5,-5,0.2,2.5, 5,1, 0, 4) 

##################################################
#   3D visual optimization fr & sc - hoarding    # 
##################################################
opt_hoarding_th_sc_and_fr<-function(T, N, temp_day, temp_night, num_food_mean, num_food_max, noplot, th_sc_min, th_sc_max, th_fr_min, th_fr_max){
  # show that optimizatio started 
  print(paste0('opt hoarding th_sc AND th_fr start' ))
  # specify optimization type 
  opt_type<<-'th_sc_and_fr'
  # creates 100 values between min and max, evenly spaced 
  th_forage_sc<<-linspace(th_sc_min, th_sc_max, n=100)
  th_forage_fr<<-linspace(th_fr_min, th_fr_max, n=100)
  # now create a space to save the survival for each different value fo th_forage_sc and th_forage_fr 
  survival_end<<-matrix(NA, length(th_forage_sc), length(th_forage_fr))
  
  
  for (th_sc in 1:length(th_forage_sc)){          # Outside for loop that goes through all values of forage_sc 
    # determine the current threshold for each loop 
    current_th_sc<<-th_forage_sc[th_sc]
    # now run through all the possible fr values for this specific sc
    for (th_fr in 1:length(th_forage_fr)){
      # set the current fat reserve threshold 
      current_th_fr<<-th_forage_fr[th_fr]
      # run the forage or rest function: 
      rest_or_forage_or_hoard(T, N, temp_day, temp_night, current_th_sc, current_th_fr, num_food_mean, num_food_max, noplot)
      # add to the previously created matrix 
      survival_end[th_sc,th_fr]<<-birds_alive_at_end
    } # end of loop for fat reserve thresholds 
  } # end of loop for sc thesholds 
  
  # The matrix should be completely filled in now and ready to do 
  # for checking during coding 
  print(paste0('opt hoarding th_sc_and_fr function did run' ))
  # plot it so you can visualise
  persp3D(z=survival_end, xlab='th_sc', ylab='th_fr', zlab='survival', main='optimal survival for th_sc and th_fr: hoarding bird')
  # I want a better graphic 
  #as.numeric(survival_end)
  fig<<-plot_ly(
    x=as.numeric(th_forage_sc), 
    y=as.numeric(th_forage_fr), 
    z=survival_end
  ) %>% 
    add_surface() %>%
    layout(
      title=list(text=paste0('Opt hoarding th_Sc and th_fr for:T=', T, ', N=', N, ', dayT=', temp_day, ', nightT=', temp_night, 'food-mean=',num_food_mean, ', foodMax=',num_food_max ), y=0.95),
      scene=list(
        xaxis=list(title= 'Threshold Sc  (gram)'),
        yaxis=list(title= 'Threshold Fr (gram)'),
        zaxis=list(title= 'Survival prob')
      )
    )
  fig
  
  # Save the image 
  # Directory for 'vera' not the smulderslab folder 
  # This one works on both laptop and desktop 
  saveWidget(fig, file=(paste0('//campus/home/home2019/c0070955/Vera/NCLU/1-PHD_PROJECT/Modelling/R/Figures/hoarding_model_predation/opt_th_sc_and_fr/', 'Plot_opt_th_sc_and_fr_T=', T, '_N=', N, '_dayT=', temp_day, '_nightT=', temp_night, '_food-mean=',num_food_mean, '_foodMax=',num_food_max, '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.html')))
  # to the smulderslab folder 
  saveWidget(fig, file=(paste0('//campus/rdw/ion02/02/smulderslab/VeraVinken/1-PHD_PROJECT/Modelling/R/Figures/hoarding_model_predation/opt_th_sc_and_fr/', 'Plot_opt_th_sc_and_fr_T=', T, '_N=', N, '_dayT=', temp_day, '_nightT=', temp_night, '_food-mean=',num_food_mean, '_foodMax=',num_food_max, '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.html')))

  
  
  
} # end of optimization function for hoarding bird th-sc and th-fr

#############################################
# testing optimization th-sc-fr - hoarding  # 
#############################################
opt_hoarding_th_sc_and_fr(2160,100,-5,-5,3,6,1, 0, 0.4, 0, 4)         # full version 
opt_hoarding_th_sc_and_fr(1080,100,-5,-5,3,6,1, 0, 0.4, 0, 4)         # fast version 
opt_hoarding_th_sc_and_fr(360,10,-10,-10,3,6,1, 0, 0.4, 0, 4)         # faster version 

# what if it is a bit colder
opt_hoarding_th_sc_and_fr(2160,100,-10,-10,3,6,1, 0, 0.4, 0, 4)         # colder
opt_hoarding_th_sc_and_fr(2160,100,-15,-15,3,6,1, 0, 0.4, 0, 4)         # more colder 

# what if we have fewer food? 
opt_hoarding_th_sc_and_fr(2160,100,-5,-5,2,4,1, 0, 0.4, 0, 4)         # mean 2 max 4 food items 





