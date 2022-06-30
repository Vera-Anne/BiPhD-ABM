#################################
# Small bird in winter - ABM 
# Start date: 20/06/2022
# Vera Vinken 
#################################

##    description     ##

# Simple model where birds live in an environment specified below 
# each timestep birds either forage or rest
# This decision is based on the stomach content (below or above threshold)
# foraging is rewarded with food, but costs energy
# resting as a lower energy expenditure 
# For each timestep the behaviours, variables and number of birds alive is registered 



##    Steps for next model      ## 

# the plot interval is not working properly 
# Sort whatever is not working in the final histograms when birds have died 
# put predation in 
# add conversion factor from stomach to fat for 'everyone' part of alive birds durning the day


##    addressed in this version  ## 

# Some changes in the final histograms 

##############################
#      load packages         #
##############################
library(usethis)
library(devtools)
# install_github("olafmersmann/truncnorm")
library(truncnorm)

##############################
#       input parameters    # 
##############################

# Use these if not running as function 

T<-100              # number of timesteps
N<-100              # number of individuals
temp<-(-5)          # Temperature 
th_forage_sc<-0.2   # threshold: if SC below this you forage 
th_forage_fr<-1     # threshold: if Fr below this you forage (AND above is true)
num_food<-1         # number of food items found (this should be a distribution)



##################################
#  set-up environment function   #
##################################

set_up_env<-function(T,N, temp_cur, num_food_mean, num_food_max){
  
  # Want to plot some initial value graphs? 
  # 1 for yes, 0 for no 
  plot_init_value<<-1 
  
  
   # Set up some parameters (Global)
  food_item<<-0.064    # value of a food item 
  stom_size<<-0.4      # stomach size of the bird 
  stom_to_fat<<-0.132  # variable that determines how many grams of sc go to fat
  fat_max<<-4          # maximum fat reserve in gram (Pravosudov & Lucas, 2001)
  plot_interval<<-10   # every x timestep a dot on the graph is added 
  start_day<<-27      # set the start of the day. Example: 27/3= 9.00 am 
  end_day<<-45        # set the end of the day.   Example: 45/3= 15.00 
  
  # Do some calculations for food distributions: 
  gram_food_mean<<-num_food_mean*food_item        # Sets the grams of fat found on average per time step
  gram_food_max<<-num_food_max*food_item          # sets the maximum grams of fat found per time step 
  
  # create individual matrices (Global)
  mat_alive<<-matrix(NA, N, (T))           # matrix to keep track of who's alive 
  mat_sc<<-matrix(NA, N, (T))              # matrix to keep track of stomach contents
  mat_fr<<-matrix(NA, N, (T))              # matrix to keep track of fat reserves 
  mat_mass<<-matrix(NA,N,(T))              # matrix to keep track of mass of birds 
  
  
  # fill in some initial values for agent variables  (global)
  mass_init<<-8+(rtruncnorm(N, a=0.01, b=0.2, mean=0.1, sd=0.01))             # Gives initial mass from normal distribution (Polo et al. 2007)
  sc_init<<-0+(rtruncnorm(N, a=0, b=stom_size, mean=(stom_size/2), sd=0.01))  # gives initial stomach content from equal distribution
  fr_init<<-0+(rtruncnorm(N, a=0, b=fat_max, mean=(fat_max/2), sd=1))         # gives initial fat reserves for random number between 0-4
  alive_init<<-rep(1, N )                                                     # all birds are alive at the start 
  # Put these in first column of the matrices  
  mat_alive[,1]<<-alive_init
  mat_sc[,1]<<-sc_init
  mat_fr[,1]<<-fr_init
  mat_mass[,1]<<-mass_init
  
  # Create empty matrices to keep track of numbers (Global)
  # keep track of means 
  sc_mean<<-matrix(NA, 1, (T))
  fr_mean<<-matrix(NA, 1, (T))
  mass_mean<<-matrix(NA,1, (T))
  alive_mean<<-matrix(NA,1, T)
  # count what the birds are up to (Global)
  forage_count<<-matrix(NA, N, (T))
  rest_count<<-matrix(NA, N, (T))
  sleep_count<<-matrix(NA, N,T)
  # total number of birds doing behaviours (Global)
  total_forage<<-matrix(NA, 1, T)                  # total number of birds foraging each timestep
  total_rest<<-matrix(NA,1, T)                     # total number of birds resting each timestep 
  total_alive<<-matrix(NA,1,T)                     # total number of birds alive each timestep 
  
  # Set up the food distribution 
  # this doesnt work cause it gets negative values
  # food_distr<-rnorm(100, mean=0.192, sd=0.4)
  
  # set metabolic rates (function )
  
  mr_function<<-function(temp_cur){                       # see pravosudov & lucas (2001) and Lucas & Walter (1991) for details 
    mr_cur<<-45.65-(1.33*temp_cur)
  }
  
  bmr_function<<-function(mr_cur, mass_cur){          # see pravosudov & lucas (2001) and Lucas & Walter (1991) for details
    bmr_cur<<-0.00616*mr_cur*((mass_cur/1000)^0.66)    # please note that the 0.00616 is for 20 min intervals 
  }
  

} # end set-up function 


###############################
#   Rest or Forage function   #     # previously : Go function in Netlogo
###############################

rest_or_forage<-function(T, N, temp_day, temp_night, th_forage_sc, th_forage_fr, num_food_mean, num_food_max){

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
          # the stomach content stays the same (initial value)
          
        } # end resting statement 
        } # end of 'Time of day = day ' statement 
        
        
        ###################
        #    EVERYONE     # 
        ###################
        # No matter what behaviour you've done, these need updating 
        
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
        # Note that this should only happen if youre not in the last timestep 
        if(t<T){
          # For the fr matrix 
          mat_fr[,(t+1)]<<-mat_fr[,t]
          # For the mass matrix 
          mat_mass[,(t+1)]<<-mat_mass[,t]
          # For the sc matrix 
          mat_sc[,(t+1)]<<-mat_sc[,t]
        }
        
      } # end of loop for alive individuals 
      
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
    
    if ((t/plot_interval)==floor(t/plot_interval)){
      par(mfrow=c(3,2))
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
  

} # end the rest/forage function 


##############################
#    testing the function    # 
#############################

# Some testing values, varying the food items found 

# REST_OR_FORAGE (timesteps, individuals, daytemp, nighttemp, th_forage_sc, th_forage_fr, number food found)

# vary with number of food found 
rest_or_forage(50, 50, -5,-5, 0.1, 1,3,6)
rest_or_forage(1000,100,-5,-5, 0.2,1,2,4)
rest_or_forage(1000,100,-5, -5, 0.2,1,3,6)
rest_or_forage(1000,100,-5, -5, 0.2,1,1,2)

# vary temperature 
rest_or_forage(1000,100,-5, -50,0.2,1,3,6)
rest_or_forage(1000,100,-5, -15,0.2,1,3,6) # at -15 we see some birds start to die at night
rest_or_forage(1000,100,-5, -10,0.2,1,3,6)
rest_or_forage(1000,100,-5, -5,0.2,1,3,6)
rest_or_forage(1000,100,-5, -7,0.2,1,3,6)

rest_or_forage(1000,100,-15, -5,0.2,1,3,6)

# run for 30 days 
rest_or_forage(2160,100,-5,-5,0.2,1,3,6)

