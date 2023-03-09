#################################
# Small bird in winter - ABM 
# Start date: 23/09/2022
# Vera Vinken 

#################################

# 23-03-03 Test if I can push from here (new laptop)  16.16
# 23-03-07 Test again :'(  
# push 23-03-09 


# now push from desktop and test if it works with  the laptop 


##    Description     ##
    
    # Original copied from 'hoarding_model_predation' on 23/10/2022 
    

##    Steps for next model      ## 

    # cost of retrieval and foraging could rely on the number of items found 
    # When rounding up/down the food items to grams and the other way around. should ceiling/floor be used? or the 'neutral' way as it is now? 
    # Check if the initial caches distributuion makes sense 
    # Why does stomach content not drop to 0? 
    # fix the bmr multi for retrieval 
    # check if all the bmr funxtions are correct 
    # do we need it to run for 30 days before doing anything? 
    # what about pilferage ? 

# TO DO URGENT 
    
    # - 

##    addressed in this version  ## 
    
    # all models are tested. Ready to run larger simulations 



##############################
#      load packages         #
##############################
load_packages<-function(){
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
library(hrbrthemes)
}

#run 
load_packages()


############################# 
#   set up directories      # 
#############################

# Set up the main directory for where you want the figures saved 
# This can be replaced by any folder you have on your computer (just make sure you have continuous connection if its a webfolder)
mainDir<-'C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Figures/5-combi_model'
setwd(mainDir)

# Run the following if doing this for the first time on devide: 
# create list of folders that we want present 
folders<-c('MOD_1_1', 'MOD_1_2', 'MOD_1_3', 'MOD_1_4')
# Check if they exist and if not, create them 
# Note that this code will warn you if it already existed 
for (folder in folders){
  dir.create(file.path(mainDir, folder ), showWarnings = TRUE)
}

##############################
#       input parameters    # 
##############################
# Use these if not running as function / debugging
# It fills in some variables if you are testing code without running full functions
fill_vars<-function(){
  TS<<-100              # number of timesteps
  N<<-100              # number of individuals
  temp<<-(-5)          # Temperature
  temp_day<<--5
  temp_night<<--5
  th_forage_sc<<-0.2   # threshold: if SC below this you forage
  th_forage_fr<<-1     # threshold: if Fr below this you forage (AND above is true)
  num_food<<-1         # number of food items found (this should be a distribution)
  num_food_max<<-6
  num_food_mean<<-3
  num_cache_min<<-50
  num_cache_max<<-100
  noplot<<-1
  hoard_on<<-1
}


#################################
#     Temperature function      #
#################################

temp_func<-function(TS, Tmax_range_low, Tmax_range_high, Tmin_range_low, Tmin_range_high, days, daylight_h, n_daylight_timestep){
  # This function is meant to create a complete temperature profile for the entire simulation
  # Will be ran inside the 'set up environment' function
  # Every timestep within the simulation will pull the 'current temperature' from the vector that this function creates
  

  # As we will set the minimum at the end of the day/start of the day
  # we can set the following before the for-loop: 
  minTemp_time_end<<-72
  
  # TEMPERATURE VECTORS FOR MIN AND MAX EACH DAY 
  
  #### CHECK IF THIS IS WHAT WE WANT ### 
  max_temp_vect<<-sample(seq(Tmax_range_low, Tmax_range_high, length=(days)))
  min_temp_vect<<-sample(seq(Tmin_range_low, Tmin_range_high, length=(days)))
  #plot(max_temp_vect, min_temp_vect)
  #now we have a temperature set for each day 
  
  
  # create an empty list where the day-profiles are going to go 
  total_temp_profile<<-c()
  
  
  # START THE FOR LOOP FOR EACH DAY 
  # Now for every day, we need to create a temperature PROFILE 
  for (i in (1:days)){
    
    # establish the current max and min for each loop (day)
    cur_Tmax<<-max_temp_vect[i]
    cur_Tmin<<-min_temp_vect[i]
    
    # CREATE LINE FROM MAX TO MIN 
    # This is the code for 75% 
    cur_maxT_time<<-round((n_daylight_timestep*0.75))
    # How far ar the max and min apart in time? 
    d_time_maxToMin<<-minTemp_time_end-cur_maxT_time
    # how far are they apart in temperature? 
    d_temp_maxToMin<<-cur_Tmax-cur_Tmin
    # Step per timestep: 
    temp_steps_maxToMin<<-d_temp_maxToMin/d_time_maxToMin
    #Now generate a vector with a temperature for each timestep
    max_to_min_vector<<-seq(cur_Tmax,cur_Tmin, by=-temp_steps_maxToMin)   # notice the '-' because we're going down in temp
    
    
    # NOW IT WILL SPLIT UP FOR DAY 1, DAY-END VS ALL THE OTHER DAYS
    # Decide what the previous and minimum is 
    
    if (i==1){
      
      # just set them to the same ones as current? 
      prev_tempMin<<-min_temp_vect[i]
      
    } else if (i==(length(max_temp_vect))){
      
      # just set them to the same ones as current? 
      prev_tempMin<<-min_temp_vect[i]
    } else {
      
      # previous day minimum 
      prev_tempMin<<-min_temp_vect[(i-1)]
      
    } # else for 'normal' days ends (excludes day 1 and the last day )
    
    
    # LINE FROM MIN TO MAX 
    
    # how far are they apart in temperature? 
    d_temp_before<<-cur_Tmax-prev_tempMin
    # Step per timestep: 
    temp_steps_before<-d_temp_before/cur_maxT_time
    #Now generate a vector with a temperature for each timestep
    before_max_vector<<-seq(prev_tempMin,cur_Tmax, by=temp_steps_before)
    # Here we start at min and end at max, so we cut of both min and max (they are include din the other vector)
    before_max_vector<<-before_max_vector[2:(length(before_max_vector)-1)]
    
    # ATTACH THE TWO LINES 
    # Glue the 3 vectors together 
    cur_day_temp_vector<<-c(before_max_vector, max_to_min_vector)
    
    # just print a day profile 
    # if (i==15){
    #   timesteps_day<-1:72
    #   yellow<-adjustcolor( 'lightgoldenrod', alpha.f = 0.5)
    #   blue<-adjustcolor('skyblue', alpha.f=0.5)
    #   plot(timesteps_day, cur_day_temp_vector, main=(paste('Temp profile for day', i, 'max-range=', Tmax_range_low, 'to', Tmax_range_high, ', min-range=', Tmin_range_low, 'to', Tmin_range_high)), type='l', col='red', ylab='Temp in degrees C', xlab='Timesteps in 20 min intervals')
    #   abline(v=n_daylight_timestep)
    #   polygon(x=c(0,0, n_daylight_timestep, n_daylight_timestep), y=c((min(cur_day_temp_vector)-2), (max(cur_day_temp_vector)+2), (max(cur_day_temp_vector)+2), (min(cur_day_temp_vector)-2)), col=yellow, border=F)
    #   polygon(x=c(n_daylight_timestep, n_daylight_timestep, (length(timesteps_day)), (length(timesteps_day))), y=c((min(cur_day_temp_vector)-2), (max(cur_day_temp_vector)+2), (max(cur_day_temp_vector)+2), ( min(cur_day_temp_vector)-2)), col=blue, border=F)
    #   print(paste('minimum temp =',cur_Tmin))
    #   print(paste('minimum temp prev day=',prev_tempMin))
    #   print(cur_day_temp_vector)
    #   
    # }
    # 
    
    # ATTACH THE CURRENT DAY PROFILE TO THE BIG ONE 
    # Ok cool, I have a vector that has the temperatures for every day. 
    # Now save them somewhere so they can all be amended 
    total_temp_profile<<-c(total_temp_profile, cur_day_temp_vector)
    
  } # for loop per day ends 
  
  
} # temp function ends 

# test it 
#temp_func(TS=2160, Tmax_range_low=2, Tmax_range_high=5, Tmin_range_low=-5, Tmin_range_high = -2, days=30, daylight_h = 6, n_daylight_timestep = 18)



#################################
#        Forage function        #
#################################

# For testing the function


# function 
forage_function<-function(num_food_mean, prob_b_forage, b_size){
  # First decide if you're going into bonanza chances or normal foraging 
  # First decide if you're going into bonanza chances or normal foraging 
  
  # The following code takes a sample of of size 1 of the elements 'forage b' and 'forage n'
  # Samplign should be with replacement
  # probw is a vector of the probability weights for obtaining the elements being sampled (forage b and forage-n)
  cur_forage_type<<-sample(c('forage-b', 'forage-n'), size=1, replace=TRUE, prob=c(prob_b_forage, (1-prob_b_forage)))
  
  # Code to draw a piechart for testing the 'pick foraging type' code 
        # prob_b_forage<-0.5
        # temp_list<-list()
        # for (i in 1:1000){
        #   cur_forage_type<<-sample(c('forage-b', 'forage-n'), size=1, replace=TRUE, prob=c(prob_b_forage, (1-prob_b_forage)))
        #   temp_list<<-append(temp_list, cur_forage_type)
        # 
        # }
        # df_for <- data.frame(do.call("rbind",temp_list)) #combine all vectors into a matrix
        # table_for<-data.frame(table(df_for$do.call..rbind...temp_list.))
        # 
        # pie<-ggplot(table_for, aes(x="", y=Freq, fill=Var1))+
        #   geom_bar(stat='identity', width=1, color='white')+
        #   coord_polar('y', start=0)+
        #   theme_void()+
        #   #geom_text(aes(y=ypos, label=Var1), color= 'white', size = 6)+
        #   scale_fill_brewer(palette='Set3')+
        #   ggtitle(label=paste('Prob B-forage =',prob_b_forage, ', #b-for = ', table_for[1,2], ', #n-for = ', table_for[2,2]))
        # pie

  
  if (cur_forage_type=='forage-b'){ 
    # Write code for bonanza here
    # we know the mean
    # set the size of your bonanza (should be input)
    # calculate the amount of times you need to find 0
    # pull a number from that distribution
    # calculate the number of times 'nothing' is found given the bonanza size and teh mean
    num_zero<<-round(((b_size-num_food_mean)/num_food_mean))
    # string of options
    item_options<<-c(b_size, (rep(0, num_zero)))
    # pick an item from these randomly
    food_item_found<<-sample(item_options,1)
    #print(paste('Bonanza foraging with ', food_item_found, 'items found'))
    
  } else {
    # Write code for normal foraging here 
    # Can be the poisson distributuion 
    # pull a number from the poisson 
    food_item_found<<-sample(rpois(100, num_food_mean),1)
    #print(paste('Normal foraging with', food_item_found, 'items found'))
    
  }
  
} # end of the foraging function 


# Quick test of the function 
######
# This code was used to check if the means are correct and if the distributions look like what they're supposed to. 
    # 
    # for (l in 1:1000){
    #   if (l==1){
    #     big_list<<-list()
    #   }
    # 
    #     for (k in 1:1000){
    #         if (k==1){
    #           forage_list_temp<<-list()
    #         }
    #         forage_function(num_food_mean = 3, prob_b_forage = 0.5, b_size = 24)
    #         forage_list_temp<<-append(forage_list_temp, food_item_found)
    # 
    #       }
    # 
    #       forage_df<-data.frame(t(data.frame(forage_list_temp)))
    #       colnames(forage_df) <- c("items")
    #       forage_table<-data.frame(table(forage_df))
    #       # calulcate mean in practice
    #       cur_mean<<-mean(forage_df$items)
    # 
    #       # plot it for an image
    #       forage_hist<<- ggplot(forage_table) +
    #         geom_bar(aes(x=items, y=Freq, fill=TRUE), stat="identity")+
    #         scale_fill_brewer(palette='Set3')+
    #         ylim (0, 400)+
    #         ggtitle(label = paste('Frequency of Number of Items found (1000 samples), mean = ', cur_mean ))
    #       forage_hist
    #       big_list<<-append(big_list, cur_mean)
    # 
    # 
    # }
    # 
    # # Check what the mean is over 1000 runs
    # big_list<-data.frame(big_list)
    # big_list<<-t(big_list)
    # mean<-mean(big_list)
    # print(paste('The mean for 1000 runs of 1000 samples = ', mean))


##################################
#  set-up environment function   #
##################################

set_up_env<-function(days,N, env_type, daylight_h){
  
  
  # PLOTTING PARAMETERS 
  # Want to plot some initial value graphs? 
  # 1 for yes, 0 for no 
  plot_init_value<<-0
  plot_interval<<-100   # every x timestep a dot on the graph is added 
  # Sets you up for a 6 hour day 
  
  # BIRD PARAMETERS 
  stom_size<<-0.4      # stomach size of the bird 
  stom_to_fat<<-0.132  # variable that determines how many grams of sc go to fat
  fat_max<<-4          # maximum fat reserve in gram (Pravosudov & Lucas, 2001)
  
  # set metabolic rates (create the functions)
  mr_function<<-function(temp_cur){                 # see pravosudov & lucas (2001) and Lucas & Walter (1991) for details 
    mr_cur<<-45.65-(1.33*temp_cur)
  }
  bmr_function<<-function(mr_cur, mass_cur){        # see pravosudov & lucas (2001) and Lucas & Walter (1991) for details
    bmr_cur<<-0.00616*mr_cur*((mass_cur/1000)^0.66)   # please note that the 0.00616 is for 20 min intervals 
  }
  
  
  # TIMINGS SET UP 
  # how many timesteps do we have? (total over the days)
  TS<<-(days*72)
  # Of every day, how many timesteps of daylight do we have 
  n_daylight_timestep<<-(daylight_h*3)              # translates this into timesteps 
  
  # For the grpahs after optimization (day by day)
  daylight_h<<-daylight_h
  
  # debugging 
  #print('just before Temp func')
  
  #  TEMPERATURES 
  # You know the environment type 
  # Set the temperatures accordingly 
  if (env_type==1|env_type==4|env_type==7|env_type==10|env_type==13|env_type==16){
    # low temperatures 
    Tmax_range_low<<- -13
    Tmax_range_high<<- -7
    Tmin_range_low<<- -23
    Tmin_range_high<<- -17
  } else if (env_type==2|env_type==5|env_type==8|env_type==11|env_type==14|env_type==17){
    # mid temperatures 
    Tmax_range_low<<- -3
    Tmax_range_high<<- 3
    Tmin_range_low<<- -13
    Tmin_range_high<<- -7
  } else{
    # high temperatures 
    Tmax_range_low<<- 7
    Tmax_range_high<<- 13
    Tmin_range_low<<- -3
    Tmin_range_high<<- 3
  }
  # Run hte temperature function 
  temp_func(TS, Tmax_range_low, Tmax_range_high, Tmin_range_low, Tmin_range_high, days, daylight_h, n_daylight_timestep)
  
  # The output of the temperature function is a variable called 'total_temp_profile', which is just a vector with a temperature for each timestep 
  # This can later be used for the current temp determination 
  # debuggin 
  #print('temp func did run ')
  
  # CACHES / FOOD DISTRIBUTIONS SET UP 
  food_item<<-0.064    # value of a food item 
  b_size<<-24          # Size of a food bonanza 
  
  num_cache_min<<-50  # minimum number of caches that each bird has initially 
  num_cache_max<<-100 # maximum number of caches each bird has initially 
  retrieve_min<<-5    # minimum number of caches needed to make retrieval worth it 
  
  # Do some calculations for food distributions: 
  # Took these out as the forage-function was implemented 12/12/2022
  #gram_food_mean<<-num_food_mean*food_item        # Sets the grams of fat found on average per time step
  #gram_food_max<<-num_food_max*food_item          # sets the maximum grams of fat found per time step 
  #food_sd<<-0.3
  
  # You know the environment types
  # Set the food distributions accordingly 
  if(env_type<4){
    # Low food and poisson distributions 
    num_food_mean<<-2
    prob_b_forage<<-0
  } else if (env_type>3 && env_type<7){
    num_food_mean<<-2
    prob_b_forage<<-0.5
  } else if (env_type>6 && env_type<10){
    num_food_mean<<-3
    prob_b_forage<<-0
  } else if (env_type>9 && env_type<13){
    num_food_mean<<-3
    prob_b_forage<<-0.5
  } else if (env_type>12 && env_type<16){
    num_food_mean<<-4
    prob_b_forage<<-0
  } else if (env_type>15){
    num_food_mean<<-4
    prob_b_forage<<-0.5
  } else{
    print('something is wrong with the env -food settings')
  }
  # PREDATION  PARAMETERS 
  # Pattack: 
  # In the current version Pattack for rest and sleep is 0 
  # Pattack for any foraging behavior (retrieve, eat-hoard, eat) 
  # This is as it is set in Pravosudov and Lucas 2001, citing Lima 1986
  Patt_for<<-0.000667
  Patt_sleep<<-0
  Patt_rest<<-0
  
  
  # MATRICES 
  # create individual matrices (Global)
  mat_alive<<-matrix(NA, N, TS)            # matrix to keep track of who's alive 
  mat_sc<<-matrix(NA, N, TS)               # matrix to keep track of stomach contents
  mat_fr<<-matrix(NA, N, TS)               # matrix to keep track of fat reserves 
  mat_mass<<-matrix(NA,N,TS)               # matrix to keep track of mass of birds 
  mat_caches<<-matrix(NA,N,TS)             # matrix to keep track of the number of caches each bird has at each timestep
  mat_Pkill<<-matrix(NA,N,TS)              # matrix to keep track of what Pkill every bird had at each timestep
  mat_find_food<<-matrix(NA, N, TS)         # Keep track of how many food items are found 
  
  # fill in some initial values for agent variables  (global)
  mass_init<<-8+(rtruncnorm(N, a=0.01, b=0.2, mean=0.1, sd=0.01))             # Gives initial mass from normal distribution (Polo et al. 2007)
  sc_init<<-0+(rtruncnorm(N, a=0, b=stom_size, mean=(stom_size/2), sd=0.01))  # gives initial stomach content from equal distribution
  fr_init<<-0+(rtruncnorm(N, a=0, b=fat_max, mean=(fat_max/2), sd=1))         # gives initial fat reserves for random number between 0-4
  alive_init<<-rep(1, N )                                                     # all birds are alive at the start 
  caches_init<<-round(0+(rtruncnorm(N, a=num_cache_min, b=num_cache_max, mean=((num_cache_min+num_cache_max)/2), sd=25))) # initial cache numbers for birds rounded to closest integer
  
  # Put these in first column of the matrices  
  mat_alive[,1]<<-alive_init
  mat_sc[,1]<<-sc_init
  mat_fr[,1]<<-fr_init
  mat_mass[,1]<<-mass_init
  mat_caches[,1]<<-caches_init
  
  
  # Create empty matrices to keep track of numbers (Global)
  # keep track of means 
  sc_mean<<-matrix(NA, 1, TS)
  fr_mean<<-matrix(NA, 1,TS)
  mass_mean<<-matrix(NA,1,TS)
  alive_mean<<-matrix(NA,1, TS)
  caches_mean<<-matrix(NA,1,TS)
  # count what the birds are up to (Global)
  forage_count<<-matrix(NA, N, TS)
  rest_count<<-matrix(NA, N, TS)
  sleep_count<<-matrix(NA, N,TS)
  retrieve_count<<-matrix(NA, N, TS)
  eat_hoard_count<<-matrix(NA, N, TS)
  eat_count<<-matrix(NA, N, TS)
  predation_count<<-matrix(NA, N,TS)                               # Keep track of how many birds have actually been killed by predation
  
  
  # total number of birds doing behaviours (Global)
  total_forage<<-matrix(NA, 1, TS)                  # total number of birds foraging each timestep
  total_rest<<-matrix(NA,1, TS)                     # total number of birds resting each timestep 
  total_alive<<-matrix(NA,1,TS)                     # total number of birds alive each timestep 
  total_retrieve<<-matrix(NA, 1, TS)                # total number of birds retrieving each timestep
  total_eat_hoard<<-matrix(NA, 1, TS)              # total number of birds eat-hoarding in each timestep
  total_eat<<-matrix(NA, 1, TS)                    # total number of birds eating in each timestep 
  total_predated<<-matrix(NA, 1, TS)
  total_sleep<<-matrix(NA,1,TS)
  
} # end set-up function 



#############################
#    MODEL 1: 1 VARIABLE    # 
#############################

#################################################################
##   Model 1.1: Non-hoarding bird, Access to Stomach Content   ##
#################################################################
    
    ################################# 
    #   set up directories 1.1     # 
    ################################
      # Set up the main directory for where you want the figures saved 
      # This can be replaced by any folder you have on your computer (just make sure you have continuous connection if its a webfolder)
      mainDir<-'C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Figures/5-combi_model/MOD_1_1'
      setwd(mainDir)
      # Run the following if doing this for the first time on devide: 
      # create list of folders that we want present 
      folders<-c('1-run_model', '2-run_opt', '3-env_loop', '4-opt_loop', '5-beh_loop')
      # Check if they exist and if not, create them 
      # Note that this code will warn you if it already existed 
      for (folder in folders){
        dir.create(file.path(mainDir, folder ), showWarnings = TRUE)
      }

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
      MOD_1_1_func(days=30, N=1000, env_type=8, th_forage_sc=0.2, th_forage_fr=1, daylight_h=8, sim_type='run_model')
      MOD_1_1_func(days=30, N=100, env_type=8, th_forage_sc=0.2, th_forage_fr=1, daylight_h=8, sim_type='run_model')
      
      # Optimise for ideal SC-threshold 
      MOD_1_1_opt_th_sc<-function(days, N, env_type, th_forage_fr, daylight_h, th_sc_min, th_sc_max, sim_type){
        # show that optimizatio started 
        print(paste0('MOD 1.1 opt th_sc start' ))
        # select optimization type 
        #sim_type<<-c('run_opt')
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
          MOD_1_1_func(days, N, env_type, current_th_sc, th_forage_fr, daylight_h, sim_type)
          # add to the previously created matrix
          survival_end[1,th]<<-birds_alive_at_end
        } # end of optimization for loop 
        
        # in the end, plot the whole thing 
        fig_opt_hoard_th_sc<<-plot(th_forage_sc, survival_end, main = paste0('opt_sc_mod_1_1_T=', days, ', N=', N, 'env type=', env_type, ', th-fr=', th_forage_fr), ylim = c(0,1) )
        fig_opt_hoard_th_sc
        #dev.off()
        
        # for checking during coding 
        print(paste0('MOD 1.1 opt th_sc function did run with N=', N, 'and Days=', days ))
        
        # save the figure in previously made folders 
        # ONly save the individual plots if youre running the optimization. Not when you'r ein opt-loop 
        
        if(sim_type=='run_opt_sc'){
          setwd(paste0(mainDir, '//2-run_opt/'))
          dev.print(pdf, (paste0('opt_sc_mod_1_1_T=', days, '_N=', N, '_th-fr=', th_forage_fr, '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))
          
        }
        
        
      } # end of optimization function for MOD 1.1 
      
      # Run Visual optimisation 
      dev.new()
      par(mfrow=c(1,1))
      MOD_1_1_opt_th_sc(days=30, N=100, env_type=8, th_forage_fr=1, daylight_h=8 , th_sc_min=0, th_sc_max=0.4, sim_type = 'run_opt_sc')
      
      

    ##############################
    #    Environments loop  1.1  # 
    ##############################
    
    # Function that loops through 18 environments, all other values given 
    
    MOD_1_1_env_loop_func<<-function(days, N,  th_forage_sc,  th_forage_fr, daylight_h, sim_type){
      # Run the model 1.1 function for each of the environments 
      # Create the plot_list 
      survival_plot_list<<-list()
      # Start loop for environments 
      for (i in 1:18){
        # For every environment run the optimization function 
        cur_env_type<<-i
        # run the 'run_model' simulation 
        MOD_1_1_func(days=days, N=N, env_type=cur_env_type, th_forage_sc=th_forage_sc, th_forage_fr=th_forage_fr,  daylight_h=daylight_h, sim_type=sim_type)
        
        # create temporary dataframe for ggplot 
        current_survival_df<<-as.data.frame(t(rbind(total_alive, (1:TS))))
        # make percentages
        current_survival_df$perc_survival<<-((current_survival_df$V1/N)*100)
        # plot
        current_survival_plot<<-ggplot(current_survival_df, aes(x=V2, y=perc_survival))+
          geom_line()+
          labs(
            title = paste('Survival - % birds alive - Environment =', cur_env_type), 
            y='% Alive', 
            x='Timestep')+
          ylim(0,100)
        #survival_plot_list[[i]]<<-current_survival_plot
        #paste('survival_plot_', i)<<-current_survival_plot
        
        survival_plot_list[[i]]<<-current_survival_plot
        
        print(paste0('MOD 1.1 ran for env-type=', cur_env_type))
        
      } # end for loop for the environments 
      
      
      # now plot all of this 
      dev.new() # new window
      do.call('grid.arrange', c(survival_plot_list, ncol=3)) # aggregate the plots 
      # Save the plots in the directories made above   
      setwd(paste0(mainDir, '/3-env_loop//')) # set current wd 
      dev.print(pdf, (paste0('MOD_1_1_env_loop_Days=', days, '_N=', N, '_th-fr=', th_forage_fr, '_th-sc=', th_forage_sc,'_', 'Daylight_h=', daylight_h, '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))
      
    }  # End for MOd 1.1 env_loop function 
    
    # Run it 
    MOD_1_1_env_loop_func(days=30, N=100, th_forage_sc=0.2, th_forage_fr = 1, daylight_h = 8, sim_type = 'env_loop')
    
    
    ###########################################
    #    Optimization loop  optimisation 1.1  # 
    ###########################################
    
  
    MOD_1_1_opt_loop_func<<-function(days, N, th_forage_fr,  th_sc_min, th_sc_max, daylight_h, sim_type){
  
      
      # create an empty object to put the maximum values in 
      mat_max_survival_th_sc<<-matrix(NA, 18, 1)
      
      # START THE FOR LOOP THROUGH EACH OF THE ENVIRONMENTS 
      for (i in 1:18){
        if (i==1){
          # create an empty list to put the optimisation plots in
          optimization_1_1_plot_list<<-list()
        }
        # For every environment run the optimisation function
        cur_env_type<<-i
        # run the function 
        MOD_1_1_opt_th_sc(days=days, N=N, env_type = cur_env_type, th_forage_fr=th_forage_fr, daylight_h=daylight_h , th_sc_min=th_sc_min, th_sc_max=th_sc_max, sim_type = sim_type)
        
        # create temporary dataframe for ggplot 
        current_optimization_df<<-as.data.frame(t(rbind(survival_end, th_forage_sc)))
        
        # SELECTING THE BEST/MAX SURVIVAL SC-TH 
        # Calculate for which threshold the survival is maximum
        current_max_survival<<-max(current_optimization_df$V1)
        # Create a dataframe which holds all rows for which survival is maximum 
        max_thresholds_df<<-subset(current_optimization_df, V1==current_max_survival)
        # Then use the average of those thresholds as the 'best threshold' to get maximum survival
        current_max_th<<-mean(max_thresholds_df$th_forage_sc) # note this will just take the 1 value if there is only one trheshold at the value 
        # get an idea of how many values were used
        num_max<<-nrow(max_thresholds_df)
        # save the current optimal threshold in the matrix 
        mat_max_survival_th_sc[i,1]<<-current_max_th
        
        
        # CREATING THE PLOT   
        # create an object with the optimisation plot in it
        current_optimization_plot<<-ggplot(current_optimization_df, aes(x=th_forage_sc, y=V1))+
          geom_line()+
          labs(
            title = paste('Mean survival at end- Environment =', cur_env_type), 
            y='Mean survival', 
            x='SC-threshold')+
          ylim(0,1)
        #annotate('text', x=0.2, y=0.2, 'Some text')
        
        # df with annotation info
        annotation<<-data.frame(
          x<-c(0.2), 
          y<-c(0.2), 
          label=paste('Opt SC-TH taken from', num_max, 'values = ', current_max_th)
        )
        
        # add the label 
        current_optimization_plot<<-current_optimization_plot + geom_label(data=annotation, aes(x=x, y=y, label=label), 
                                                                           color='orange', 
                                                                           size=3, angle=45, fontface='bold')
        #survival_plot_list[[i]]<<-current_survival_plot
        #paste('survival_plot_', i)<<-current_survival_plot
        
        # put the current plot in a list 
        # optimization_1_1_plot_list[[i]]<<-current_optimization_plot
        optimization_1_1_plot_list<<-append(optimization_1_1_plot_list, list(current_optimization_plot))
        
        # confirm
        print(paste('Optimization ran for environment ', cur_env_type))
        print(paste('The optimal Sc-th for this environment = ', current_max_th))
        
  } # end of the loop for each environment 
  
  # now plot all of this with the optimal values 
  #dev.new() # new window
  do.call('grid.arrange', c(optimization_1_1_plot_list, ncol=3)) # aggregate the plots 
  # Save the plots in the directories made above   
  setwd(paste0(mainDir, '/4-opt_loop//')) # set current wd 
  dev.print(pdf, (paste0('MOD_1_1_opt_loop_Days=', days, '_N=', N, '_th-fr=', th_forage_fr, 'Daylight_h=', daylight_h, '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))
  # save the matrix with optimal values in a dataframe 
  write.csv(mat_max_survival_th_sc, (paste0('opt_loop_max_surv_sc_1_1_df',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.csv')), row.names=FALSE)
  
  
} # end MOD 1.1 opt loop function 

        # Run it 
        # open a new window 
        dev.new()
        # with the right outlines 
        par(mfrow=c(6,3))
        MOD_1_1_opt_loop_func(days = 30, N = 1000, th_forage_fr = 1,  th_sc_min = 0, th_sc_max = 0.4, daylight_h = 8, sim_type = 'opt_loop')

    ###########################################
    #       Behaviour loop - SC-TH 1.1        # 
    ###########################################
    
    # For i in 18 environments 
    # Take the best TH from that enviornment (needs to be created in above code) - Done? 
    # Run the model 1.1 with it 
    # Take those matrices and make the graph with the bheaviours
    # Also make a graph with the stomach content and fat reserves in it 
    
    # Loop for environments 
    dev.new() # new image thingey if needed 
    par(mfrow=c(6,3)) # give it the right format if needed 
    
    # Write the function 
    MOD_1_1_beh_loop_func<<-function(days, N, th_forage_fr, daylight_h, sim_type){
      # Open a new plotting area 
      #dev.new()
      #par(mfrow=c(6,3))
      
      # Loop for each environment 
      for (i in 1:18){
        # Make some lists to fill 
        if (i==1){
          stacked_chart_data_list<<-list()
          stacked_chart_plot_list<<-list()
          fr_sc_plot_list<<-list()
          survival_plot_list<<-list()
        }
        mat_cur_perc_rest<<-matrix(data=NA, nrow= (days*72), ncol=1)
        mat_cur_perc_for<<-matrix(NA, TS, 1)
        mat_cur_perc_sleep<<-matrix(NA, TS, 1)
        # indicate the current environment
        cur_env_type<<-i
        # Take the sc-th from the optimisation that had maximum survival 
        current_opt_sc_th<<-mat_max_survival_th_sc[i,1]
        
        # Now run the model 1.1 with this value 
        MOD_1_1_func(days= days, N= N, env_type=cur_env_type, th_forage_sc=current_opt_sc_th, th_forage_fr=th_forage_fr, daylight_h= daylight_h, sim_type = sim_type)
        
        # GRAPHS TO SHOW SUVIVAL TRAJECTORIES 
              # For the graphs that display the survival trajectories throughout 30 days in each env 
              # create temporary dataframe for ggplot 
              current_survival_df<<-as.data.frame(t(rbind(total_alive, (1:TS))))
              # make percentages
              current_survival_df$perc_survival<<-((current_survival_df$V1/N)*100)
              # plot it 
              current_survival_plot<<-ggplot(current_survival_df, aes(x=V2, y=perc_survival))+
                geom_line(size = 1)+
                labs(
                  title = paste('Survival - % birds alive - Environment =', cur_env_type, 'sc-th=', round(current_opt_sc_th, digits = 2)), 
                  y='% Alive', 
                  x='Timestep')+
                ylim(0,100)
              # pop the plot in the list 
              survival_plot_list<<-append(survival_plot_list, list(current_survival_plot))
              # save the dataframe 
              setwd(paste0(mainDir, '/5-beh_loop//')) # set current wd 
              write.csv(current_survival_df, (paste0('beh_loop_surv_1_1_df_env', i, '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.csv')), row.names=FALSE)
              
        
        # GRAPHS TO SHOW THE BEHAVIOUR TRAJECTORIES   
              # Once you have hte matrices, calculate this for every timestep 
              for (j in (1:TS)){
                # The percentage resting 
                mat_cur_perc_rest[j,1]<<-((total_rest[1,j]/total_alive[1,j])*100)
                # The percentage foraging 
                mat_cur_perc_for[j,1]<<-((total_forage[1,j]/total_alive[1,j])*100)
                # the percentage sleeping 
                mat_cur_perc_sleep[j,1]<<-((total_sleep[1,j]/total_alive[1,j])*100)
              }
              # Add column with numbers 
              timesteps<<-1:TS
              # put them on a daily scale 
              timesteps_dayscale<<-timesteps%%72
              # Attach matrices 
              mat_perc_cur_env<<-cbind(mat_cur_perc_rest, mat_cur_perc_for, mat_cur_perc_sleep, timesteps_dayscale)
              # turn to df 
              df_perc_cur_env<<-as.data.frame(mat_perc_cur_env)
              # set names 
              colnames(df_perc_cur_env)[1]<<-'rest'
              colnames(df_perc_cur_env)[2]<<-'forage'
              colnames(df_perc_cur_env)[3]<<-'sleep'
              # now start grouping 
              rest_perc<<-group_by(df_perc_cur_env, timesteps_dayscale) %>% summarize (m=mean(rest))
              forage_perc<<-group_by(df_perc_cur_env, timesteps_dayscale) %>% summarize (m=mean(forage))
              sleep_perc<<-group_by(df_perc_cur_env, timesteps_dayscale) %>% summarize (m=mean(sleep))
              # add group
              rest_perc$beh<<-rep('rest')
              forage_perc$beh<<-rep('forage')
              sleep_perc$beh<<-rep('sleep')
              # make new dataframe 
              df_for_chart<<-rbind(rest_perc, forage_perc, sleep_perc)
              # Ideally, I'd store this in some sort of list so I can access it afterwards 
              stacked_chart_data_list<<-append(stacked_chart_data_list, list(df_for_chart))
              # I want to plot only the time that the birds are awake (they all go to sleep at night anyway)
              # calculate the # of timesteps that birds are awake and put this in the xlim of the graphs 
              timesteps_awake<<-daylight_h*3
              # Now make the chart 
              cur_stacked_plot<<-ggplot(df_for_chart, aes(x=timesteps_dayscale, y=m, fill=beh))+
                geom_area(alpha=0.8, size=0.5, colour='white')+
                scale_fill_viridis(discrete = T)+
                #theme_ipsum()+
                #ggtitle('Percentage of Birds per Behaviour')
                labs(
                  title = paste('Average %of Alive Birds in env.', i,  'sc-th=', round(current_opt_sc_th, digits = 2)), 
                  x='Timestep in a 24 day (20 min increments)', 
                  y='% of Alive birds')+
                xlim(0, timesteps_awake)
              # put plot in the list 
              stacked_chart_plot_list<<-append(stacked_chart_plot_list, list(cur_stacked_plot))
              # save dataframe 
              write.csv(df_for_chart, (paste0('beh_loop_beh_1_1_df_env', i, '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.csv')), row.names=FALSE)
        
        # GRAPHS TO SHOW THE FR AND SC TRAJECTORIES 
              # create a df
              fr_sc_graph<<-rbind(fr_mean, sc_mean, timesteps_dayscale)
              fr_sc_graph<<-t(fr_sc_graph)
              # turn to df
              fr_sc_graph<<-as.data.frame(fr_sc_graph)
              # set names
              colnames(fr_sc_graph)[1]<<-'fr'
              colnames(fr_sc_graph)[2]<<-'sc'
              # start grouping
              # now start grouping
              fr_grouped<<-group_by(fr_sc_graph, timesteps_dayscale) %>% summarize (m=mean(fr))
              sc_grouped<<-group_by(fr_sc_graph, timesteps_dayscale) %>% summarize (m=mean(sc))
              # add group
              fr_grouped$type<<-rep('fr')
              sc_grouped$type<<-rep('sc')
              #sleep_perc$beh<-rep('sleep')
              # make new dataframe
              df_for_sc_fr_chart<<-rbind(fr_grouped, sc_grouped)
              # graph
              # Now make the chart
              cur_fr_sc_plot<<-ggplot(df_for_sc_fr_chart, aes(x=timesteps_dayscale, y=m, col=type))+
                #geom_area(alpha=0.8, size=0.5, colour='white')+
                geom_line(size = 1)+
                scale_fill_viridis(discrete = T)+
                #theme_ipsum()+
                #ggtitle('Percentage of Birds per Behaviour')
                labs(
                  title = paste('FR and SC in environment', i,  'sc-th=', round(current_opt_sc_th, digits = 2)),
                  x='Timestep in a 24 day (20 min increments)',
                  y='grams')+
                xlim(0, timesteps_awake)+
                ylim(0,5)
              # put plot in the list
              fr_sc_plot_list<<-append(fr_sc_plot_list, list(cur_fr_sc_plot))
              # save dataframe 
              write.csv(df_for_sc_fr_chart, (paste0('beh_loop_sc_fr_1_1_df_env', i, '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.csv')), row.names=FALSE)
              # for ease of use 
              print(paste('Code for the stacked area graphs/sc-fr graphs is done for env=', cur_env_type))
        
      } # END FOR LOOP ENVIRONTMENTS 
      
      
    } # end function MOD 1.1 behaviour loop 
    
        # Run it 
        MOD_1_1_beh_loop_func(days = 30, N = 10, th_forage_fr = 1, daylight_h = 8, sim_type = 'beh_loop')
        
        
        # Plot all 3 the graph panels
        setwd(paste0(mainDir, '/5-beh_loop//')) # set current wd 
        #dev.new()
        #par(mfrow=c(6,3))
        do.call('grid.arrange', c(survival_plot_list, ncol=3))
        dev.print(pdf, (paste0('beh_loop_surv_1_1',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))
        do.call('grid.arrange', c(stacked_chart_plot_list, ncol=3)) # aggregate the plots
        dev.print(pdf, (paste0('beh_loop_beh_traj_1_1',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))
        do.call('grid.arrange', c(fr_sc_plot_list, ncol=3)) # aggregate the plots
        dev.print(pdf, (paste0('beh_loop_sc_fr_1_1',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))
        
    
    
#################################################################
##   Model 1.2: Non-hoarding bird, Access to Fat-reserves      ##
#################################################################        

    ################################ 
    #   set up directories   1.2   # 
    ################################
    
    # Set up the main directory for where you want the figures saved 
    # This can be replaced by any folder you have on your computer (just make sure you have continuous connection if its a webfolder)
    mainDir_1_2<-'C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Figures/5-combi_model/MOD_1_2'
    setwd(mainDir_1_2)
    # Run the following if doing this for the first time on devide: 
    # create list of folders that we want present 
    folders<-c('1-run_model', '2-run_opt', '3-env_loop', '4-opt_loop', '5-beh_loop')
    # Check if they exist and if not, create them 
    # Note that this code will warn you if it already existed 
    for (folder in folders){
      dir.create(file.path(mainDir_1_2, folder ), showWarnings = TRUE)
    }
    
    ###############################
    #    Functions & running 1.2  #
    ###############################
    
    # write function 
    MOD_1_2_func<-function(days, N, env_type, th_forage_sc, th_forage_fr,daylight_h, sim_type){
      
      # new plotting area 
      if (sim_type=='run_model'){
        dev.new()
      }
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
        }else{
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
          }else if (mat_fr[i,t]==0){
            # if not step 1 and not previously dead 
            # check if the bird should die now
            mat_alive[i,t]<<-0
          }else{
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
          }else {
            
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
              #set the predation risk 
              Patt_cur<<-Patt_sleep
              
              # Food will be moved from the stomach
              # Into the fat reserves 
              # and be burned depending on BMR-multi
              # in the ' Everyone '  part of the code below
            # end of birds that are asleep   
            }else{
              
              # NON SLEEPING BIRDS START HERE : >>>>>>>>>
              # set the sleeping matrix to 0 
              sleep_count[i,t]<<-0
              
              # Check what behavior the bird should do if it is day 
              # CHECK IF BIRD IS HUNGRY AND NEEDS FOOD
              # This is done for all hoarding and non-hoarding birds 
              
              # Time to forage: 
              
              # Only access to stomach-content 
              if ((mat_fr[i,t]) < th_forage_fr){
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
                
                # update the eating-matrix 
                # update the global counters 
                eat_hoard_count[i,t]<<-0
                retrieve_count[i,t]<<-0
                eat_count[i,t]<<-1
                
                # Stomach content is already updated 
                # Update BMR multi
                BMR_multi<<-8
                # ends the foraging for non hoarding birds statement 
              # ends the foraging statement  
              }else{
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
            }else{
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
              }else{
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
        total_predated[1,t]<<-sum(predation_count[,t], na.rm=TRUE)        # how many birds were killed by predation in this timestep 
        total_sleep[1,t]<<-sum(sleep_count[,t], na.rm = TRUE)             # same for sleep
        
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
          
          # 6 Percentage of birds that are resting (of the alive birds)
          plot6<<-plot(1:t, ((total_rest[1,(1:t)])/(total_alive[1,(1:t)])*100), ylim=c(0, 100), ylab='%', xlab='Timestep', main='Percentage of alive birds resting', type='l')
          
          # 7
          plot7<<-plot(1:t, ((total_retrieve[1,(1:t)])/(total_alive[1,(1:t)])*100), ylim=c(0, 100), ylab='%', xlab='Timestep', main='Percentage of alive birds retrieving', type='l')
          
          # 8
          plot8<<-plot(1:t, ((total_eat_hoard[1,(1:t)])/(total_alive[1,(1:t)])*100), ylim=c(0, 100), ylab='%', xlab='Timestep', main='Percentage of alive birds eat-hoarding', type='l')
          
          # 7: To show predation
          plot9<-plot(1:t, (total_predated[1,(1:t)]), ylim=c(0, 5), ylab='# killed by predation', xlab='Timestep', main='Number of birds killed by predation', type='l')
          
          # 10 total forage 
          plot10<<-plot(1:t, ((total_forage[1,(1:t)])/(total_alive[1,(1:t)])*100), ylim=c(0, 100), ylab='%', xlab='Timestep', main='Percentage of alive birds foraging', type='l')
          
          
          mtext((paste('Days=', days, '_N=', N, 'Daylight_h=', daylight_h, '_th-fr=', th_forage_fr )), side=3, cex=0.8,line=-2, outer=TRUE)
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
      if(sim_type=='run_opt_fr'){                                                     
        print(paste0('1.2 function did run for fr:', current_th_fr ))
      }
      
      # print the graph with 10 panels (beh & sc & fr) if you are just running the model 
      if(sim_type=='run_model'){
        print(paste0('ready to save MOD 1.2 simulation plots'))
        setwd(paste0(mainDir_1_2, '/1-run_model//')) # set current wd 
        dev.print(pdf, (paste0('Simulation_Days=', days, '_N=', N, 'env=', env_type, 'MaxT-range=', Tmax_range_low, '-', Tmax_range_high, ' MinT-range=', Tmin_range_high, '-', Tmin_range_low, '_th-fr=', th_forage_fr, '_th-sc=', th_forage_sc, '_food-m=',num_food_mean, '_', 'Daylight_h=', daylight_h, '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))
        
      }
      
      
    } # end the 1.2 function 
    
    # Run it
    dev.new()
    MOD_1_2_func(days=30, N=100, env_type=8, th_forage_sc=0.2, th_forage_fr=1, daylight_h=8, sim_type='run_model')
    #MOD_1_2_func(days=30, N=1000, env_type=3, th_forage_sc=0.2, th_forage_fr=1, noplot=0, hoard_on=0, daylight_h=8)
    
    # Optimize for ideal FR-threshold 
    MOD_1_2_opt_th_fr<-function(days, N, env_type, th_forage_sc, daylight_h, th_fr_min, th_fr_max, sim_type){
      # show that optimizatio started 
      print(paste0('MOD 1.2 opt th_fr start ' ))
      # creates 100 values between 0 and 0.4, evenly spaced between minimum and maximum inputs 
      th_forage_fr<<-linspace(th_fr_min, th_fr_max, n=100)
      
      # now create a space to save the survival for each different value of th_forage_sc 
      survival_end<<-matrix(NA, 1, length(th_forage_fr))
      
      for (th in 1:length(th_forage_fr)){
        # Run the rest_forage function for each th_forage_sc that you have created. 
        # keep the number of days ,individuals, day temp, night temp, fat-reserve threshold, food distribution the same
        # determine the current threshold for each loop 
        current_th_fr<<-th_forage_fr[th]
        current_th<<-current_th_fr            # needs to have a general name for the rest-forage function printing (works for both sc and fr optimisations)
        # now run 
        MOD_1_2_func(days = days, N = N, env_type = env_type, th_forage_sc = th_forage_sc, th_forage_fr = current_th_fr, daylight_h = daylight_h, sim_type = sim_type)
        # add to the previously created matrix
        survival_end[1,th]<<-birds_alive_at_end
      } # end of optimization for loop 
      
      # in the end, plot the whole thing 
      fig_opt_hoard_th_fr<<-plot(th_forage_fr, survival_end, main = paste0('opt th_fr T=', days, ', N=', N, 'env type=', env_type, ', th-sc=', th_forage_sc), ylim = c(0,1) )
      fig_opt_hoard_th_fr
      
      # for checking during coding 
      print(paste0('MOD 1.2 opt th_fr function did run' ))
      
      if(sim_type=='run_opt_fr'){
        setwd(paste0(mainDir_1_2, '//2-run_opt/'))
        dev.print(pdf, (paste0('opt_fr_mod_1_2_T=', days, '_N=', N, '_th-sc=', th_forage_sc, '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))
        
      }
      #dev.print(pdf, (paste0('opt_th_fr_T=', days, '_N=', N, '_th-sc=', th_forage_sc,  'Hoard=', hoard_on, '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))
      
    } # end of optimization function 1.2 for FR 
    
        # Run it 
        dev.new()
        MOD_1_2_opt_th_fr(days=30, N=100, env_type=3, th_forage_sc=0.2, daylight_h=8, th_fr_min=0, th_fr_max=4, sim_type = 'run_opt_fr') 
        
    
    
    ###############################
    #    Environments loop  1.2   # 
    ###############################
    
    # Run the model 1.1 function for each of the environments 
    
    MOD_1_2_env_loop_func<<-function(days, N, th_forage_sc, th_forage_fr, daylight_h, sim_type){
      dev.new()
      survival_plot_list<<-list()
      
      for (i in 1:18){
        
        # For every environment run the optimisation function 
        cur_env_type<<-i
        #MOD_1_1_opt_th_sc(days=30, N=100, env_typ=cur_env_type, th_forage_fr=1, noplot=1, hoard_on=0, daylight_h=8 , th_sc_min=0, th_sc_max=0.4)
        MOD_1_2_func(days= days, N= N, env_type=cur_env_type, th_forage_sc=th_forage_sc, th_forage_fr=th_forage_fr, daylight_h=daylight_h, sim_type = sim_type)
        
        # create temporary dataframe for ggplot 
        current_survival_df<<-as.data.frame(t(rbind(total_alive, (1:TS))))
        # make percentages 
        current_survival_df$perc_survival<<-((current_survival_df$V1/N)*100)
        # plot 
        current_survival_plot<<-ggplot(current_survival_df, aes(x=V2, y=perc_survival))+
          geom_line()+
          labs(
            title = paste('Survival - % birds alive - Environment =', cur_env_type), 
            y='% Alive', 
            x='Timestep')+
          ylim(0,101)
        #survival_plot_list[[i]]<<-current_survival_plot
        #paste('survival_plot_', i)<<-current_survival_plot
        
        survival_plot_list[[i]]<<-current_survival_plot
        
        # for ease of use 
        print(paste('environment loop 1.2 done for env=', cur_env_type))
        
      } # end for loop for the environments 
      
      # now plot all of this 
      do.call('grid.arrange', c(survival_plot_list, ncol=3)) # aggregate the plots 
      # Save the plots in the directories made above   
      setwd(paste0(mainDir_1_2, '/3-env_loop//')) # set current wd 
      dev.print(pdf, (paste0('MOD_1_2_env_loop_Days=', days, '_N=', N, '_th-fr=', th_forage_fr, '_th-sc=', th_forage_sc,'_', 'Daylight_h=', daylight_h, '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))
      
      
    } # end of MOD 1.2 environment loop function 
    
    # Now run it 
    MOD_1_2_env_loop_func(days = 30, N = 100, th_forage_sc = 0.2, th_forage_fr = 1, daylight_h = 8, sim_type = 'env_loop')
    
    ###########################################
    #          Optimization Loop 1.2          # 
    ###########################################

    MOD_1_2_opt_loop_func<<-function(days, N, th_forage_sc, th_fr_min, th_fr_max, daylight_h, sim_type){
    
      # create an empty object to put the maximum values in 
      mat_max_survival_th_fr<<-matrix(NA, 18, 1)
      
      # START THE FOR LOOP THROUGH EACH OF THE ENVIRONMENTS 
      for (i in 1:18){
        if (i==1){
          # create an empty list to put the optimisation plots in
          optimization_1_2_plot_list<<-list()
        }
        # For every environment run the optimisation function
        cur_env_type<<-i
        MOD_1_2_opt_th_fr(days=days, N=N, env_typ=cur_env_type, th_forage_sc=th_forage_sc, daylight_h=daylight_h , th_fr_min=th_fr_min, th_fr_max=th_fr_max, sim_type = sim_type)
        
        # create temporary dataframe for ggplot 
        current_optimization_df<<-as.data.frame(t(rbind(survival_end, th_forage_fr)))
        
        # SELECTING THE BEST/MAX SURVIVAL SC-TH 
        # Calculate for which threshold the survival is maximum
        current_max_survival<<-max(current_optimization_df$V1)
        # Create a dataframe which holds all rows for which survival is maximum 
        max_thresholds_df<<-subset(current_optimization_df, V1==current_max_survival)
        # Then use the average of those thresholds as the 'best threshold' to get maximum survival
        current_max_th<<-mean(max_thresholds_df$th_forage_fr) # note this will just take the 1 value if there is only one trheshold at the value 
        # get an idea of how many values were used
        num_max<<-nrow(max_thresholds_df)
        # save the current optimal threshold in the matrix 
        mat_max_survival_th_fr[i,1]<<-current_max_th
        
        
        # CREATING THE PLOT   
        # create an object with the optimisation plot in it
        current_optimization_plot<<-ggplot(current_optimization_df, aes(x=th_forage_fr, y=V1))+
          geom_line()+
          labs(
            title = paste('Mean survival at end- Environment =', cur_env_type), 
            y='Mean survival', 
            x='FR-threshold')+
          ylim(0,1)
        #annotate('text', x=0.2, y=0.2, 'Some text')
        
        # df with annotation info
        annotation<<-data.frame(
          x<-c(2), 
          y<-c(0.2), 
          label=paste('Opt FR-TH taken from', num_max, 'values = ', current_max_th)
        )
        
        # add the label 
        current_optimization_plot<<-current_optimization_plot + geom_label(data=annotation, aes(x=x, y=y, label=label), 
                                                                           color='orange', 
                                                                           size=3, angle=45, fontface='bold')
        
        # put the current plot in a list 
        # optimization_1_1_plot_list[[i]]<<-current_optimization_plot
        optimization_1_2_plot_list<<-append(optimization_1_2_plot_list, list(current_optimization_plot))
        
        
        # confirm
        print(paste('Optimization ran for environment ', cur_env_type))
        print(paste('The optimal FR-th for this environment = ', current_max_th))
        
      } # end of the loop for each environment 
      
      
      # now plot all of this 
      #dev.new() # new window
      do.call('grid.arrange', c(optimization_1_2_plot_list, ncol=3)) # aggregate the plots 
      
      # Save the plots in the directories made above   
      setwd(paste0(mainDir_1_2, '/4-opt_loop//')) # set current wd 
      dev.print(pdf, (paste0('MOD_1_2_opt_loop_Days=', days, '_N=', N, '_th-sc=', th_forage_sc, 'Daylight_h=', daylight_h, '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))
      
      
    } # End of the optimization loop MOD 1.2 function 
        
        # open a new window 
        dev.new()
        # with the right outlines 
        par(mfrow=c(6,3))
        # Now run it 
        MOD_1_2_opt_loop_func(days = 30, N = 10, th_forage_sc = 0.2, th_fr_min = 0, th_fr_max = 4, daylight_h = 8, sim_type = 'opt_loop')
        
    ###########################################
    #       Behaviour loop - SC-TH 1.2        # 
    ###########################################
  
    # Write the function 
    MOD_1_2_beh_loop_func<<-function(days, N, th_forage_sc, daylight_h, sim_type){
        # Open a new plotting area 
        # dev.new()
        # par(mfrow=c(6,3))
        # 
        # Loop for each environment 
        for (i in 1:18){
          # Make some lists to fill 
          if (i==1){
            stacked_chart_data_list<<-list()
            stacked_chart_plot_list<<-list()
            fr_sc_plot_list<<-list()
            survival_plot_list<<-list()
          }
          mat_cur_perc_rest<<-matrix(data=NA, nrow= (days*72), ncol=1)
          mat_cur_perc_for<<-matrix(NA, TS, 1)
          mat_cur_perc_sleep<<-matrix(NA, TS, 1)
          # indicate the current environment
          cur_env_type<<-i
          # Take the sc-th from the optimisation that had maximum survival 
          current_opt_fr_th<<-mat_max_survival_th_fr[i,1]
          
          # Now run the model 1.2 with this value 
          MOD_1_2_func(days= days, N= N, env_type=cur_env_type, th_forage_sc=th_forage_sc, th_forage_fr=current_opt_fr_th, daylight_h= daylight_h, sim_type = sim_type)
          
          
          # GRAPHS TO SHOW SUVIVAL TRAJECTORIES 
                # For the graphs that display the survival trajectories throughout 30 days in each env 
                # create temporary dataframe for ggplot 
                current_survival_df<<-as.data.frame(t(rbind(total_alive, (1:TS))))
                # make percentages
                current_survival_df$perc_survival<<-((current_survival_df$V1/N)*100)
                # plot it 
                current_survival_plot<<-ggplot(current_survival_df, aes(x=V2, y=perc_survival))+
                  geom_line(size=1)+
                  labs(
                    title = paste('Survival (%alive)- Env =', cur_env_type, 'th-fr=', round(current_opt_fr_th, digits = 2)), 
                    y='% Alive', 
                    x='Timestep')+
                  ylim(0,100)
                # pop the plot in the list 
                survival_plot_list<<-append(survival_plot_list, list(current_survival_plot))
                
          
          # GRAPHS TO SHOW THE BEHAVIOUR TRAJECTORIES   
                # Once you have hte matrices, calculate this for every timestep 
                for (j in (1:TS)){
                  # The percentage resting 
                  mat_cur_perc_rest[j,1]<<-((total_rest[1,j]/total_alive[1,j])*100)
                  # The percentage foraging 
                  mat_cur_perc_for[j,1]<<-((total_forage[1,j]/total_alive[1,j])*100)
                  # the percentage sleeping 
                  mat_cur_perc_sleep[j,1]<<-((total_sleep[1,j]/total_alive[1,j])*100)
                }
                # Add column with numbers 
                timesteps<<-1:TS
                # put them on a daily scale 
                timesteps_dayscale<<-timesteps%%72
                # Attach matrices 
                mat_perc_cur_env<<-cbind(mat_cur_perc_rest, mat_cur_perc_for, mat_cur_perc_sleep, timesteps_dayscale)
                # turn to df 
                df_perc_cur_env<<-as.data.frame(mat_perc_cur_env)
                # set names 
                colnames(df_perc_cur_env)[1]<<-'rest'
                colnames(df_perc_cur_env)[2]<<-'forage'
                colnames(df_perc_cur_env)[3]<<-'sleep'
                # now start grouping 
                rest_perc<<-group_by(df_perc_cur_env, timesteps_dayscale) %>% summarize (m=mean(rest))
                forage_perc<<-group_by(df_perc_cur_env, timesteps_dayscale) %>% summarize (m=mean(forage))
                sleep_perc<<-group_by(df_perc_cur_env, timesteps_dayscale) %>% summarize (m=mean(sleep))
                # add group
                rest_perc$beh<<-rep('rest')
                forage_perc$beh<<-rep('forage')
                sleep_perc$beh<<-rep('sleep')
                # make new dataframe 
                df_for_chart<<-rbind(rest_perc, forage_perc, sleep_perc)
                # Ideally, I'd store this in some sort of list so I can access it afterwards 
                stacked_chart_data_list<<-append(stacked_chart_data_list, list(df_for_chart))
                # I want to plot only the time that the birds are awake (they all go to sleep at night anyway)
                # calculate the # of timesteps that birds are awake and put this in the xlim of the graphs 
                timesteps_awake<<-daylight_h*3
                # Now make the chart 
                cur_stacked_plot<<-ggplot(df_for_chart, aes(x=timesteps_dayscale, y=m, fill=beh))+
                  geom_area(alpha=0.8, size=0.5, colour='white')+
                  scale_fill_viridis(discrete = T)+
                  #theme_ipsum()+
                  #ggtitle('Percentage of Birds per Behaviour')
                  labs(
                    title = paste('% of birds in Env.', i, 'th-fr=', round(current_opt_fr_th, digits = 2)), 
                    x='Timestep in a 24 day (20 min increments)', 
                    y='% of Alive birds')+
                  xlim(0, timesteps_awake)
                # put plot in the list 
                stacked_chart_plot_list<<-append(stacked_chart_plot_list, list(cur_stacked_plot))
                
          # GRAPHS TO SHOW THE FR AND SC TRAJECTORIES 
              # create a df
              fr_sc_graph<<-rbind(fr_mean, sc_mean, timesteps_dayscale)
              fr_sc_graph<<-t(fr_sc_graph)
              # turn to df
              fr_sc_graph<<-as.data.frame(fr_sc_graph)
              # set names
              colnames(fr_sc_graph)[1]<<-'fr'
              colnames(fr_sc_graph)[2]<<-'sc'
              # start grouping
              # now start grouping
              fr_grouped<<-group_by(fr_sc_graph, timesteps_dayscale) %>% summarize (m=mean(fr))
              sc_grouped<<-group_by(fr_sc_graph, timesteps_dayscale) %>% summarize (m=mean(sc))
              # add group
              fr_grouped$type<<-rep('fr')
              sc_grouped$type<<-rep('sc')
              #sleep_perc$beh<-rep('sleep')
              # make new dataframe
              df_for_sc_fr_chart<<-rbind(fr_grouped, sc_grouped)
              # graph
              # Now make the chart
              cur_fr_sc_plot<<-ggplot(df_for_sc_fr_chart, aes(x=timesteps_dayscale, y=m, col=type))+
                #geom_area(alpha=0.8, size=0.5, colour='white')+
                geom_line(size=1)+
                scale_fill_viridis(discrete = T)+
                #theme_ipsum()+
                #ggtitle('Percentage of Birds per Behaviour')
                labs(
                  title = paste('FR and SC in Env=', i, 'th-fr=', round(current_opt_fr_th, digits = 2)),
                  x='Timestep in a 24 day (20 min increments)',
                  y='grams')+
                xlim(0, timesteps_awake)+
                ylim(0,5)
              # put plot in the list
              fr_sc_plot_list<<-append(fr_sc_plot_list, list(cur_fr_sc_plot))
              
              # for ease of use 
              print(paste('Code for the stacked area graphs/sc-fr graphs is done for env=', cur_env_type))
              
        } # END FOR LOOP ENVIRONTMENTS 
        
        
      } # end function MOD 1.2 behaviour loop 
      
        # Run it 
        MOD_1_2_beh_loop_func(days = 30, N = 10, th_forage_sc = 0.2, daylight_h = 8, sim_type = 'beh_loop')
      
        
        # Plot all 3 the graph panels
        setwd(paste0(mainDir_1_2, '/5-beh_loop//')) # set current wd 
          # dev.new()
          # par(mfrow=c(6,3))
        do.call('grid.arrange', c(survival_plot_list, ncol=3))
        dev.print(pdf, (paste0('beh_loop_surv_1_2',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))
        # beh
        do.call('grid.arrange', c(stacked_chart_plot_list, ncol=3)) # aggregate the plots
        dev.print(pdf, (paste0('beh_loop_beh_traj_1_2',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))
        # fr and sc
        do.call('grid.arrange', c(fr_sc_plot_list, ncol=3)) # aggregate the plots
        dev.print(pdf, (paste0('beh_loop_sc_fr_1_2',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))
        
        
        


#################################################################
##   Model 1.3: Hoarding bird, Access to Stomach content       ##
#################################################################        

    ################################ 
    #   set up directories   1.3   # 
    ################################
    # Set up the main directory for where you want the figures saved 
    # This can be replaced by any folder you have on your computer (just make sure you have continuous connection if its a webfolder)
    mainDir<-'C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Figures/5-combi_model/MOD_1_3'
    setwd(mainDir)
    # Run the following if doing this for the first time on devide: 
    # create list of folders that we want present 
    folders<-c('1-run_model', '2-run_opt', '3-env_loop', '4-opt_loop', '5-beh_loop')
    # Check if they exist and if not, create them 
    # Note that this code will warn you if it already existed 
    for (folder in folders){
      dir.create(file.path(mainDir, folder ), showWarnings = TRUE)
    }
    
    ###############################
    #    Functions & running 1.3  #
    ###############################
    
    # write function for the  model
    MOD_1_3_func<-function(days, N, env_type, th_forage_sc1, th_forage_sc2, th_forage_fr, daylight_h, sim_type){
      
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
        } else{
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
          }else if (mat_alive[i,(t-1)]==0){
            # if not step 1, check if bird was previously dead
            # if previously dead, it needs to be dead now 
            mat_alive[i,t]<<-0
          }else if (mat_fr[i,t]==0){
            # if not step 1 and not previously dead 
            # check if the bird should die now 
            mat_alive[i,t]<<-0
          }else{
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
            } else{
              
              # NON SLEEPING BIRDS START HERE : >>>>>>>>>
              # set the sleeping matrix to 0 
              sleep_count[i,t]<<-0
              
              # Check what behavior the bird should do if it is day 
              # CHECK IF BIRD IS HUNGRY AND NEEDS FOOD
              # This is done for all hoarding and non-hoarding birds 
              
              # Time to forage: 
              
              ##################################
              #####  CHANGE FOR MODEL 1.3 ###### 
              ##################################
              
              # Only access to stomach-content 
              # The lowest threshold determines if the bird will retrieve 
              # The hightes threshold determines if the bird will rest 
              
              if ((mat_sc[i,t]) <= th_forage_sc2){
                # If this is the case, the bird is hungry and needs to forage for food 
                # Resting is not an option here 
                
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
                
                
                # START IF STATEMENT FOR HOARDING BIRDS           
                
                # WHAT KIND OF FORAGING IS HAPPENING?
                # 3 kinds of foraging are possible:
                #       1. Retrieve stored item
                #       2. Find new item and eat it (done after this)
                #       3. Find new item and eat it till full, hoard after this
                
                
                ####################
                ##   RETRIEVING   ## 
                ####################
                
                # retrieving happens when the stomach content is below the lowest threshold (sc-th1)
                # The bird also needs to have the minimum number of caches to allow retrieval 
                if ((mat_sc[i,t]<=th_forage_sc1) && (mat_caches[i,t]>retrieve_min)){

                    # You can retrieve now 
                    # note that birds that have a sc too high will forage
                    # The same goes for birds that don't have enough caches to go and retrieve 
                  
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
             
                } else{
                  
                  #######################
                  #   NORMAL FORAGING   #
                  #######################
                  
                  # If the bird is foraging, but not retrieving, it will eat-hoard or eat the food
                  # Either way, it will need to find food first
                  
                  # code checking
                  #print(paste('bird ', N, 'is foraging not retrieving'))
                  
                  # update the global counting variable
                  retrieve_count[i,t]<<-0
                  
                  # Run the forage function and decide what number of items is found
                  # The outcoem here is 'food_item_found'
                  forage_function(num_food_mean, prob_b_forage, b_size)
                  
                  # Pop this in the matrix (this is in number of items found)
                  mat_find_food[i,t]<<-food_item_found
                  # convert to grams
                  food_item_found_gram<<-(food_item_found*food_item)
                  
                  # Update agent-owned variables:
                  # First, increase the stomach content with whatever food is found
                  mat_sc[i,(t)]<<-(mat_sc[i,t])+(food_item_found_gram)
                  
                  # now check if this exceeds the stomach size
                  if (mat_sc[i,(t)]>stom_size){
                    # This means the bird found more than it can eat
                    # It will hoard the surpluss
                    
                    ######################
                    #    EAT-HOARD       #
                    ######################
                    
                    # code checking
                    #print(paste0('bird ', i, ' is eat-hoarding'))
                    
                    # update the global counters
                    eat_hoard_count[i,t]<<-1
                    eat_count[i,t]<<-0
                    
                    # update agent-owned variables
                    hoard_surplus<<-floor((mat_sc[i,(t)]-stom_size)/food_item)  # Determine The surplus available in whole food items
                    mat_sc[i,(t)]<<-stom_size                                   # The stomach is set to the stomach size (full)
                    mat_caches[i,t]<<-(mat_caches[i,t])+hoard_surplus
                    
                    # update BMR multi
                    BMR_multi<<-8
                  
                    # end of eat-hoard if-statement  
                  } else{
                    # This means the food eaten does not exceed teh stomach size
                    # no hoarding required, the bird will just eat
                    
                    #############
                    #   EAT     #
                    #############
                    
                    # code cdhecking
                    #print(paste0('bird ', i, ' is eating'))
                    # update the global counters
                    eat_hoard_count[i,t]<<-0
                    eat_count[i,t]<<-1
                    
                    # Stomach content is already updated
                    
                    # Update BMR multi
                    BMR_multi<<-8
                    
                  } # end of the eat statement
                  
                } # end of forage but not retrieving statement
                
                # ends the if hoard_on == 1 statement (fOR ALL BIRDS IN MODEL 1.3 TRUE)
                
                # NOW THE SECTION FOR THE NON-HOARDING BIRDS 
                # Don't forget to change the matrix names here: forage should be 'eat' now. 
                
                # ends the foraging code (below the sc-th2) 
              } else {
                ##################
                #    RESTING     # 
                ##################
                # All birds of which the th-sc is above th2 should go rest 
                
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
              
            } # end of 'Time of day = day ' statement (non-sleeping birds )
            
            
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
            } else{
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
              }else{
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
        total_predated[1,t]<<-sum(predation_count[,t], na.rm=TRUE)        # how many birds were killed by predation in this timestep 
        total_sleep[1,t]<<-sum(sleep_count[,t], na.rm = TRUE)             # same for sleep
        
        
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
        if ((t/plot_interval)==floor(t/plot_interval) && sim_type == 'run_model' ){
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
          # 5 % birds eating 
          plot5<<-plot(1:t, ((total_eat[1,(1:t)])/(total_alive[1,(1:t)])*100), ylim=c(0, 100), ylab='%', xlab='Timestep', main='Percentage of alive birds eating', type='l')
          
          # 6 Percentage of birds that are resting (of the alive birds)
          plot6<<-plot(1:t, ((total_rest[1,(1:t)])/(total_alive[1,(1:t)])*100), ylim=c(0, 100), ylab='%', xlab='Timestep', main='Percentage of alive birds resting', type='l')
          
          # 7
          plot7<<-plot(1:t, ((total_retrieve[1,(1:t)])/(total_alive[1,(1:t)])*100), ylim=c(0, 100), ylab='%', xlab='Timestep', main='Percentage of alive birds retrieving', type='l')
          
          # 8
          plot8<<-plot(1:t, ((total_eat_hoard[1,(1:t)])/(total_alive[1,(1:t)])*100), ylim=c(0, 100), ylab='%', xlab='Timestep', main='Percentage of alive birds eat-hoarding', type='l')
          
          # 7: To show predation
          plot9<-plot(1:t, (total_predated[1,(1:t)]), ylim=c(0, 5), ylab='# killed by predation', xlab='Timestep', main='Number of birds killed by predation', type='l')
          
          # 10 total forage 
          plot10<<-plot(1:t, ((total_forage[1,(1:t)])/(total_alive[1,(1:t)])*100), ylim=c(0, 100), ylab='%', xlab='Timestep', main='Percentage of alive birds foraging (ret/eat/eat-hoard)', type='l')
          
          
          mtext((paste('Days=', days, '_N=', N, 'Daylight_h=', daylight_h,  '_th-fr=', th_forage_fr)), side=3, cex=0.8,line=-2, outer=TRUE)
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
      if (exists('sim_type')){                                                      # Check if you are optimising or just running 
        if(sim_type=='run_model'){                                                      # If so, are you optimising th_sc? 
          print(paste0('Model 1.3 ran'))
        }
        if(sim_type=='run_opt'){                                                      # Or are you optimising th_fr? 
          print(paste0('Model 1.3 optimization ran'))
        }
        
      }
      
      
      if(sim_type=='run_model'){
        #print(paste0('ready to save MOd1.3 simulation plots'))
        
        # SAVE LINE PLOTS 
        setwd(paste0(mainDir, '/1-run_model//')) # set current wd 
        dev.print(pdf, (paste0('Run_MOD_1_3_days=', days, '_N=', N, 'env=', env_type, '_th-fr=', th_forage_fr, '_th-sc1=', th_forage_sc1, '_th-sc2=', th_forage_sc2, '_', 'Daylight_h=', daylight_h, '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))
        
      } # end loop for run_model
      
    } # end the 1.3 function 
    
        # Run it
        dev.new()
        MOD_1_3_func(days=30, N=100, env_type=8, th_forage_sc1=0.3, th_forage_sc2=0.1, th_forage_fr=1,  daylight_h=8, sim_type = 'run_model')
        
    # Write function for optimisation 
    MOD_1_3_opt_thsc1_thsc2<-function(days, N, env_type, th_sc1_min, th_sc1_max, th_sc2_min, th_sc2_max, daylight_h, sim_type){
      
      # open the right windows, but only if you're running the function by itself 
      if (sim_type =='run_opt'){
        dev.new()
        par(mfrow=c(1,1))
      }
      
      # show that optimizatio started 
      print(paste0('Optimizing MOD 1.3 for sc-th1 and sc-th2' ))
      
      # creates 100 values between min and max, evenly spaced 
      th_forage_sc1<<-linspace(th_sc1_min, th_sc1_max, n=50)
      th_forage_sc2<<-linspace(th_sc2_min, th_sc2_max, n=50)
      
      # now create a space to save the survival for each different value fo th_forage_sc1 and th_forage_sc2 
      survival_end<<-matrix(NA, length(th_forage_sc1), length(th_forage_sc2))
      
      
      for (th_sc1 in 1:length(th_forage_sc1)){          # Outside for loop that goes through all values of forage_sc1 
        # determine the current threshold for each loop 
      
        current_th_sc1<<-th_forage_sc1[th_sc1]
        # now run through all the possible sc2 values for this specific sc1
        for (th_sc2 in 1:length(th_forage_sc2)){
          # set the current sc2 threshold 
          current_th_sc2<<-th_forage_sc2[th_sc2]
          
          # We can assume that any case where TH2 is lower than TH1 is not logical: Birds would never forage, only retrieve 
          # So, eleminate those options 
          if (current_th_sc2 > current_th_sc1){
              # run the MOD 1.3 function: 
              MOD_1_3_func(days, N, env_type, th_forage_sc1=current_th_sc1, th_forage_sc2=current_th_sc2, th_forage_fr, daylight_h, sim_type = sim_type)
              # add to the previously created matrix 
              survival_end[th_sc1,th_sc2]<<-birds_alive_at_end
          }else {
            # Just give this teh value '0' as this is not a relevant option that we need to explore 
            survival_end[th_sc1, th_sc2]<<-0
          }
          print(paste('opt MOD 1.3 for sc1=', current_th_sc1, ' and sc2=', current_th_sc2))
        } # end of loop for sc2 thresholds 
      } # end of loop for sc1 thesholds 
      
      # The matrix should be completely filled in now and ready to do 
      
      # for checking during coding 
      print(paste0('Optimization MOD 1.3 ran' ))
      
      # plot it so you can visualise
      # This is the simple lplot, not the html one 
      #dev.new()
      #par(mfrow=c(1,1))
      persp3D(z=survival_end, xlab='th_sc1', ylab='th_sc2', zlab='survival', main='Optimal survival for th_sc1 and th_sc2', zlim= c(0, 1))
      
      # setwd 
      setwd(paste0(mainDir, '/2-run_opt//'))
      # Use the other way of plotting 3D plots 
      fig_MOD_1_3<-plot_ly(
        x=as.numeric(th_forage_sc2), 
        y=as.numeric(th_forage_sc1), 
        z=survival_end
      )
      fig_MOD_1_3<-fig_MOD_1_3 %>% add_surface()
      fig_MOD_1_3<-fig_MOD_1_3 %>% layout(
        title=list(text=paste0('Opt MOD 1.3 th_Sc1 and th_sc2 for:T=', days, ', N=', N, ', env=', env_type ), y=0.95),
        scene=list(
          xaxis=list(title= 'Threshold sc2 (gram)'),
          yaxis=list(title= 'Threshold Sc1 (gram)'),
          zaxis=list(title= 'Survival prob'
          )))
      fig_MOD_1_3
      
      # SAVE THE WIDGET 
      # The saveWidget function has trouble saving in new directories and sometimes doesnt delete the temporary files
      # I found this code that should get rid of it (works so far )
      # Function that warns you when you are overwriting 
      save_widget_wrapper <- function(plot, file, overwrite = FALSE){
        # save the file if it doesn't already exist or if overwrite == TRUE
        if( !file.exists(file) | overwrite ){
          withr::with_dir(new = dirname(file), 
                          code = htmlwidgets::saveWidget(plot, 
                                                         file = basename(file)))
        } else {
          print("File already exists and 'overwrite' == FALSE. Nothing saved to file.")
        }
      }
      
      
      
      #setwd(paste0(mainDir, '/run_opt//'))
      
      # create  seperate timesamp so the supproting folders don't have a differen tone than the html file 
      Fig_timestamp<-format(Sys.time(), "%Y_%m_%d__%H_%M_%S")
      Filename<-paste0('MOD_1_3_opt_3D','_T', days, '_N',N, '_env',env_type,'_', Fig_timestamp,'.html')
      #Filename<-paste0(Fig_timestamp,'.html')
      save_widget_wrapper(fig_MOD_1_3, Filename)
      
    } # end of optimization function for hoarding bird th-sc1 and th-sc2
    
        # run it 
        MOD_1_3_opt_thsc1_thsc2(days=30, N=10, env_type=8, th_sc1_min=0, th_sc1_max=0.4, th_sc2_min=0, th_sc2_max=0.4, daylight_h=8, sim_type = 'run_opt')
        
    ###############################
    #    Environments loop  1.3   # 
    ###############################
    
    MOD_1_3_env_loop_func<-function(days, N, th_forage_sc1, th_forage_sc2, th_forage_fr, daylight_h, sim_type){
      # Run the model 1.3 function for each of the environments
      # prepare list 
      survival_plot_list<<-list()
      # Set up plotting window 
      dev.new()
      par(mfrow=c(1,1))
      # Start loop environmnets 
      for (i in 1:18){
        
        # For every environment run the optimisation function
        cur_env_type<<-i
        #MOD_1_1_opt_th_sc(days=30, N=100, env_typ=cur_env_type, th_forage_fr=1, noplot=1, hoard_on=0, daylight_h=8 , th_sc_min=0, th_sc_max=0.4)
        MOD_1_3_func(days=days, N=N, env_type=cur_env_type, th_forage_sc1=th_forage_sc1, th_forage_sc2=th_forage_sc2, th_forage_fr=th_forage_fr, daylight_h=daylight_h, sim_type = sim_type)
        
        # create temporary dataframe for ggplot
        current_survival_df<<-as.data.frame(t(rbind(total_alive, (1:TS))))
        # make sure that the column depicts a percentage 
        current_survival_df$perc_survival<<-((current_survival_df$V1/N)*100)
        # plot 
        current_survival_plot<<-ggplot(current_survival_df, aes(x=V2, y=perc_survival))+
          geom_line()+
          labs(
            title = paste('Survival - % birds alive - Environment =', cur_env_type),
            y='% Alive',
            x='Timestep')+
          ylim(0,101)
        
        # Add to the list with plots 
        survival_plot_list[[i]]<<-current_survival_plot
        
        # for info
        print(paste('environment loop 1.3 done for env=', cur_env_type))
        
      } # end for loop for the environments
      
      # now plot all of this
      #dev.new() # new window
      do.call('grid.arrange', c(survival_plot_list, ncol=3)) # aggregate the plots
      # set wd 
      setwd(paste0(mainDir, '/3-env_loop//')) # set current wd 
      # Now save it 
      dev.print(pdf, (paste0('Sim_env_loop_MOD_1_3', '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))
      
    } # end of 1.3 environment loop 
    
    # Run it 
    MOD_1_3_env_loop_func(days = 30, N= 10, th_forage_sc1 = 0.1, th_forage_sc2 = 0.3, th_forage_fr = 1, daylight_h = 8, sim_type = 'env_loop')
    
    
    ###########################################
    #      Optimization loop model  1.3       # 
    ###########################################
    
    
    # Start writing a function 
    MOD_1_3_opt_loop_func<-function(days, N, th_sc1_min, th_sc1_max, th_sc2_min, th_sc2_max, daylight_h, sim_type){
      
      # open a new window
      dev.new()
      # with the right outlines
      par(mfrow=c(6,3))
      # create an empty object to put the values  of th_sc2 and th_sc1 for max survival in 
      # These can be used in the behaviour loop
      mat_max_survival_th_sc1_sc2<<-matrix(NA, 18, 2) 
      print('code working here')
      
      # START THE FOR LOOP THROUGH EACH OF THE ENVIRONMENTS
      for (i in 1:18){
        if (i==1){
          # create an empty list to put the optimisation plots in
          opt_loop_1_3_df_list<<-list()
        }
        # For every environment run the optimisation function
        cur_env_type<<-i

        #run the function 
        MOD_1_3_opt_thsc1_thsc2(days=days, N=N, env_type=cur_env_type, th_sc1_min=th_sc1_min, th_sc1_max=th_sc1_max, th_sc2_min=th_sc2_min, th_sc2_max=th_sc2_max, daylight_h=daylight_h, sim_type = sim_type)
        
        # At the end of each optimization (for each of the environments) --> there is one matrix names 'survival_end'
        # Rows: th_sc1 
        # Columns: th_sc2 
        # Cell values: survival at the end of the model run (30 days in most cases)
        
        # confirm
        print(paste('Optimization ran for environment ', cur_env_type))
        #print(paste('The optimal FR-th for this environment = ', current_max_th))
        
        current_opt_df<<-survival_end
        opt_loop_1_3_df_list<<-append(opt_loop_1_3_df_list, list(current_opt_df))
        
        # EXTRACT THRESHOLD VALUES FOR OPTIMAL SURVIVAL  
        #current max is: 
        cur_max_surv<<-max(current_opt_df)
        # What is the location of the current max? 
        cur_location<<-which(current_opt_df == max(current_opt_df), arr.ind = TRUE)
        # There could be a problem here, if there are multiple maxima/or everything is 0 (common when no birds survive)
        # The code will take the middle value of the th-sc1 and th-sc2 
        # It wil take the average column/row number and round this up to the closest whole number. 
        # This will be the location of the threshold used 
        # What is the current optimal th-sc1 
        cur_opt_th_sc1<<-th_forage_sc1[(round(mean(cur_location[,1])))]
        #What is the current optimal th_sc2
        cur_opt_th_sc2<<-th_forage_sc2[(round(mean(cur_location[,2])))]
        # now pop the values into the previously made df 
        mat_max_survival_th_sc1_sc2[i, 1]<<-cur_opt_th_sc1
        mat_max_survival_th_sc1_sc2[i, 2]<<-cur_opt_th_sc2
        
        
      } # end of the loop for each environment
      
      # Save the whole thing 
      # Set the wd 
      setwd(paste0(mainDir, '/4-opt_loop//'))
      # save the big images 
      dev.print(pdf, (paste0('Sim_1_3_opt_loop_days=', days, '_N=', N,'sc1min', th_sc1_min, '_sc1max', th_sc1_max, '_sc2min', th_sc2_min, '_sc2max', th_sc2_max,  '_', 'Daylight_h=', daylight_h, '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))
      
      
    } # end mod 1.3 opt_loop function 
    
    # Run it 
    MOD_1_3_opt_loop_func(days = 30, N = 100, th_sc1_min=0, th_sc1_max=0.4, th_sc2_min=0, th_sc2_max=0.4, daylight_h=8, sim_type = 'opt_loop')
    
    ###### To do next: 
    # Get the threshold values for max survival printed on graph - might be difficult. Graph code is in the opt code -_- (30/01/2023) --> but I did put it in the beh-loop
    # save them in a dataframe/matrix - DONE 30/01/23
    # Then run the beh-loop with this - DONE 30/01/23
    
    
    ############################################
    #      Behaviour loop - SC-TH 1.3          # 
    ############################################
    
    # Write the function 
    MOD_1_3_beh_loop_func<<-function(days, N, th_forage_fr, daylight_h, sim_type){
      # Open a new plotting area 
      # dev.new()
      # par(mfrow=c(6,3))
      
      # Loop for each environment 
      for (i in 1:18){
        # Make some lists to fill 
        if (i==1){
          stacked_chart_data_list<<-list()
          stacked_chart_plot_list<<-list()
          fr_sc_plot_list<<-list()
          survival_plot_list<<-list()
        }
        mat_cur_perc_rest<<-matrix(data=NA, nrow= (days*72), ncol=1)
        mat_cur_perc_for<<-matrix(NA, TS, 1)
        mat_cur_perc_sleep<<-matrix(NA, TS, 1)
        # indicate the current environment
        cur_env_type<<-i
        
        # Take the sc-th from the optimisation that had maximum survival 
        current_opt_th_sc1<<-mat_max_survival_th_sc1_sc2[i,1]
        current_opt_th_sc2<<-mat_max_survival_th_sc1_sc2[i,2]
        
        
        # Now run the model 1.3 with these values 
        MOD_1_3_func(days=days, N=N, env_type=i, th_forage_sc1=current_opt_th_sc1, th_forage_sc2=current_opt_th_sc2, th_forage_fr=th_forage_fr,  daylight_h=daylight_h, sim_type = sim_type)
        
        
        # GRAPHS TO SHOW SUVIVAL TRAJECTORIES 
            # For the graphs that display the survival trajectories throughout 30 days in each env 
            # create temporary dataframe for ggplot 
            current_survival_df<<-as.data.frame(t(rbind(total_alive, (1:TS))))
            # make percentages
            current_survival_df$perc_survival<<-((current_survival_df$V1/N)*100)
            # plot it 
            current_survival_plot<<-ggplot(current_survival_df, aes(x=V2, y=perc_survival))+
              geom_line(size= 1)+
              labs(
                title = paste('% survival - Env=', cur_env_type), 
                y='% Alive', 
                x='Timestep')+
              ylim(0,100)+
              annotate("text", x=1500, y=90, label= paste("th_sc1=", round(current_opt_th_sc1, digits = 2), ' & th_sc2=', round(current_opt_th_sc2, digits = 2))) 
            # pop the plot in the list 
            survival_plot_list<<-append(survival_plot_list, list(current_survival_plot))
        
        
        # GRAPHS TO SHOW THE BEHAVIOUR TRAJECTORIES   
            # Once you have hte matrices, calculate this for every timestep 
            for (j in (1:TS)){
              # The percentage resting 
              mat_cur_perc_rest[j,1]<<-((total_rest[1,j]/total_alive[1,j])*100)
              # The percentage foraging 
              mat_cur_perc_for[j,1]<<-((total_forage[1,j]/total_alive[1,j])*100)
              # the percentage sleeping 
              mat_cur_perc_sleep[j,1]<<-((total_sleep[1,j]/total_alive[1,j])*100)
            }
            # Add column with numbers 
            timesteps<<-1:TS
            # put them on a daily scale 
            timesteps_dayscale<<-timesteps%%72
            # Attach matrices 
            mat_perc_cur_env<<-cbind(mat_cur_perc_rest, mat_cur_perc_for, mat_cur_perc_sleep, timesteps_dayscale)
            # turn to df 
            df_perc_cur_env<<-as.data.frame(mat_perc_cur_env)
            # set names 
            colnames(df_perc_cur_env)[1]<<-'rest'
            colnames(df_perc_cur_env)[2]<<-'forage'
            colnames(df_perc_cur_env)[3]<<-'sleep'
            # now start grouping 
            rest_perc<<-group_by(df_perc_cur_env, timesteps_dayscale) %>% summarize (m=mean(rest))
            forage_perc<<-group_by(df_perc_cur_env, timesteps_dayscale) %>% summarize (m=mean(forage))
            sleep_perc<<-group_by(df_perc_cur_env, timesteps_dayscale) %>% summarize (m=mean(sleep))
            # add group
            rest_perc$beh<<-rep('rest')
            forage_perc$beh<<-rep('forage')
            sleep_perc$beh<<-rep('sleep')
            # make new dataframe 
            df_for_chart<<-rbind(rest_perc, forage_perc, sleep_perc)
            # Ideally, I'd store this in some sort of list so I can access it afterwards 
            stacked_chart_data_list<<-append(stacked_chart_data_list, list(df_for_chart))
            # I want to plot only the time that the birds are awake (they all go to sleep at night anyway)
            # calculate the # of timesteps that birds are awake and put this in the xlim of the graphs 
            timesteps_awake<<-daylight_h*3
            # Now make the chart 
            cur_stacked_plot<<-ggplot(df_for_chart, aes(x=timesteps_dayscale, y=m, fill=beh))+
              geom_area(alpha=0.8, size=0.5, colour='white')+
              scale_fill_viridis(discrete = T)+
              #theme_ipsum()+
              #ggtitle('Percentage of Birds per Behaviour')
              labs(
                title = paste('Env.', i, " th_sc1=", round(current_opt_th_sc1, digits = 2), ' & th_sc2=', round(current_opt_th_sc2, digits = 2)), 
                x='Timestep in a 24 day (20 min increments)', 
                y='Mean % of Alive birds')+
              xlim(0, timesteps_awake)
              #annotate("text", x=1500, y=90, label= paste("th_sc1=", current_opt_th_sc1, ' & th_sc2=', current_opt_th_sc2))  
              # put plot in the list 
              stacked_chart_plot_list<<-append(stacked_chart_plot_list, list(cur_stacked_plot))
        
        # GRAPHS TO SHOW THE FR AND SC TRAJECTORIES 
            # create a df
            fr_sc_graph<<-rbind(fr_mean, sc_mean, timesteps_dayscale)
            fr_sc_graph<<-t(fr_sc_graph)
            # turn to df
            fr_sc_graph<<-as.data.frame(fr_sc_graph)
            # set names
            colnames(fr_sc_graph)[1]<<-'fr'
            colnames(fr_sc_graph)[2]<<-'sc'
            # start grouping
            # now start grouping
            fr_grouped<<-group_by(fr_sc_graph, timesteps_dayscale) %>% summarize (m=mean(fr))
            sc_grouped<<-group_by(fr_sc_graph, timesteps_dayscale) %>% summarize (m=mean(sc))
            # add group
            fr_grouped$type<<-rep('fr')
            sc_grouped$type<<-rep('sc')
            #sleep_perc$beh<-rep('sleep')
            # make new dataframe
            df_for_sc_fr_chart<<-rbind(fr_grouped, sc_grouped)
            # graph
            # Now make the chart
            cur_fr_sc_plot<<-ggplot(df_for_sc_fr_chart, aes(x=timesteps_dayscale, y=m, col=type))+
              #geom_area(alpha=0.8, size=0.5, colour='white')+
              geom_line(size = 1)+
              scale_fill_viridis(discrete = T)+
              #theme_ipsum()+
              #ggtitle('Percentage of Birds per Behaviour')
              labs(
                title = paste('Env.', i, " th_sc1=", round(current_opt_th_fr1, digits = 2), ' & th_sc2=', round(current_opt_th_fr2, digits = 2)),
                x='Timestep in a 24 day (20 min increments)',
                y='FR and SC (gram)')+
              xlim(0, timesteps_awake)+
              ylim(0,5)
            # put plot in the list
            fr_sc_plot_list<<-append(fr_sc_plot_list, list(cur_fr_sc_plot))
            
            # for ease of use 
            print(paste('Code for the stacked area graphs/sc-fr graphs is done for env=', cur_env_type))
            
      } # END FOR LOOP ENVIRONTMENTS 
      
      
    } # end function MOD 1.3 behaviour loop 
    
        # Run it 
        MOD_1_3_beh_loop_func(days = 30, N = 100, th_forage_fr = 1, daylight_h = 8, sim_type = 'beh_loop')
        
        
        # Plot all 3 the graph panels
        setwd(paste0(mainDir, '/5-beh_loop//')) # set current wd 
        #dev.new()
        #par(mfrow=c(6,3))
        do.call('grid.arrange', c(survival_plot_list, ncol=3))
        dev.print(pdf, (paste0('beh_loop_surv_1_3',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))
        # beh
        do.call('grid.arrange', c(stacked_chart_plot_list, ncol=3)) # aggregate the plots
        dev.print(pdf, (paste0('beh_loop_beh_traj_1_3',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))
        # fr and sc
        do.call('grid.arrange', c(fr_sc_plot_list, ncol=3)) # aggregate the plots
        dev.print(pdf, (paste0('beh_loop_sc_fr_1_3',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))
        
    
    
#################################################################
##   Model 1.4: Hoarding bird, Access to Fat reserves          ##
#################################################################        
    
    ################################ 
    #   set up directories   1.4   # 
    ################################
        # Set up the main directory for where you want the figures saved 
        # This can be replaced by any folder you have on your computer (just make sure you have continuous connection if its a webfolder)
        mainDir_1_4<-'C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Figures/5-combi_model/MOD_1_4'
        setwd(mainDir_1_4)
        # Run the following if doing this for the first time on devide: 
        # create list of folders that we want present 
        folders<-c('1-run_model', '2-run_opt', '3-env_loop', '4-opt_loop', '5-beh_loop')
        # Check if they exist and if not, create them 
        # Note that this code will warn you if it already existed 
        for (folder in folders){
          dir.create(file.path(mainDir_1_4, folder ), showWarnings = TRUE)
        }    
        
    ###############################
    #    Functions & running 1.4  #
    ###############################
        
      # write function for the  model
      MOD_1_4_func<-function(days, N, env_type, th_forage_fr1, th_forage_fr2, th_forage_sc, daylight_h, sim_type){
        
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
          }else{
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
            } else{
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
              } else{
                
                # NON SLEEPING BIRDS START HERE : >>>>>>>>>
                # set the sleeping matrix to 0 
                sleep_count[i,t]<<-0
                
                # Check what behavior the bird should do if it is day 
                # CHECK IF BIRD IS HUNGRY AND NEEDS FOOD
                
                # Time to forage: 
                
                ##################################
                #####  CHANGE FOR MODEL 1.4 ###### 
                ##################################
                
                # Only access to stomach-content 
                # The lowest threshold determines if the bird will retrieve 
                # The hightes threshold determines if the bird will rest 
                
                if ((mat_fr[i,t]) <= th_forage_fr2){
                  # If this is the case, the bird is hungry and needs to forage for food 
                  # Resting is not an option here 
                  
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
                  
                  
                  # START IF STATEMENT FOR HOARDING BIRDS           
                  
                  # WHAT KIND OF FORAGING IS HAPPENING?
                  # 3 kinds of foraging are possible:
                  #       1. Retrieve stored item
                  #       2. Find new item and eat it (done after this)
                  #       3. Find new item and eat it till full, hoard after this
                  
                  
                  ####################
                  ##   RETRIEVING   ## 
                  ####################
                  
                  # retrieving happens when the stomach content is below the lowest threshold (sc-th1)
                  # The bird also needs to have the minimum number of caches to allow retrieval 
                  if ((mat_fr[i,t]<=th_forage_fr1)&& (mat_caches[i,t]>retrieve_min)){
                    
                    # code checking (temp for debugging)
                    #print(paste0('bird ', N, ' is retrieving'))
                    
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
                    # end of retrieving birds 
                  } else{
                    
                    #######################
                    #   NORMAL FORAGING   #
                    #######################
                    
                    # If the bird is foraging, but not retrieving, it will eat-hoard or eat the food
                    # Either way, it will need to find food first
                    
                    # code checking
                    #print(paste('bird ', N, 'is foraging not retrieving'))
                    
                    # update the global counting variable
                    retrieve_count[i,t]<<-0
                    
                    # now round this up/down to the closest number of items (a bird cannot find half items)
                    # then move this back to grams
                    #food_g_found<<-(round(food_g_found/food_item))
                    #food_g_found<<-(food_g_found*food_item)
                    # Food is found, we need to check how much it is and if the bird will hoard the surpluss
                    
                    # Run the forage function and decide what number of items is found
                    # The outcoem here is 'food_item_found'
                    forage_function(num_food_mean, prob_b_forage, b_size)
                    
                    # Pop this in the matrix (this is in number of items found)
                    mat_find_food[i,t]<<-food_item_found
                    # convert to grams
                    food_item_found_gram<<-(food_item_found*food_item)
                    
                    # Update agent-owned variables:
                    # First, increase the stomach content with whatever food is found
                    mat_sc[i,(t)]<<-(mat_sc[i,t])+(food_item_found_gram)
                    
                    # now check if this exceeds the stomach size
                    if (mat_sc[i,(t)]>stom_size){
                      # This means the bird found more than it can eat
                      # It will hoard the surpluss
                      
                      ######################
                      #    EAT-HOARD       #
                      ######################
                      
                      # code checking
                      #print(paste0('bird ', i, ' is eat-hoarding'))
                      
                      # update the global counters
                      eat_hoard_count[i,t]<<-1
                      eat_count[i,t]<<-0
                      
                      # update agent-owned variables
                      hoard_surplus<<-floor((mat_sc[i,(t)]-stom_size)/food_item)  # Determine The surplus available in whole food items
                      mat_sc[i,(t)]<<-stom_size                                   # The stomach is set to the stomach size (full)
                      mat_caches[i,t]<<-(mat_caches[i,t])+hoard_surplus
                      
                      # update BMR multi
                      BMR_multi<<-8
                      # end of eat-hoard if-statement 
                    } else{
                      # This means the food eaten does not exceed teh stomach size
                      # no hoarding required, the bird will just eat
                      
                      #############
                      #   EAT     #
                      #############
                      
                      # code cdhecking
                      #print(paste0('bird ', i, ' is eating'))
                      # update the global counters
                      eat_hoard_count[i,t]<<-0
                      eat_count[i,t]<<-1
                      
                      # Stomach content is already updated
                      
                      # Update BMR multi
                      BMR_multi<<-8
                      
                    } # end of the eat statement
                    
                  } # end of forage but not retrieving statement
               
                  # ends the foraging code (below the sc-th2)
                } else{
                  ##################
                  #    RESTING     # 
                  ##################
                  # All birds of which the th-sc is above th2 should go rest 
                  
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
              }else{
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
                } else{
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
          total_predated[1,t]<<-sum(predation_count[,t], na.rm=TRUE)        # how many birds were killed by predation in this timestep 
          total_sleep[1,t]<<-sum(sleep_count[,t], na.rm = TRUE)             # same for sleep
          
          
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
          if ((t/plot_interval)==floor(t/plot_interval) && sim_type == 'run_model' ){
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
            
            # 6 Percentage of birds that are resting (of the alive birds)
            plot6<<-plot(1:t, ((total_rest[1,(1:t)])/(total_alive[1,(1:t)])*100), ylim=c(0, 100), ylab='%', xlab='Timestep', main='Percentage of alive birds resting', type='l')
            
            # 7
            plot7<<-plot(1:t, ((total_retrieve[1,(1:t)])/(total_alive[1,(1:t)])*100), ylim=c(0, 100), ylab='%', xlab='Timestep', main='Percentage of alive birds retrieving', type='l')
            
            # 8
            plot8<<-plot(1:t, ((total_eat_hoard[1,(1:t)])/(total_alive[1,(1:t)])*100), ylim=c(0, 100), ylab='%', xlab='Timestep', main='Percentage of alive birds eat-hoarding', type='l')
            
            # 7: To show predation
            plot9<-plot(1:t, (total_predated[1,(1:t)]), ylim=c(0, 5), ylab='# killed by predation', xlab='Timestep', main='Number of birds killed by predation', type='l')
            
            # 10 total forage 
            plot10<<-plot(1:t, ((total_forage[1,(1:t)])/(total_alive[1,(1:t)])*100), ylim=c(0, 100), ylab='%', xlab='Timestep', main='Percentage of alive birds foraging', type='l')
            
            
            mtext((paste('mod 1.4 Days=', days, '_N=', N, 'Daylight_h=', daylight_h,  '_th-sc=', th_forage_sc, 'env = ', env_type)), side=3, cex=0.8,line=-2, outer=TRUE)
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
        if (exists('sim_type')){                                                      # Check if you are optimising or just running 
          if(sim_type=='run_model'){                                                      # If so, are you optimising th_sc? 
            print(paste0('Model 1.4 ran'))
          }
          if(sim_type=='run_opt'){                                                      # Or are you optimising th_fr? 
            print(paste0('Model 1.4 optimization ran'))
          }
          
        }
        
        
        if(sim_type=='run_model'){
          #print(paste0('ready to save MOd1.3 simulation plots'))
          
          # SAVE LINE PLOTS 
          setwd(paste0(mainDir_1_4, '/1-run_model//')) # set current wd 
          dev.print(pdf, (paste0('Run_MOD_1_4_days=', days, '_N=', N, 'env=', env_type, '_th-sc=', th_forage_sc, '_th-fr1=', th_forage_fr1, '_th-fr2=', th_forage_fr2, '_', 'Daylight_h=', daylight_h, '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))
          
        } # end loop for run_model
        
      } # end the 1.4 function 
      
          # Run it
          dev.new()
          MOD_1_4_func(days=30, N=100, env_type=8, th_forage_fr1=1, th_forage_fr2=1.5, th_forage_sc=0.2,  daylight_h=8, sim_type = 'run_model')
      
      # Write function for optimisation 
      MOD_1_4_opt_thfr1_thfr2<-function(days, N, env_type, th_fr1_min, th_fr1_max, th_fr2_min, th_fr2_max, daylight_h, sim_type){
        
        # open the right windows, but only if you're running the function by itself 
        if (sim_type =='run_opt'){
          dev.new()
          par(mfrow=c(1,1))
        }
        
        # show that optimizatio started 
        print(paste0('Optimizing MOD 1.4 for fr-th1 and fr-th2' ))
        
        # creates 100 values between min and max, evenly spaced 
        th_forage_fr1<<-linspace(th_fr1_min, th_fr1_max, n=25)
        th_forage_fr2<<-linspace(th_fr2_min, th_fr2_max, n=25)
        
        # now create a space to save the survival for each different value fo th_forage_fr1 and th_forage_fr2 
        survival_end<<-matrix(NA, length(th_forage_fr1), length(th_forage_fr2))
        
        
        for (th_fr1 in 1:length(th_forage_fr1)){          # Outside for loop that goes through all values of forage_fr1 
          # determine the current threshold for each loop 
          current_th_fr1<<-th_forage_fr1[th_fr1]
          # now run through all the possible fr2 values for this specific fr1
          for (th_fr2 in 1:length(th_forage_fr2)){
            # set the current fr2 threshold 
            current_th_fr2<<-th_forage_fr2[th_fr2]
            
            # Again: only run for combinations that make sense 
            if (current_th_fr2 > current_th_fr1){
                # run the MOD 1.4 function: 
                MOD_1_4_func(days, N, env_type, th_forage_fr1=current_th_fr1, th_forage_fr2=current_th_fr2, th_forage_sc, daylight_h, sim_type = sim_type)
                # add to the previously created matrix 
                survival_end[th_fr1,th_fr2]<<-birds_alive_at_end
            }else{
              # Just set this to 0, as this is not a feasible option to explore 
              survival_end[th_fr1, th_fr2]<<-0
            }
            print(paste('opt MOD 1.4 for fr1=', current_th_fr1, ' and fr2=', current_th_fr2))
          } # end of loop for fr2 thresholds 
        } # end of loop for fr1 thesholds 
        
        # The matrix should be completely filled in now and ready to do 
        
        # for checking during coding 
        print(paste0('Optimization MOD 1.4 ran' ))
        
        # plot it so you can visualise
        # This is the simple lplot, not the html one 
        #dev.new()
        #par(mfrow=c(1,1))
        persp3D(z=survival_end, xlab='th_fr1', ylab='th_fr2', zlab='survival', main='Optimal survival for th_fr1 and th_fr2', zlim= c(0, 1))
        
        # setwd 
        setwd(paste0(mainDir_1_4, '/2-run_opt//'))
        # Use the other way of plotting 3D plots 
        fig_MOD_1_4<-plot_ly(
          x=as.numeric(th_forage_fr2), 
          y=as.numeric(th_forage_fr1), 
          z=survival_end
        )
        fig_MOD_1_4<-fig_MOD_1_4 %>% add_surface()
        fig_MOD_1_4<-fig_MOD_1_4 %>% layout(
          title=list(text=paste0('Opt MOD 1.4 th_fr1 and th_fr2 for:T=', days, ', N=', N, ', env=', env_type ), y=0.95),
          scene=list(
            xaxis=list(title= 'Threshold fr2 (gram)'),
            yaxis=list(title= 'Threshold fr1 (gram)'),
            zaxis=list(title= 'Survival prob'
            )))
        fig_MOD_1_4
        
        # SAVE THE WIDGET 
        # The saveWidget function has trouble saving in new directories and sometimes doesnt delete the temporary files
        # I found this code that should get rid of it (works so far )
        # Function that warns you when you are overwriting 
        save_widget_wrapper <- function(plot, file, overwrite = FALSE){
          # save the file if it doesn't already exist or if overwrite == TRUE
          if( !file.exists(file) | overwrite ){
            withr::with_dir(new = dirname(file), 
                            code = htmlwidgets::saveWidget(plot, 
                                                           file = basename(file)))
          } else {
            print("File already exists and 'overwrite' == FALSE. Nothing saved to file.")
          }
        }
        
        
        
        #setwd(paste0(mainDir, '/run_opt//'))
        
        # create  seperate timesamp so the supproting folders don't have a differen tone than the html file 
        Fig_timestamp<-format(Sys.time(), "%Y_%m_%d__%H_%M_%S")
        Filename<-paste0('MOD_1_4_opt_3D','_T', days, '_N',N, '_env',env_type,'_', Fig_timestamp,'.html')
        #Filename<-paste0(Fig_timestamp,'.html')
        save_widget_wrapper(fig_MOD_1_4, Filename)
        
      } # end of optimization function for hoarding bird th-fr1 and th-fr2
      
        # run it 
        MOD_1_4_opt_thfr1_thfr2(days = 30, N = 100, env_type = 9, th_fr1_min = 1, th_fr1_max = 3, th_fr2_min = 0, th_fr2_max = 4, daylight_h = 8, sim_type = 'run_opt')
      
      
    ###############################
    #    Environments loop  1.4   # 
    ###############################
    
    # I dont think the environments loop is adding much. 
    # Add later if needed 

      
    ###########################################
    #      Optimization loop model  1.4      # 
    ###########################################
      
      # Start writing a function 
      MOD_1_4_opt_loop_func<-function(days, N, th_fr1_min, th_fr1_max, th_fr2_min, th_fr2_max, daylight_h, sim_type){

        # create an empty object to put the values  of th_fr2 and th_fr1 for max survival in 
        # These can be used in the behaviour loop
        mat_max_survival_th_fr1_fr2<<-matrix(NA, 18, 2) 
        
        # START THE FOR LOOP THROUGH EACH OF THE ENVIRONMENTS
        for (i in 1:18){
          if (i==1){
            # create an empty list to put the optimisation plots in
            opt_loop_1_4_df_list<<-list()
          }
          # For every environment run the optimisation function
          cur_env_type<<-i
          #run the function 
          MOD_1_4_opt_thfr1_thfr2(days=days, N=N, env_type=cur_env_type, th_fr1_min=th_fr1_min, th_fr1_max=th_fr1_max, th_fr2_min=th_fr2_min, th_fr2_max=th_fr2_max, daylight_h=daylight_h, sim_type = sim_type)
          
          # At the end of each optimization (for each of the environments) --> there is one matrix names 'survival_end'
          # Rows: th_sc1 
          # Columns: th_sc2 
          # Cell values: survival at the end of the model run (30 days in most cases)
          
          # confirm
          print(paste('Optimization ran for environment ', cur_env_type))
          #print(paste('The optimal FR-th for this environment = ', current_max_th))
          
          current_opt_df<<-survival_end
          # saves the data of the optimisations 
          opt_loop_1_4_df_list<<-append(opt_loop_1_4_df_list, list(current_opt_df))
          
          # EXTRACT THRESHOLD VALUES FOR OPTIMAL SURVIVAL  
          #current max is: 
          cur_max_surv<<-max(current_opt_df)
          # What is the location of the current max? 
          cur_location<<-which(current_opt_df == max(current_opt_df), arr.ind = TRUE)
          # There could be a problem here, if there are multiple maxima/or everything is 0 (common when no birds survive)
          # The code will take the middle value of the th-fr1 and th-fr2 
          # It wil take the average column/row number and round this up to the closest whole number. 
          # This will be the location of the threshold used 
          # What is the current optimal th-sc1 
          cur_opt_th_fr1<<-th_forage_fr1[(round(mean(cur_location[,1])))]
          #What is the current optimal th_sc2
          cur_opt_th_fr2<<-th_forage_fr2[(round(mean(cur_location[,2])))]
          # now pop the values into the previously made df 
          mat_max_survival_th_fr1_fr2[i, 1]<<-cur_opt_th_fr1
          mat_max_survival_th_fr1_fr2[i, 2]<<-cur_opt_th_fr2
          
          
        } # end of the loop for each environment
        
        # Save the whole thing 
        # Set the wd 
        setwd(paste0(mainDir_1_4, '/4-opt_loop//'))
        # save the big images 
        dev.print(pdf, (paste0('Sim_1_4_opt_loop_days=', days, '_N=', N,'fr1min', th_fr1_min, '_fr1max', th_fr1_max, '_fr2min', th_fr2_min, '_fr2max', th_fr2_max,  '_', 'Daylight_h=', daylight_h, '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))
        # save the dataframe with the optimal values 
        write.csv(mat_max_survival_th_fr1_fr2, paste0(mainDir_1_4, '/4-opt_loop//max_surv_matrix_1_4', '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.csv'), row.names=FALSE)
  
        
      } # end mod 1.4 opt_loop function 
          
          # open a new window
          dev.new()
          # with the right outlines
          par(mfrow=c(6,3))
          # Run it 
          MOD_1_4_opt_loop_func(days = 30, N = 50, th_fr1_min=0, th_fr1_max=4, th_fr2_min=0, th_fr2_max=4, daylight_h=8, sim_type = 'opt_loop')
          
    ############################################
    #      Behaviour loop - FR-TH 1.4          # 
    ############################################
          
          # Write the function 
          MOD_1_4_beh_loop_func<<-function(days, N, th_forage_sc, daylight_h, sim_type){
            # Open a new plotting area 
            dev.new()
            par(mfrow=c(6,3))
            
            # Loop for each environment 
            for (i in 1:18){
              # Make some lists to fill 
              if (i==1){
                stacked_chart_data_list<<-list()
                stacked_chart_plot_list<<-list()
                fr_sc_plot_list<<-list()
                survival_plot_list<<-list()
              }
              mat_cur_perc_rest<<-matrix(data=NA, nrow= (days*72), ncol=1)
              mat_cur_perc_for<<-matrix(NA, TS, 1)
              mat_cur_perc_sleep<<-matrix(NA, TS, 1)
              # indicate the current environment
              cur_env_type<<-i
              
              # Take the sc-th from the optimisation that had maximum survival 
              current_opt_th_fr1<<-mat_max_survival_th_fr1_fr2[i,1]
              current_opt_th_fr2<<-mat_max_survival_th_fr1_fr2[i,2]
              
              
              # Now run the model 1.3 with these values 
              MOD_1_4_func(days=days, N=N, env_type=i, th_forage_fr1=current_opt_th_fr1, th_forage_fr2=current_opt_th_fr2, th_forage_sc=th_forage_sc,  daylight_h=daylight_h, sim_type = sim_type)
              
              
              # GRAPHS TO SHOW SUVIVAL TRAJECTORIES 
                  # For the graphs that display the survival trajectories throughout 30 days in each env 
                  # create temporary dataframe for ggplot 
                  current_survival_df<<-as.data.frame(t(rbind(total_alive, (1:TS))))
                  # make percentages
                  current_survival_df$perc_survival<<-((current_survival_df$V1/N)*100)
                  # plot it 
                  current_survival_plot<<-ggplot(current_survival_df, aes(x=V2, y=perc_survival))+
                    geom_line(size = 1)+
                    labs(
                      title = paste('% survival 1.4 - Env=', cur_env_type), 
                      y='% Alive', 
                      x='Timestep')+
                    ylim(0,100)+
                    annotate("text", x=1500, y=90, label= paste("th_fr1=", round(current_opt_th_fr1, digits = 2), ' & th_fr2=', round(current_opt_th_fr2, digits = 2))) 
                  # pop the plot in the list 
                  survival_plot_list<<-append(survival_plot_list, list(current_survival_plot))
                  
              
              # GRAPHS TO SHOW THE BEHAVIOUR TRAJECTORIES   
                  # Once you have hte matrices, calculate this for every timestep 
                  for (j in (1:TS)){
                    # The percentage resting 
                    mat_cur_perc_rest[j,1]<<-((total_rest[1,j]/total_alive[1,j])*100)
                    # The percentage foraging 
                    mat_cur_perc_for[j,1]<<-((total_forage[1,j]/total_alive[1,j])*100)
                    # the percentage sleeping 
                    mat_cur_perc_sleep[j,1]<<-((total_sleep[1,j]/total_alive[1,j])*100)
                  }
                  # Add column with numbers 
                  timesteps<<-1:TS
                  # put them on a daily scale 
                  timesteps_dayscale<<-timesteps%%72
                  # Attach matrices 
                  mat_perc_cur_env<<-cbind(mat_cur_perc_rest, mat_cur_perc_for, mat_cur_perc_sleep, timesteps_dayscale)
                  # turn to df 
                  df_perc_cur_env<<-as.data.frame(mat_perc_cur_env)
                  # set names 
                  colnames(df_perc_cur_env)[1]<<-'rest'
                  colnames(df_perc_cur_env)[2]<<-'forage'
                  colnames(df_perc_cur_env)[3]<<-'sleep'
                  # now start grouping 
                  rest_perc<<-group_by(df_perc_cur_env, timesteps_dayscale) %>% summarize (m=mean(rest))
                  forage_perc<<-group_by(df_perc_cur_env, timesteps_dayscale) %>% summarize (m=mean(forage))
                  sleep_perc<<-group_by(df_perc_cur_env, timesteps_dayscale) %>% summarize (m=mean(sleep))
                  # add group
                  rest_perc$beh<<-rep('rest')
                  forage_perc$beh<<-rep('forage')
                  sleep_perc$beh<<-rep('sleep')
                  # make new dataframe 
                  df_for_chart<<-rbind(rest_perc, forage_perc, sleep_perc)
                  # Ideally, I'd store this in some sort of list so I can access it afterwards 
                  stacked_chart_data_list<<-append(stacked_chart_data_list, list(df_for_chart))
                  # I want to plot only the time that the birds are awake (they all go to sleep at night anyway)
                  # calculate the # of timesteps that birds are awake and put this in the xlim of the graphs 
                  timesteps_awake<<-daylight_h*3
                  # Now make the chart 
                  cur_stacked_plot<<-ggplot(df_for_chart, aes(x=timesteps_dayscale, y=m, fill=beh))+
                    geom_area(alpha=0.8, size=0.5, colour='white')+
                    scale_fill_viridis(discrete = T)+
                    #theme_ipsum()+
                    #ggtitle('Percentage of Birds per Behaviour')
                    labs(
                      title = paste('Mod 1.4 -Env.', i, " th_fr1=", round(current_opt_th_fr1, digits = 2), ' & th_fr2=', round(current_opt_th_fr2, digits = 2)), 
                      x='Timestep in a 24 day (20 min increments)', 
                      y='Mean % of Alive birds')+
                    xlim(0, timesteps_awake)
                    #annotate("text", x=1500, y=90, label= paste("th_sc1=", current_opt_th_sc1, ' & th_sc2=', current_opt_th_sc2))  
                    # put plot in the list 
                    stacked_chart_plot_list<<-append(stacked_chart_plot_list, list(cur_stacked_plot))
              
              # GRAPHS TO SHOW THE FR AND SC TRAJECTORIES 
                  # create a df
                  fr_sc_graph<<-rbind(fr_mean, sc_mean, timesteps_dayscale)
                  fr_sc_graph<<-t(fr_sc_graph)
                  # turn to df
                  fr_sc_graph<<-as.data.frame(fr_sc_graph)
                  # set names
                  colnames(fr_sc_graph)[1]<<-'fr'
                  colnames(fr_sc_graph)[2]<<-'sc'
                  # start grouping
                  # now start grouping
                  fr_grouped<<-group_by(fr_sc_graph, timesteps_dayscale) %>% summarize (m=mean(fr))
                  sc_grouped<<-group_by(fr_sc_graph, timesteps_dayscale) %>% summarize (m=mean(sc))
                  # add group
                  fr_grouped$type<<-rep('fr')
                  sc_grouped$type<<-rep('sc')
                  #sleep_perc$beh<-rep('sleep')
                  # make new dataframe
                  df_for_sc_fr_chart<<-rbind(fr_grouped, sc_grouped)
                  # graph
                  # Now make the chart
                  cur_fr_sc_plot<<-ggplot(df_for_sc_fr_chart, aes(x=timesteps_dayscale, y=m, col=type))+
                    #geom_area(alpha=0.8, size=0.5, colour='white')+
                    geom_line(size = 1)+
                    scale_fill_viridis(discrete = T)+
                    #theme_ipsum()+
                    #ggtitle('Percentage of Birds per Behaviour')
                    labs(
                      title = paste('Mod 1.4 -Env.', i, " th_fr1=", round(current_opt_th_fr1, digits = 2), ' & th_fr2=', round(current_opt_th_fr2, digits = 2)),
                      x='Timestep in a 24 day (20 min increments)',
                      y='FR and SC (gram)')+
                    xlim(0, timesteps_awake)+
                    ylim(0,5)
                  # put plot in the list
                  fr_sc_plot_list<<-append(fr_sc_plot_list, list(cur_fr_sc_plot))
                  
                  # for ease of use 
                  print(paste('Code for the stacked area graphs/sc-fr graphs is done for env=', cur_env_type))
              
            } # END FOR LOOP ENVIRONTMENTS 
            
            
          } # end function MOD 1.4 behaviour loop 
          
              # Run it 
              MOD_1_4_beh_loop_func(days = 30, N = 50, th_forage_sc = 0.2, daylight_h = 8, sim_type = 'beh_loop')
              
              
              # Plot all 3 the graph panels
            
              # set wd
              setwd(paste0(mainDir_1_4, '/5-beh_loop//')) # set current wd 
              #dev.new()
              #par(mfrow=c(6,3))
              do.call('grid.arrange', c(survival_plot_list, ncol=3))
              dev.print(pdf, (paste0('beh_loop_surv_1_4',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))
              # beh
              do.call('grid.arrange', c(stacked_chart_plot_list, ncol=3)) # aggregate the plots
              dev.print(pdf, (paste0('beh_loop_beh_traj_1_4',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))
              # fr and sc
              do.call('grid.arrange', c(fr_sc_plot_list, ncol=3)) # aggregate the plots
              dev.print(pdf, (paste0('beh_loop_sc_fr_1_4',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.pdf')))
              
              
          
    

