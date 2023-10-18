# functions for the hoarding model 
# Vera Vinken 
# March 2023 


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


#################################
#     Temperature function      #
#################################

temp_func<-function(TS, Tmax_range_low, Tmax_range_high, Tmin_range_low, Tmin_range_high, days, daylight_h, n_daylight_timestep){
  # This function is meant to create a complete temperature profile for the entire simulation
  # Will be ran inside the 'set up environment' function
  # Every timestep within the simulation will pull the 'current temperature' from the vector that this function creates
  
  
  # As we will set the minimum at the end of the day/start of the day
  # NOte that this does depend on 20 minute timesteps 
  # we can set the following before the for-loop: 
  minTemp_time_end<-72
  
  # TEMPERATURE VECTORS FOR MIN AND MAX EACH DAY 
  # These take the given max and min for the high temperature range and hte low temperature range
  max_temp_vect<-sample(seq(Tmax_range_low, Tmax_range_high, length=(days)))
  min_temp_vect<-sample(seq(Tmin_range_low, Tmin_range_high, length=(days)))
  #plot(max_temp_vect, min_temp_vect)
  #now we have a temperature set for each day 
  
  # create an empty variable where the day-profiles are going to go 
  total_temp_profile<-c()
  
  
  # START THE FOR LOOP FOR EACH DAY 
  # Now for every day, we need to create a temperature PROFILE 
  for (i in (1:days)){
    
    # establish the current max and min for each loop (day)
    cur_Tmax<-max_temp_vect[i]
    cur_Tmin<-min_temp_vect[i]
    
    # CREATE LINE FROM MAX TO MIN 
    # This is the code for 75% 
    cur_maxT_time<-round((n_daylight_timestep*0.75))
    # How far ar the max and min apart in time? 
    d_time_maxToMin<-minTemp_time_end-cur_maxT_time
    # how far are they apart in temperature? 
    d_temp_maxToMin<-cur_Tmax-cur_Tmin
    # Step per timestep: 
    temp_steps_maxToMin<-d_temp_maxToMin/d_time_maxToMin
    #Now generate a vector with a temperature for each timestep
    max_to_min_vector<-seq(cur_Tmax,cur_Tmin, by=-temp_steps_maxToMin)   # notice the '-' because we're going down in temp
    
    
    # NOW IT WILL SPLIT UP FOR DAY 1, DAY-END VS ALL THE OTHER DAYS
    # Decide what the previous and minimum is 
    
    if (i==1){
      
      # just set them to the same ones as current? 
      prev_tempMin<-min_temp_vect[i]
      
    } else if (i==(length(max_temp_vect))){
      
      # just set them to the same ones as current? 
      prev_tempMin<-min_temp_vect[i]
    } else {
      
      # previous day minimum 
      prev_tempMin<-min_temp_vect[(i-1)]
      
    } # else for 'normal' days ends (excludes day 1 and the last day )
    
    
    # LINE FROM MIN TO MAX 
    
    # how far are they apart in temperature? 
    d_temp_before<-cur_Tmax-prev_tempMin
    # Step per timestep: 
    temp_steps_before<-d_temp_before/cur_maxT_time
    #Now generate a vector with a temperature for each timestep
    before_max_vector<-seq(prev_tempMin,cur_Tmax, by=temp_steps_before)
    # Here we start at min and end at max, so we cut of both min and max (they are include din the other vector)
    before_max_vector<-before_max_vector[2:(length(before_max_vector)-1)]
    
    # ATTACH THE TWO LINES 
    # Glue the 3 vectors together 
    cur_day_temp_vector<-c(before_max_vector, max_to_min_vector)
    
    # just print a day profile (usefull for visualising this fucntion, not for the models)
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
  
  return(total_temp_profile)
} # temp function ends 

#################################
#        Forage function        #
#################################

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

