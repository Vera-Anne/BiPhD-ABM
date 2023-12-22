#####################################
# Small bird in winter - ABM 
# Start date: 25/04/2023
# Vera Vinken 
# Model 1 Sourcefile for functions 
#####################################

##################################
#  set-up environment function   #
##################################

# don't think this is used anymore (18/07)
# Removed this so it doesn't load all the time (15/09/2023)

#############################################
#  set-up environment function - PARALLEL   #
#############################################

# global environment
set_up_func_general<-function(days, env_type, daylight_h){
  # This function includes all the set up variables that are the same for every bird in the simulation 
  
  # BIRD PARAMETERS 
  stom_size<<-0.4      # stomach size of the bird 
  stom_to_fat<<-0.132  # variable that determines how many grams of sc go to fat
  fat_max<<-4          # maximum fat reserve in gram (Pravosudov & Lucas, 2001)
  
  # METABOLISM- set metabolic rates (create the functions)
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
  
  #  TEMPERATURES 
  # You know the environment type 
  # Set the temperatures accordingly 
  if (env_type==1|env_type==3|env_type==5|env_type==7|env_type==9|env_type==11){
    # Low temperatures (the 'old' medium)
    Tmax_range_low<<- -3
    Tmax_range_high<<- 3
    Tmin_range_low<<- -13
    Tmin_range_high<<- -7
  } else if (env_type==2|env_type==4|env_type==6|env_type==8|env_type==10|env_type==12){
    # high temperatures 
    Tmax_range_low<<- 7
    Tmax_range_high<<- 13
    Tmin_range_low<<- -3
    Tmin_range_high<<- 3
  } else{
    print('error with environment types')

  }
  
  # CACHES / FOOD DISTRIBUTIONS SET UP 
  food_item<<-0.064    # value of a food item 
  b_size<<-24          # Size of a food bonanza 
  
  num_cache_min<<-50  # minimum number of caches that each bird has initially 
  num_cache_max<<-100 # maximum number of caches each bird has initially 
  retrieve_min<<-5    # minimum number of caches needed to make retrieval worth it 
  
  cache_halflife<<-1440            # The halflife as taken from Pravosudov & Lucas 2001. 20 days. 20*72 = 1440 timesteps. 
  lambda<<-(log(2)/cache_halflife)   # Calculated as in radioactive decay curves. Lambda = 0.693/T-halftime (check pilferage.Rmd file for details)
  
  # FOOD DISTRIBUTIONS 
  # You know the environment types
  # Set the food distributions accordingly 
  if(env_type<3){
    # Low food and poisson distributions 
    num_food_mean<<-2
    prob_b_forage<<-0
  } else if (env_type>2 && env_type<5){
    num_food_mean<<-2
    prob_b_forage<<-0.5
  } else if (env_type>4 && env_type<7){
    num_food_mean<<-3
    prob_b_forage<<-0
  } else if (env_type>6 && env_type<9){
    num_food_mean<<-3
    prob_b_forage<<-0.5
  } else if (env_type>8 && env_type<11){
    num_food_mean<<-4
    prob_b_forage<<-0
  } else if (env_type>10){
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
  
} # end set up general function 

# Individual bird 
set_up_func_indiv<-function(days, env_type, daylight_h, mod_type){
  #print('here')
  # # link to the function file 
  # setwd("C:/Local_R/BiPhD-ABM/")
  # source('MOD_1_FuncSource.R')
  
  # The output of the temperature function is a variable called 'total_temp_profile', 
  # This is just a vector with a temperature for each timestep 
  # This can later be used for the current temp determination 
  
  # MATRICES 
  # create individual matrices (Global)
  mat_alive<<-matrix(NA, 1, TS)            # matrix to keep track of who's alive 
  mat_sc<<-matrix(NA, 1, TS)               # matrix to keep track of stomach contents
  mat_fr<<-matrix(NA, 1, TS)               # matrix to keep track of fat reserves 
  mat_mass<<-matrix(NA,1,TS)               # matrix to keep track of mass of birds 
  mat_caches<<-matrix(NA,1,TS)             # matrix to keep track of the number of caches each bird has at each timestep
  mat_Pkill<<-matrix(NA,1,TS)              # matrix to keep track of what Pkill every bird had at each timestep
  mat_find_food<<-matrix(NA, 1, TS)         # Keep track of how many food items are found 
  mat_flr<<-matrix(NA, 1, TS)               # Keeps track of the fat-change rate 
  mat_Pdecay<<-matrix(NA, 1, TS)             # Keeps track of the probability that caches decay at each timestep 
  
  
  # fill in some initial values for agent variables  (global)
  # This also needs to be specific (and stochastic) for the individual
  mass_base<<-8+(rtruncnorm(1, a=0.01, b=0.2, mean=0.1, sd=0.01))             # Gives initial mass from normal distribution (Polo et al. 2007
  sc_init<<-0 #+(rtruncnorm(1, a=0, b=stom_size, mean=(stom_size/2), sd=0.01))  # gives initial stomach content from equal distribution
  fr_init<<-0+(rtruncnorm(1, a=0, b=fat_max, mean=(fat_max/2), sd=1))         # gives initial fat reserves for random number between 0-4
  alive_init<<-rep(1, 1 )                                                     # all birds are alive at the start 

  # for non hoarders, there should not be any initial caches. For the other models this needs to be set
  if (mod_type=="11" |mod_type=="21" |mod_type=="31"){
    caches_init<<-0
  } else{
    caches_init<<-round(0+(rtruncnorm(1, a=num_cache_min, b=num_cache_max, mean=((num_cache_min+num_cache_max)/2), sd=25))) # initial cache numbers for birds rounded to closest integer
    
  }
  
  
  # Put these in first column of the matrices  
  mat_alive[,1]<<-alive_init
  mat_sc[,1]<<-sc_init
  mat_fr[,1]<<-fr_init
  mass_init<<-(mass_base+ sc_init + fr_init)   # first calculate the actual initial mass from the base weight 
  mat_mass[,1]<<-mass_init                     # Then put this in as an initial value 
  mat_caches[,1]<<-caches_init

  # Keep track of what the bird is doing 
  forage_count<<-matrix(NA, 1, TS)
  rest_count<<-matrix(NA, 1, TS)
  sleep_count<<-matrix(NA, 1,TS)
  retrieve_count<<-matrix(NA, 1, TS)
  eat_hoard_count<<-matrix(NA, 1, TS)
  eat_count<<-matrix(NA, 1, TS)
  predation_count<<-matrix(NA, 1,TS)                               # Keep track of how many birds have actually been killed by predation
  hoard_count<<-matrix(NA, 1, TS)
  decay_count<<-matrix(NA, 1, TS)
  
} # end set-up function for individuals 

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
    
    #Create a matrix alternative, which can be saved 
    mat_temp<<-t(as.matrix(total_temp_profile))
    
  } # for loop per day ends 
  
  
} # temp function ends 


#################################
#       FOOD DISTRIBUTION       #
#################################

food_distr_func<-function(num_food_mean, prob_b_forage, b_size){
  # First decide if you're going into bonanza chances or normal foraging 
  # First decide if you're going into bonanza chances or normal foraging 
  
  # The following code takes a sample of of size 1 of the elements 'forage b' and 'forage n'
  # Samplign should be with replacement
  # probw is a vector of the probability weights for obtaining the elements being sampled (forage b and forage-n)
  cur_forage_type<<-sample(c('forage-b', 'forage-p'), size=1, replace=TRUE, prob=c(prob_b_forage, (1-prob_b_forage)))
  
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

#################################
#         FAT-LOSS-RATE         # 
#################################

flr_func<-function(t, i){
  # In the case of 'normal' timesteps 
  if (t>7){
    
    # the 'current' fat-reserve is where the previous timestep ended (therefore the use of t-1)
    cur_fr<<-mat_fr[i, (t-1)]
    # the 'previous' fat-reserve is where the timestep 2 hours (6 timesteps) before that ended (hence t-7)
    prev_fr<<-mat_fr[i, (t-7)]
    # In the special case of the first timestep in the simulation 
  } else if (t==1){
    
    # First do all the work to generate the pre-simulation matrix 
          # This is where the matrix with previous FR's needs to be generated
          cur_indiv_fr_prerun_mat<<-matrix(NA, 1, 7)
          # The same for a total mass situation
          cur_indiv_mass_prerun_mat<<-matrix(NA, 1, 7)
          # Fill in the first value (t = t1 - 1)
          # This will have the initival values (they represent the FR value at the end of the last timestep)
          cur_indiv_fr_prerun_mat[1,1]<<-fr_init
          # Give the initial value for mass as well - note that sc is not in here, as we assume an empty stomach
          cur_indiv_mass_prerun_mat[1,1]<<-(mass_base + fr_init)
          # Now, for the 6 timesteps left, we need to calculate how much fat reserve was left in the timestep before
          # We know at each piont in time, the bird was resting (the simulation starts first thing in the morning)
          # For the temperature, we will assume the initial temperature of t=1
          # We can use the functions for bmr and mr to calculate what must have gone on
          # First find the temperature to use
          # The temp function did already run, because it is included in setup_general
          temp_for_prerun_fr<<-total_temp_profile[1]
          # Put that temperature in the mr function
          cur_mr<<-mr_function(temp_cur = temp_for_prerun_fr)
          #print('debug')

          # The rest will need to happen for each of the leftover timesteps
          for (o in 1:6){
            # Calcualte the current bmr
            # note that we can only do this with the mass that the bird has in the timestep after it
            # so it is not completely correct
            cur_bmr<<-bmr_function(mr_cur=cur_mr, mass_cur = cur_indiv_mass_prerun_mat[o])
            # Now we need to subtract this amount off the fat-reserves of the timestep at hand
            cur_indiv_fr_prerun_mat[(o+1)]<<-(cur_indiv_fr_prerun_mat[o]+cur_bmr)
            # put the correct value in the mass matrix as well
            cur_indiv_mass_prerun_mat[(o+1)]<<-mass_base+cur_indiv_fr_prerun_mat[(o+1)]
          }

    # Now extract what actually needs to be inputted for the first timestepl 
        # The 'current fr' is the fat reserve that the bird (likely) had 1 hypothetical timestep before the simulation started 
        # Note that the order of that matrix goes from closest in time (1) to furthest away in time (7)
        cur_fr<<-cur_indiv_fr_prerun_mat[1]
        # The 'previous' FR will be 6 timesteps before that 
        prev_fr<<-cur_indiv_fr_prerun_mat[7]
    # In the other cases where cur_FR needs to come from the normal matrix, but prev_FR needs to come from the prerun matrix 
  } else {
    cur_fr<<-mat_fr[i, (t-1)]
    prev_fr<<-cur_indiv_fr_prerun_mat[8-t]
    
  }
  # Now these results are used to calcualte the FLR 
  cur_flr<<-cur_fr-prev_fr
  mat_flr[i,t]<<-cur_flr
}


#################################
#   dead-or-alive function      #
#################################

dead_or_alive_func<-function(t, i){
  #print(paste('this is i=', i ,'and t=', t))
  
  # Check if individual is alive? 
  # in step 1 all birds are alive 
  
  
  if (t==1){
    mat_alive[i,t]<<- 1
    #print(paste(mat_alive[i,t]))
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
    #print('this should be printed when t=2')
    #print(paste(' the mat alive in dead/alive at i=', i, 'and t=', t, 'is', (mat_alive[i,t])))
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
    # For the FLR matrix 
    mat_flr[i,t]<<-NA
    # for Pdecay 
    mat_Pdecay[i,t]<<-NA
  } else {
    
    #################
    #  ALIVE BIRDS  #
    #################
    #print(paste('bird', i, 'is alive t=', t))
    # 
    # commented these out to move them to later in th emetabolic function 
          # # Set the current BMR 
          # # Note: I have made the decision to calculate this at the start of the tick. 
          # # So this is before any behaviour, or food is moved around 
          # # set the current mass 
          # mass_cur<<-mat_mass[i,t]
          # # calculate the current mr 
          # mr_function(temp_cur)                                   # note that this will need to be changed if we're using different temperatures
          # # calculate the current 
          # bmr_function(mr_cur, mass_cur)
  } # end of alive birds 
  
  # print(paste('dead or alive for bird', i, ' and t=', t, 'done'))
  
} # end of dead_or_alive function 


####################
#     SLEEPING     #
####################

sleep_func<-function(t, i){
  
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
    # set the direct hoarding matrix to 0
    hoard_count[i,t]<<-0
    # find food matrix
    mat_find_food[i,t]<<-NA
    
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
    
    ################
    #    AWAKE     # 
    ################
    
    # set the sleeping matrix to 0 
    sleep_count[i,t]<<-0
  }
  
} # end of the sleeping function 

############################
#        FORAGING          # 
###########################

forage_function<-function(t, i){
  
  # set the forage to 1
  forage_count[i, t]<<-1
  
  # Run the function with the food distribution 
  # The outcome here is 'food_item_found'
  food_distr_func(num_food_mean, prob_b_forage, b_size)
  
  # Pop this in the matrix (this is in number of items found)
  mat_find_food[i,t]<<-food_item_found
  # convert to grams
  food_item_found_gram<<-(food_item_found*food_item)
  
} # end foraging function 


#######################
#    EAT FUNCTION     # 
#######################

eat_func<-function(t ,i){
  
  # At this point, the bird has a 'known' number of food items found 
  
  # First check if any food was found 
  # Set the eating matrix 
  if (food_item_found_gram==0){
    # if the bird went foraging but did not actually find anything
    eat_count[i,t]<<-0
  } else {
    # Add this to the stomach content 
    mat_sc[i,(t)]<<-(mat_sc[i,t])+(food_item_found_gram)
    
    # Now check if this doesn't exceed the stomach size. If so, set the stomach content to stomach size
    if (mat_sc[i,(t)]>stom_size){
      mat_sc[i,(t)]<<-stom_size
    }
    # set the eating matrix 
    eat_count[i,t]<<-1
  }
  
  
  # Set the BMR to the right level: cost of foraging
  BMR_multi<<-8
  
  #set the predation risk
  Patt_cur<<-Patt_for

  # update the other matrices 
  eat_hoard_count[i,t]<<-0
  retrieve_count[i,t]<<-0
  rest_count[i,t]<<-0
  hoard_count[i,t]<<-0
  
} # end of eating function 

#####################
#      RESTING      # 
#####################

rest_func<-function(t,i){
  
  # set bmr to right level: resting 
  BMR_multi<<-1.95                   
  
  # set the predation risk 
  Patt_cur<<-Patt_rest
  
  # set the rest matrix to 1
  rest_count[i,t]<<-1
  # set the unused behaviour matrices to 0
  forage_count[i,t]<<-0
  retrieve_count[i,t]<<-0
  eat_hoard_count[i,t]<<-0
  eat_count[i,t]<<-0
  hoard_count[i,t]<<-0
  # find food matrix
  mat_find_food[i,t]<<-NA
  
} # end of the resting function 

####################
##   RETRIEVING   ## 
####################

retrieve_func<-function(t,i){
  # retrieving happens when the stomach content is below the lowest threshold (sc-th1)
  # The bird also needs to have the minimum number of caches to allow retrieval 
  
  # determine how many caches are retrieved
  cur_stomach_space<<-(stom_size-mat_sc[i,t])                     # What is the space left in the stomach?
  cur_caches_retrieved<<-((round(cur_stomach_space/food_item)))   # how many caches to fill this back up
  mat_caches[i,t]<<-(mat_caches[i, (t)]-cur_caches_retrieved)     # update the number of cahches that are left
  if(mat_caches[i,t]<0){                                          # Make sure that the number of caches cannot go below 0
    mat_caches[i,t]<<-0
  }
  
  # update the stomach content
  food_g_retrieved<<-cur_caches_retrieved*food_item               # retrieved food in grams
  mat_sc[i,t]<<-((mat_sc[i,t])+food_g_retrieved)                  # Add the food to the stomach content
  
  # set new BMR multi for retrieval behaviour
  BMR_multi<<-8
  
  # Set the predation risk 
  Patt_cur<<-Patt_for
  
  # Update the retrieving matrix 
  retrieve_count[i,t]<<-1
  # Update the other matrices 
  forage_count[i,t]<<-0
  rest_count[i,t]<<-0
  eat_count[i,t]<<-0
  eat_hoard_count[i,t]<<-0
  hoard_count[i,t]<<-0
  # find food matrix
  mat_find_food[i,t]<<-NA
  
  
  
} # End of retrieving function 

#############################
#  LEFTOVER HOARD FUNCTION  #
#############################

eat_hoard_func<-function(t,i){
  
  # Check if the bird actually found a food item 
  
  # NO FOOD ITEM FOUND 
  if (food_item_found_gram==0){
    eat_hoard_count[i,t]<<-0
    rest_count[i,t]<<-0
    eat_count[i,t]<<-0
    retrieve_count[i,t]<<-0
    hoard_count[i,t]<<-0
    # update BMR multi
    BMR_multi<<-8
    # Set the predation risk 
    Patt_cur<<-Patt_for

  } else{
  
  # Fill the stomach with whatever food items are found 
  mat_sc[i,(t)]<<-(mat_sc[i,t])+(food_item_found_gram)
  
  # now check if this exceeds the stomach size
  if (mat_sc[i,(t)]>stom_size){
    # This means the bird found more than it can eat
    # It will hoard the surplus
    
    ######################
    #    EAT-HOARD       #
    ######################
    
    # update agent-owned variables
    hoard_surplus<<-floor((mat_sc[i,(t)]-stom_size)/food_item)  # Determine The surplus available in whole food items
    mat_sc[i,(t)]<<-stom_size                                   # The stomach is set to the stomach size (full)
    mat_caches[i,t]<<-(mat_caches[i,t])+hoard_surplus
    
    # update BMR multi
    BMR_multi<<-8
    
    # Set the predation risk 
    Patt_cur<<-Patt_for
    
    # update the eat-hoard matrix 
    eat_hoard_count[i,t]<<-1
    
    # update the global counters
    eat_count[i,t]<<-0
    rest_count[i,t]<<-0
    retrieve_count[i,t]<<-0
    hoard_count[i,t]<<-0
  } else {
    
    ################
    #  EAT NORMAL  # 
    ################
    
    # The food is already in the stomach 
    # update BMR multi
    BMR_multi<<-8
    
    # Set the predation risk 
    Patt_cur<<-Patt_for
    
    # update the eat  matrix 
    eat_count[i,t]<<-1 
    
    # update the global counters
    eat_hoard_count[i,t]<<-0
    rest_count[i,t]<<-0
    retrieve_count[i,t]<<-0
    hoard_count[i,t]<<-0
    
  } # END THE NORMAL EATING PART 
  } # END THE FOOD ITEM FOUND PART 
  
  
} # end of the eat-hoard function 

#############################
#   DIRECT HOARD FUNCTION   #
#############################

dir_hoard_func<-function(t,i){
  
  # NO FOOD ITEM FOUND 
  if (food_item_found_gram==0){
    # update the eat-hoard matrix 
    hoard_count[i,t]<<-0
  } else{
    # Just add the number of food items that were found to the matrix with caches in 
    mat_caches[i,t]<<-((mat_caches[i,t])+food_item_found)
    # update the eat-hoard matrix 
    hoard_count[i,t]<<-1
    
  } # end items were found 
    
  
  # update BMR multi
  BMR_multi<<-8
  
  # Set the predation risk 
  Patt_cur<<-Patt_for
  
  # update the global counters
  eat_count[i,t]<<-0
  rest_count[i,t]<<-0
  eat_count[i,t]<<-0
  retrieve_count[i,t]<<-0
  eat_hoard_count[i,t]<<-0
  
} # end of the direct hoarding function 


##########################
#   PREDATION FUNCTION   # 
##########################

predation_func<-function(t,i){
  
  # Some calulations to prepare 
  mass_cur<<-mat_mass[i,t]                                            # find out the current mass of the bird 
  Pcap_cur<<-(0.78+(0.5*(10^-8)*exp(1.4*mass_cur)))                   # calculate the current Pcapture 
  Pkill_cur<<-Pcap_cur*Patt_cur                                       # calculate the current Pkill 
  mat_Pkill[i,t]<<-Pkill_cur                                          # put in the matrix 
  
  # now check if the bird dies or not 
  Psurv_cur<<-runif(1)                                                  # Random number between 0 and 1 for survival chance 
  if(Psurv_cur<(mat_Pkill[i,t])){                                       # if the prob for survival < prob to die 
    mat_alive[i,t]<<-0                                                  # Set the matrix to 'dead' 
    predation_count[i,t]<<-1
  } else {
    # Surviving birds should update their values: 
    predation_count[i,t]<<-0
  }
} # end of the predation function 

#########################
#   ENERGY METABOLISM   # 
#########################

# Metabolic rate, based on temperature 
mr_function<<-function(temp_cur){                 # see pravosudov & lucas (2001) and Lucas & Walter (1991) for details 
  mr_cur<<-45.65-(1.33*temp_cur)
}

# Basal metabolic rate based on MR and the current mass of the bird 
bmr_function<<-function(mr_cur, mass_cur){        # see pravosudov & lucas (2001) and Lucas & Walter (1991) for details
  bmr_cur<<-0.00616*mr_cur*((mass_cur/1000)^0.66)   # please note that the 0.00616 is for 20 min intervals 
}

# Updating
en_metab_func<-function(t,i){
  

  
  # UPDATE THE FAT RESERVES AND STOMACH CONTENT
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
  
  # Calculate the mr and bmr 
  # Set the current BMR 
  # Note: these are calcualted with the mat_mass[i,t] which as not changed since the start of the tick 
  # So this is before any behaviour, or food is moved around 
  # set the current mass 
  mass_cur<<-mat_mass[i,t]
  # calculate the current mr 
  mr_function(temp_cur)                                   # note that this will need to be changed if we're using different temperatures
  # calculate the current 
  bmr_function(mr_cur, mass_cur)
  
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
  mat_mass[i,t]<<-((mass_base[i])+(mat_fr[i,t])+(mat_sc[i,t]))
  
} # end of the metabolism function 


######################
#     PILFERAGE      #
######################
pilf_func<-function(i,t, lambda){
  # Calculcate the current lambda for all caches that this bird has 
  cur_lambda<<-(lambda*mat_caches[i,t])
  # calculate the probability of no decay at all
  P_noDecay<<-exp(-cur_lambda)
  # Now calculate the chance that 1 or more caches decayed 
  P_oneOrMore<<-(1-P_noDecay)
  # add that to the matrix 
  mat_Pdecay[i,t]<<-P_oneOrMore
  # Set a random nuber
  P_random<<-runif(1)
  
  if(P_oneOrMore>P_random){
    # pilfer 1 item 
    mat_caches[i,t]<<-(mat_caches[i,t]-1)
    if(mat_caches[i,t]<0){
      mat_caches[i,t]<<-0
    }
    # set the counting matrix 
    decay_count[i,t]<<-1
  }else{
    decay_count[i,t]<<-0
  }
}


##################################
#   PREPARE FOR NEXT TIMESTEP   # 
##################################

ts_prep_func<-function(t,i, TS){
  
  # MOVE ALL VARAIBLES TO T+1 
  # Note that this should only happen if youre not in the last timestep 
  if(t<TS){
    #print('about to do the t+1 stuff ')
    # For the fr matrix 
    mat_fr[,(t+1)]<<-mat_fr[,t]
    # For the mass matrix 
    mat_mass[,(t+1)]<<-mat_mass[,t]
    # For the sc matrix 
    mat_sc[,(t+1)]<<-mat_sc[,t]
    # for the caches matrix 
    mat_caches[,(t+1)]<<-mat_caches[,t]
  }
  
} # end of preparing for next timestep function 

#######################################
#  SETTING UP DIRECTORIES FOR SAVING  # 
#######################################

direct_func<-function(modelType){
  # Set up the main directory for where you want the figures saved 
  # This can be replaced by any folder you have on your computer (just make sure you have continuous connection if its a webfolder)
  mainDir<<-'C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/'
  setwd(mainDir)
  
  # Run the following if doing this for the first time on devide: 
  # create list of folders that we want present 
  folders<-c('MOD_1_1', 'MOD_1_2', 'MOD_1_3', 'MOD_2_1', 'MOD_2_2', 'MOD_2_3')
  # Check if they exist and if not, create them 
  # Note that this code will warn you if it already existed 
  for (folder in folders){
    dir.create(file.path(mainDir, folder ), showWarnings = TRUE)
  }
  
  # Now set to the correct folder 
  setwd(paste0(mainDir, modelType))
  
}

#######################################
#    GO TO DIRECTORY FOR ANALYSIS     # 
#######################################

retrieve_output_func<-function(modelType, fileName){
  
  # set main directory for retrieving data 
  mainDir<<-'C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/'
  # Now set to the correct folder 
  setwd(paste0(mainDir, modelType))
  
}


######################################
#  SPLIT DATAFRAMES AND NAME THEM    # 
######################################

create_df_func<-function(outputFile, modelType, env_type){
  require(purrr)
  N<-nrow(outputFile)
  
  for (k in 1:17){
    if (k==1){
      # create a clean list in the first round 
      list_outcome_vars<-list()
    }
    # Create a dataframe from the first column of the total matrix 
    cur_df<-as.data.frame(do.call(rbind, outputFile[1:N, k]))
    # add this to the empty list created 
    list_outcome_vars<-append(list_outcome_vars, list(cur_df))
  }
  # Give the dataframes in the 'list_outcome_vars' the correct names 
  variable_names<<-c('eat', 'eat_hoard', 'forage', 'dir_hoard', 'alive', 'caches', 'find_food', 'fat_res', 'stom_con', 'fat_loss_r', 'mass',  'predation', 'rest', 'retrieve', 'sleep', 'cache_decay','temp')
  names(list_outcome_vars)<-variable_names
  
  # create a list of teh raw dataframes 
  y<-assign(paste0('output_df_list_raw',modelType),list_outcome_vars, envir=.GlobalEnv)
  
  mean_dfs<<-lapply(y, colMeans, na.rm=TRUE)
  #mean_dfs<<-lapply(mean_dfs, t)
  mean_dfs<<-lapply(mean_dfs, as.data.frame)
  mean_dfs<<-lapply(mean_dfs, function(x){
    cbind(x, "timestep"=1:nrow(x))
  })
  
  names<-c('value', 'timestep')
  mean_dfs<<-lapply(mean_dfs, setNames, nm=names)
  
  # Same names but for this dataframe 
  names(mean_dfs)<<-variable_names
  
  # export the mean_dfs as a specific name 
  assign(paste0('output_means_list',modelType, 'env', env_type, sep=''), mean_dfs, envir=.GlobalEnv)
  
  # Now put them all together in a dataframe with an id 
  total_vars_df<-map_df(mean_dfs, ~as.data.frame(.x), .id = 'id')
  # same for the total dataframe 
  assign(paste('total_vars_df',modelType,sep=''), total_vars_df, envir=.GlobalEnv)
  
  
}


######################################
#      12 PLOT VISUALISATION         #
######################################

plots_12_func<-function(inputdata, modelType){
  
  inputdata<-data.table(inputdata) 
  
  
  inputdata[id == "alive", y_min :=0]
  inputdata[id == "alive", y_max :=1]
  
  inputdata[id == "cache_decay", y_min:=0]
  inputdata[id == "cache_decay", y_max:=1]
  
  inputdata[id == "caches",y_min := 0]
  inputdata[id == "caches",y_max := 300]
  
  inputdata[id == "dir_hoard",y_min := 0]
  inputdata[id == "dir_hoard",y_max := 1]
  
  inputdata[id == "eat",y_min := 0]
  inputdata[id == "eat",y_max := 1]
  
  inputdata[id == "eat_hoard",y_min := 0]
  inputdata[id == "eat_hoard",y_max := 1]
  
  inputdata[id == "fat_res",y_min := 0]
  inputdata[id == "fat_res",y_max := 5]
  
  inputdata[id == "find_food",y_min := 0]
  inputdata[id == "find_food",y_max := 5]
  
  inputdata[id == "forage",y_min := 0]
  inputdata[id == "forage",y_max := 1]
  
  inputdata[id == 'fat_loss_r',y_min := 0]
  inputdata[id == 'fat_loss_r',y_max := 0.5]
  
  inputdata[id == "predation",y_min := 0]
  inputdata[id == "predation",y_max := 1]
  
  inputdata[id == "stom_con",y_min := 0]
  inputdata[id == "stom_con",y_max := 0.5]
  
  inputdata[id == 'rest', y_min := 0]
  inputdata[id == 'rest', y_max := 1]
  
  inputdata[id == 'mass', y_min := 8]
  inputdata[id == 'mass', y_max := 13]
  
  inputdata[id == 'retrieve', y_min := 0]
  inputdata[id == 'retrieve', y_max := 1]
  
  inputdata[id == 'sleep', y_min := 0]
  inputdata[id == 'sleep', y_max := 1]
  
  inputdata[id == 'fat_loss_r', y_min := -0.6]
  inputdata[id == 'fat_loss_r', y_max := 0.6]
  
  inputdata[id == 'temp', y_min :=-14]
  inputdata[id == 'temp', y_max :=14]
  
  plot<-ggplot(inputdata, aes(x=timestep, y=value)) + 
    geom_line() +
    facet_wrap(.~id, scales='free_y', nrow=5)+
    ggtitle(paste('Output model', modelType, 'N=',N, 'days=',days, 'daylight_h=',daylight_h, 'envType=', env_type))+
    geom_blank(aes(y = y_min)) +
    geom_blank(aes(y = y_max))
  
  # Then assign this some useful name 
  assign(paste0('plot_12_', modelType), plot, envir=.GlobalEnv)
  
  
  # In future I want to change the scales for easier comparison 
  # this code could help: 
  
  
  
}


###############################
#  plots environment function #
###############################

plot_env_18_surv<-function(output_env_func, modelType){
  # Now do an overview image 
  survival_graph<-output_env_func[2]
  for (i in 1:length(survival_graph[[1]])){
    if(i==1){
      plot_list<-list()
    }
    cur_df<-subset(survival_graph[[1]][[i]], survival_graph[[1]][[i]]$id=="alive")
    cur_plot<-ggplot(data=cur_df, aes(x=timestep, y=value))+
      geom_line()+
      ggtitle(label=paste('env=', i, ' MOD', modelType))+
      ylim(0,1)
    plot_list[[i]]<-cur_plot
  }
  
  # plot it 
  #do.call(what = grid.arrange, args=c(plot_list, ncol=3))
  
  # Then assign this some useful name 
  assign(paste0('plot_list_env_'), plot_list, envir=.GlobalEnv)
  #ggarrange(plot_list, ncol=3)
  do.call(grid.arrange, c(plot_list, ncol=2))
  
}

# For the Rmarkdown file that runs with minimal input vars ('split' setting )
plot_env_12_comp_func<-function(env_func_out_list, models, int_var, focus){
  
  # Loop through the models to extract the correct variable 
  for (i in 1:length(models)){
    if (i==1){
      output_per_model_list<-list()
    }
    # Set the current model from the env_func_out_list (input to this function)
    cur_out<-env_func_out_list[[models[i]]]
    # Now subset for the variable of interest 
    cur_out_filtered<-lapply(cur_out[[2]], function(df){subset(df, id==paste(int_var))})
    # Add the environment number to the dataframes 
    for (j in 1:length(cur_out_filtered)){
      cur_out_filtered[[j]]$env_id<-j
      cur_out_filtered[[j]]$mod_id<-models[i]
      cur_out_filtered[[j]]$line_id<-lim_col_df[lim_col_df$all_models==models[i], 5]
    }
    output_per_model_list[[i]]<-cur_out_filtered
    
  }
  # for the individual dataframes 
  total_all_models<-list()
  # loop through each of the models  and retrieve all 12 dataframes to put in the 'individual dataframe list' 
  for (sublist in output_per_model_list) {
    for (df in sublist) {
      total_all_models <- rbind(total_all_models, df)
    }
  }

  
  # Now you want to produce a grid of plots, where data is split per environment 
    # On your x-axis you want time "timestep" 
    # On you y-axis you want the value of the chosen variable 'value' 
  
  # select colours
  if(focus=="colours_hoard_foc"){
    colours<-lim_col_df$colours_hoard_foc
  }else if(focus=="colours_prox_foc"){
    colours<-lim_col_df$colours_prox_foc
  }else if(focus=="all_colours"){
    colours<-lim_col_df$all_colours
  }
      plot_12<-ggplot(total_all_models, aes(x=timestep, y=value))+
        geom_line(aes( col=mod_id, linetype=line_id))+
        facet_wrap(.~env_id,  nrow=6)+
        ggtitle(label=paste(int_var, "- Variable per environment type" ))+
        theme(plot.title=element_text(face='bold'), 
              axis.title.x = element_text( face='bold' ), 
              axis.title.y=element_text(face='bold'), 
              strip.text=element_text(), 
              legend.text=element_text())+
        scale_color_manual(values=colours)

      plot_12<-ggplotly(plot_12)
      return(plot_12)
}




# For the Rmarkdown file that runs with minimal input vars ('mean' setting)
plot_mean_12_env_func<-function(env_func_out_list, models, int_var, focus){
  # loop through each of the models 
  for (i in 1:length(models)){
    if (i==1){
      output_per_model_list<-list()
    }
    # Set the current model from the env_func_out_list (input to this function)
      cur_out<-env_func_out_list[[models[i]]]
    
      # create empty list
      cur_mod_df<-list()
      
      # Concatenate the dataframes in the list 
      for (df in cur_out[[2]]){
        cur_mod_df<-rbind(cur_mod_df, df)
      }
      # summarize them by variable and timestep 
      cur_mod_df_sum<-cur_mod_df%>%
        group_by(timestep, id)%>%
        summarise(mean_val = mean(value, na.rm=T))%>%
        mutate(mod_id=models[i])%>%
        mutate(line_id=lim_col_df[lim_col_df$all_models==models[i], 5])

      output_per_model_list[[i]]<-cur_mod_df_sum
  }
  
  mean_var_per_model_total<-bind_rows(output_per_model_list)
  
  if(focus=="colours_hoard_foc"){
    colours<-lim_col_df$colours_hoard_foc
  }else if(focus=="colours_prox_foc"){
    colours<-lim_col_df$colours_prox_foc
  }else if(focus=="all_colours"){
    colours<-lim_col_df$all_colours
  }
  # Now you want to produce a grid of plots, where data is split per environment 
  # On your x-axis you want time "timestep" 
  # On you y-axis you want the value of the chosen variable 'value' 
  plot_var<-ggplot(mean_var_per_model_total, aes(x=timestep, y=mean_val))+
    geom_line(aes( col=mod_id, linetype=line_id), size=1)+
    facet_wrap(.~id, scales='free_y', nrow=6)+
    ggtitle(label=paste("Average across all environments per interest variable" ))+
    theme(plot.title=element_text(size=25, face='bold'), 
          axis.title.x = element_text(size=20, face='bold' ), 
          axis.title.y=element_text(size=20, face='bold'), 
          strip.text=element_text(size=20), 
          legend.text=element_text(size=20))+
    scale_color_manual(values=colours)
 
  plot_var
  return(plot_var)    
  }
  
  


###############################
#      HALFLIFE FUNCTION      #
###############################

t_halflife_func<-function(halflife_input){
  for (i in 1:length(halflife_input)){
    if (i==1){
      # list for the t_HL
      t_HL_list<<-list()
      # list for general fit summaries
      fit_sum_list<-list()
    } 
    
    # Create the dataframe you'll be dealing with 
    df<-subset(halflife_input[[i]], halflife_input[[i]]$id=='alive')
    # clean up the dataframe
    df$timestep<-as.numeric(df$timestep)
    df<-df[,2:3]
    colnames(df)<-c('y', 't')
    
    # Now fit the model 
    
    # To control the interations in NLS I use the following 
    nls.control(maxiter = 100)
    # I use a basic exponential decay curve 
    # starting values need to be given 
    fit<-nls(y ~ a*exp(-b*t), data=df, 
             start=list(a=1, b=0.0000001))
    # pull out hte summary --> this has the estimated values for a an db in it 
    sum_fit<-summary(fit)
    # put in the list 
    fit_sum_list[[i]]<-sum_fit$parameters
    
    # Now, where does it cross the x-axis? 
    # Set the current a & b 
    cur_a<-fit_sum_list[[i]][1]
    cur_b<-fit_sum_list[[i]][2]
    # set the halflife 
    y_halflife<-0.5
    # now calculate the timestep at which this will occur 
    t_halflife<-(-(log(y_halflife/cur_a)/cur_b))
    # calculate y from there (just to check)
    #ytest<-(cur_a*exp(-cur_b*t_halflife))
    # put in the list 
    t_HL_list[i]<<-t_halflife
  }
  return(t_HL_list)
}




#############################
#    BRANCHING FUNCTIONS    # 
#############################

# Basic branching function 
branch_func<-function(range_min, range_max, branch_num){
  # determine stepsize
  step_size<<-((range_max-range_min)/branch_num)
  
  # Set the value for the threshold for each branch
  th_b1<-range_min + (step_size/2)
  th_b2<-(th_b1+step_size)
  th_b3<-(th_b2+step_size)
  th_b4<-(th_b3+step_size)
  
  # concatenate output 
  output_branches<<-c(th_b1, th_b2, th_b3, th_b4, step_size)
  return(output_branches)
}

# Function that runs models x.1 through the branch and outputs the best threshold (each th run 25x throughout the 12 environments )
branch_4_optimization_x1<-function(model_type, range_min_start, range_max_start){
  
  ##### LEVEL 1 
  # Run the branch function 
  vals_l1<-branch_func(range_min = range_min_start, range_max = range_max_start, branch_num = 4)
  
  # Now run the model 
  for (i in 1:4){
    if (i==1){
      # Create empty list for results 
      level_1_results<<-list()
    }
    # Set current th 
    cur_th<-vals_l1[i]
    # Run the model 25 times in the 12 environments and take the average
    for (j in 1:25){
      if (j==1){
        out_per_run_list<-list()
      }
      # Run the model 
      if(model_type==11){
        env_func_1_1_par_hpc(days = days, N= N, th_forage_sc = cur_th, daylight_h = daylight_h, modelType = 11)
      }else if(model_type==21){
        env_func_2_1_par_hpc(days = days, N= N, th_forage_fr = cur_th, daylight_h = daylight_h, modelType = 21)
      }else if(model_type==31){
        env_func_3_1_par_hpc(days = days, N= N, th_forage_flr = cur_th, daylight_h = daylight_h, modelType = 31)
      }else{
        print('problem with model_type settings')
      }
      out_per_run_list[j]<-cbind(output_env_func[[1]][1], cur_th)
      print(paste(j))
    } # end of loop for 25 run s
    
    # bind together
    out_per_run_df<-data.frame(matrix(unlist(out_per_run_list), nrow=25, byrow=TRUE),stringsAsFactors=FALSE)
    colnames(out_per_run_df)<-"HL"
    # Now take the average for the threshold 
    mean_perf<-c(mean(out_per_run_df$HL), cur_th)
    # add to list
    level_1_results[[i]]<<-mean_perf
    # mark that things are working
    print(paste("Done with level 1 ", i))
  } # end of 1-4 branching loop
  
  halflife_out_l1<-data.frame(matrix(unlist(level_1_results), nrow=4, byrow=TRUE),stringsAsFactors=FALSE)
  names<-c("HL",  "th")
  colnames(halflife_out_l1)<-names
  
  # select highest outcome
  max_HL_l1<-max(halflife_out_l1$HL)
  best_l1<-halflife_out_l1[halflife_out_l1$HL==max_HL_l1, ]
  best_th_l1<-best_l1[1,2]
  
  #### LEVEL 2 
  # Calculate the new ranges for level 2 (this uses the stepsize that comes out of the branching function in level 1 )
  range_min_l2<-(best_th_l1-(step_size/2))
  range_max_l2<-(best_th_l1+(step_size/2))
  
  # Run the branch function 
  vals_l2<-branch_func(range_min = range_min_l2, range_max = range_max_l2, branch_num = 4)
  
  # Now run the model 
  for (i in 1:4){
    if (i==1){
      # Create empty list for results 
      level_2_results<<-list()
    }
    # Set current th 
    cur_th<-vals_l2[i]
    # Run the model 25 times in the 12 environments and take the average
    for (j in 1:25){
      if (j==1){
        out_per_run_list<-list()
      }
      # Run the model 
      if(model_type==11){
        env_func_1_1_par_hpc(days = days, N= N, th_forage_sc = cur_th, daylight_h = daylight_h, modelType = 11)
      }else if(model_type==21){
        env_func_2_1_par_hpc(days = days, N= N, th_forage_fr = cur_th, daylight_h = daylight_h, modelType = 21)
      }else if(model_type==31){
        env_func_3_1_par_hpc(days = days, N= N, th_forage_flr = cur_th, daylight_h = daylight_h, modelType = 31)
      }else{
        print('problem with model_type settings')
      }
      out_per_run_list[j]<-cbind(output_env_func[[1]][1], cur_th)
      print(paste(j))
    } # end of loop for 25 run s
    
    # bind together
    out_per_run_df<-data.frame(matrix(unlist(out_per_run_list), nrow=25, byrow=TRUE),stringsAsFactors=FALSE)
    colnames(out_per_run_df)<-"HL"
    # Now take the average for the threshold 
    mean_perf<-c(mean(out_per_run_df$HL), cur_th)
    # add to list
    level_2_results[[i]]<<-mean_perf
    
    # mark that things are working
    print(paste("Done with level 2 ", i))
  } # end of 1-4 branching loop
  
  halflife_out_l2<-data.frame(matrix(unlist(level_2_results), nrow=4, byrow=TRUE),stringsAsFactors=FALSE)
  names<-c("HL",  "th")
  colnames(halflife_out_l2)<-names
  
  # select highest outcome
  max_HL_l2<-max(halflife_out_l2$HL)
  best_l2<-halflife_out_l2[halflife_out_l2$HL==max_HL_l2, ]
  best_th_l2<-best_l2[1,2]
  
  #### LEVEL 3 
  # Calculate the new ranges for level 2 (this uses the stepsize that comes out of the branching function in level 1 )
  range_min_l3<-(best_th_l2-(step_size/2))
  range_max_l3<-(best_th_l2+(step_size/2))
  
  # Run the branch function 
  vals_l3<-branch_func(range_min = range_min_l3, range_max = range_max_l3, branch_num = 4)
  
  # Now run the model 
  for (i in 1:4){
    if (i==1){
      # Create empty list for results 
      level_3_results<<-list()
    }
    # Set current th 
    cur_th<-vals_l3[i]
    # Run the model 25 times in the 12 environments and take the average
    for (j in 1:25){
      if (j==1){
        out_per_run_list<-list()
      }
      # Run the model 
      if(model_type==11){
        env_func_1_1_par_hpc(days = days, N= N, th_forage_sc = cur_th, daylight_h = daylight_h, modelType = 11)
      }else if(model_type==21){
        env_func_2_1_par_hpc(days = days, N= N, th_forage_fr = cur_th, daylight_h = daylight_h, modelType = 21)
      }else if(model_type==31){
        env_func_3_1_par_hpc(days = days, N= N, th_forage_flr = cur_th, daylight_h = daylight_h, modelType = 31)
      }else{
        print('problem with model_type settings')
      }
      out_per_run_list[j]<-cbind(output_env_func[[1]][1], cur_th)
      print(paste(j))
    } # end of loop for 25 run s
    
    # bind together
    out_per_run_df<-data.frame(matrix(unlist(out_per_run_list), nrow=25, byrow=TRUE),stringsAsFactors=FALSE)
    colnames(out_per_run_df)<-"HL"
    # Now take the average for the threshold 
    mean_perf<-c(mean(out_per_run_df$HL), cur_th)
    # add to list
    level_3_results[[i]]<<-mean_perf
    
    # mark that things are working
    print(paste("Done with level 3 ", i))
  } # end of 1-4 branching loop - level 3
  
  halflife_out_l3<-data.frame(matrix(unlist(level_3_results), nrow=4, byrow=TRUE),stringsAsFactors=FALSE)
  names<-c("HL", "th")
  colnames(halflife_out_l3)<-names
  
  # select highest outcome
  max_HL_l3<-max(halflife_out_l3$HL)
  best_l3<-halflife_out_l3[halflife_out_l3$HL==max_HL_l3, ]
  best_th_l3<-best_l3[1,2]
  
  # Output the best threshold 
  return_list<-list(level_1_results, level_2_results, level_3_results, best_l3)
  return(return_list)
  
} # end of function 

# Function that runs models x.1 through the branch and outputs the best threshold (each th run 25x throughout the 12 environments )
branch_4_optimization_x1_single<-function(model_type, range_min_start, range_max_start){
  
  ##### LEVEL 1 
  # Run the branch function 
  vals_l1<-branch_func(range_min = range_min_start, range_max = range_max_start, branch_num = 4)
  
  # Now run the model 
  for (i in 1:4){
    if (i==1){
      # Create empty list for results 
      level_1_results<<-list()
    }
    # Set current th 
    cur_th<-vals_l1[i]
    # Run the model 25 times in the 12 environments and take the average
    for (j in 1:1){
      if (j==1){
        out_per_run_list<-list()
      }
      # Run the model 
      if(model_type==11){
        env_func_1_1_par_hpc(days = days, N= N, th_forage_sc = cur_th, daylight_h = daylight_h, modelType = 11)
      }else if(model_type==21){
        env_func_2_1_par_hpc(days = days, N= N, th_forage_fr = cur_th, daylight_h = daylight_h, modelType = 21)
      }else if(model_type==31){
        env_func_3_1_par_hpc(days = days, N= N, th_forage_flr = cur_th, daylight_h = daylight_h, modelType = 31)
      }else{
        print('problem with model_type settings')
      }
      out_per_run_list[j]<-cbind(output_env_func[[1]][1], cur_th)
      print(paste(j))
    } # end of loop for 25 run s
    
    # bind together
    out_per_run_df<-data.frame(matrix(unlist(out_per_run_list), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
    colnames(out_per_run_df)<-"HL"
    # Now take the average for the threshold 
    mean_perf<-c(mean(out_per_run_df$HL), cur_th)
    # add to list
    level_1_results[[i]]<<-mean_perf
    # mark that things are working
    print(paste("Done with level 1 ", i))
  } # end of 1-4 branching loop
  
  halflife_out_l1<-data.frame(matrix(unlist(level_1_results), nrow=4, byrow=TRUE),stringsAsFactors=FALSE)
  names<-c("HL",  "th")
  colnames(halflife_out_l1)<-names
  
  # select highest outcome
  max_HL_l1<-max(halflife_out_l1$HL)
  best_l1<-halflife_out_l1[halflife_out_l1$HL==max_HL_l1, ]
  best_th_l1<-best_l1[1,2]
  
  #### LEVEL 2 
  # Calculate the new ranges for level 2 (this uses the stepsize that comes out of the branching function in level 1 )
  range_min_l2<-(best_th_l1-(step_size/2))
  range_max_l2<-(best_th_l1+(step_size/2))
  
  # Run the branch function 
  vals_l2<-branch_func(range_min = range_min_l2, range_max = range_max_l2, branch_num = 4)
  
  # Now run the model 
  for (i in 1:4){
    if (i==1){
      # Create empty list for results 
      level_2_results<<-list()
    }
    # Set current th 
    cur_th<-vals_l2[i]
    # Run the model 25 times in the 12 environments and take the average
    for (j in 1:1){
      if (j==1){
        out_per_run_list<-list()
      }
      # Run the model 
      if(model_type==11){
        env_func_1_1_par_hpc(days = days, N= N, th_forage_sc = cur_th, daylight_h = daylight_h, modelType = 11)
      }else if(model_type==21){
        env_func_2_1_par_hpc(days = days, N= N, th_forage_fr = cur_th, daylight_h = daylight_h, modelType = 21)
      }else if(model_type==31){
        env_func_3_1_par_hpc(days = days, N= N, th_forage_flr = cur_th, daylight_h = daylight_h, modelType = 31)
      }else{
        print('problem with model_type settings')
      }
      out_per_run_list[j]<-cbind(output_env_func[[1]][1], cur_th)
      print(paste(j))
    } # end of loop for 25 run s
    
    # bind together
    out_per_run_df<-data.frame(matrix(unlist(out_per_run_list), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
    colnames(out_per_run_df)<-"HL"
    # Now take the average for the threshold 
    mean_perf<-c(mean(out_per_run_df$HL), cur_th)
    # add to list
    level_2_results[[i]]<<-mean_perf
    
    # mark that things are working
    print(paste("Done with level 2 ", i))
  } # end of 1-4 branching loop
  
  halflife_out_l2<-data.frame(matrix(unlist(level_2_results), nrow=4, byrow=TRUE),stringsAsFactors=FALSE)
  names<-c("HL",  "th")
  colnames(halflife_out_l2)<-names
  
  # select highest outcome
  max_HL_l2<-max(halflife_out_l2$HL)
  best_l2<-halflife_out_l2[halflife_out_l2$HL==max_HL_l2, ]
  best_th_l2<-best_l2[1,2]
  
  #### LEVEL 3 
  # Calculate the new ranges for level 2 (this uses the stepsize that comes out of the branching function in level 1 )
  range_min_l3<-(best_th_l2-(step_size/2))
  range_max_l3<-(best_th_l2+(step_size/2))
  
  # Run the branch function 
  vals_l3<-branch_func(range_min = range_min_l3, range_max = range_max_l3, branch_num = 4)
  
  # Now run the model 
  for (i in 1:4){
    if (i==1){
      # Create empty list for results 
      level_3_results<<-list()
    }
    # Set current th 
    cur_th<-vals_l3[i]
    # Run the model 25 times in the 12 environments and take the average
    for (j in 1:1){
      if (j==1){
        out_per_run_list<-list()
      }
      # Run the model 
      if(model_type==11){
        env_func_1_1_par_hpc(days = days, N= N, th_forage_sc = cur_th, daylight_h = daylight_h, modelType = 11)
      }else if(model_type==21){
        env_func_2_1_par_hpc(days = days, N= N, th_forage_fr = cur_th, daylight_h = daylight_h, modelType = 21)
      }else if(model_type==31){
        env_func_3_1_par_hpc(days = days, N= N, th_forage_flr = cur_th, daylight_h = daylight_h, modelType = 31)
      }else{
        print('problem with model_type settings')
      }
      out_per_run_list[j]<-cbind(output_env_func[[1]][1], cur_th)
      print(paste(j))
    } # end of loop for 25 run s
    
    # bind together
    out_per_run_df<-data.frame(matrix(unlist(out_per_run_list), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
    colnames(out_per_run_df)<-"HL"
    # Now take the average for the threshold 
    mean_perf<-c(mean(out_per_run_df$HL), cur_th)
    # add to list
    level_3_results[[i]]<<-mean_perf
    
    # mark that things are working
    print(paste("Done with level 3 ", i))
  } # end of 1-4 branching loop - level 3
  
  halflife_out_l3<-data.frame(matrix(unlist(level_3_results), nrow=4, byrow=TRUE),stringsAsFactors=FALSE)
  names<-c("HL", "th")
  colnames(halflife_out_l3)<-names
  
  # select highest outcome
  max_HL_l3<-max(halflife_out_l3$HL)
  best_l3<-halflife_out_l3[halflife_out_l3$HL==max_HL_l3, ]
  best_th_l3<-best_l3[1,2]
  
  # Output the best threshold 
  return_list<-list(level_1_results, level_2_results, level_3_results, best_l3)
  return(return_list)
  
} # end of function 

# The HPC function 
branch_4_optimization_x2_hpc<-function(model_type, range_min_start_th1, range_max_start_th1, range_min_start_th2, range_max_start_th2){
  
  ##### LEVEL 1 
  # Run the branch function 
  vals_l1_th1<-branch_func(range_min = range_min_start_th1, range_max = range_max_start_th1, branch_num = 4)
  # save the stepsize used for this
  step_size_l1_th1<-step_size
  # run for second threshold 
  vals_l1_th2<-branch_func(range_min = range_min_start_th2, range_max = range_max_start_th2, branch_num = 4)
  # save the stepsize for this 
  step_size_l1_th2<-step_size
  
  # Generate the possible combinations 
  # Take out the stepsize 
  vals_l1_th1<-vals_l1_th1[1:4]
  vals_l1_th2<-vals_l1_th2[1:4]
  
  # Generate a grid with the possible combinations 
  l1_grid<-expand.grid(vals_l1_th1, vals_l1_th2)
  names<-c("th1", "th2")
  colnames(l1_grid)<-names
  
  # For models x.1 of the first subset, th2 needs to be higher than th1 
  l1_grid<-l1_grid[l1_grid$th1<l1_grid$th2, ]
  
  # Now run the model 
  for (i in 1:nrow(l1_grid)){
    if (i==1){
      # Create empty list for results 
      level_1_results<-list()
    }
    # Set current thresholds 
    cur_th1<-l1_grid[i,1]
    cur_th2<-l1_grid[i,2]
    
    # Run the model 25 times in the 12 environments and take the average
    for (j in 1:25){
      if (j==1){
        out_per_run_list<-list()
      }
      # Run the model 
      if(model_type==12){
        env_func_1_2_par_hpc(days = days, N=N , th_forage_sc1 = cur_th1 , th_forage_sc2 = cur_th2 , daylight_h = daylight_h, modelType = 12)
      }else if(model_type==22){
        env_func_2_2_par_hpc(days = days, N= N, th_forage_fr1 = cur_th1 , th_forage_fr2 = cur_th2 , daylight_h = daylight_h, modelType = 22)
      }else if(model_type==32){
        env_func_3_2_par_hpc(days = days, N= N, th_forage_flr1 = cur_th1 , th_forage_flr2 = cur_th2 , daylight_h = daylight_h, modelType = 32)
      }else{
        print('problem with model_type settings')
      }
      out_per_run_list[j]<-cbind(output_env_func[[1]][1], cur_th1, cur_th2)
      print(paste(j))
    } # end of loop for 25 run s
    
    # bind together
    out_per_run_df<-data.frame(matrix(unlist(out_per_run_list), nrow=25, byrow=TRUE),stringsAsFactors=FALSE)
    colnames(out_per_run_df)<-"HL"
    # Now take the average for the threshold 
    mean_perf<-c(mean(out_per_run_df$HL), cur_th1, cur_th2)
    # add to list
    level_1_results[[i]]<-mean_perf
    # mark that things are working
    print(paste("Done with level 1 ", i))
  } # end of 1-4 branching loop
  
  halflife_out_l1<-data.frame(matrix(unlist(level_1_results), nrow=nrow(l1_grid), byrow=TRUE),stringsAsFactors=FALSE)
  names<-c("HL",  "th1", "th2")
  colnames(halflife_out_l1)<-names
  
  # select highest outcome
  max_HL_l1<-max(halflife_out_l1$HL)
  best_l1<-halflife_out_l1[halflife_out_l1$HL==max_HL_l1, ]
  #best_ths_l1<-best_l1[1,2]
  
  #### LEVEL 2 
  # Calculate the new ranges for level 2 
  # For threshold 1 
  range_min_l2_th1<-(best_l1$th1-(step_size_l1_th1/2))
  range_max_l2_th1<-(best_l1$th1+(step_size_l1_th1/2))
  # Same for threshold 2
  range_min_l2_th2<-(best_l1$th2-(step_size_l1_th2/2))
  range_max_l2_th2<-(best_l1$th2+(step_size_l1_th2/2))
  
  # Run the branch function 
  # for th 1
  vals_l2_th1<-branch_func(range_min = range_min_l2_th1, range_max = range_max_l2_th1, branch_num = 4)
  # stepseize
  step_size_l2_th1<-step_size
  # run for th 2
  vals_l2_th2<-branch_func(range_min = range_min_l2_th2, range_max = range_max_l2_th2, branch_num = 4)
  # save the stepsize for later
  step_size_l2_th2<-step_size
  
  # Generate the possible combinations 
  # Take out the stepsize 
  vals_l2_th1<-vals_l2_th1[1:4]
  vals_l2_th2<-vals_l2_th2[1:4]
  
  # Generate a grid with the possible combinations 
  l2_grid<-expand.grid(vals_l2_th1, vals_l2_th2)
  names<-c("th1", "th2")
  colnames(l2_grid)<-names
  
  # For models x.1 of the first subset, th2 needs to be higher than th1 
  l2_grid<-l2_grid[l2_grid$th1<l2_grid$th2, ]
  
  # Now run the model 
  for (i in 1:nrow(l2_grid)){
    if (i==1){
      # Create empty list for results 
      level_2_results<-list()
    }
    
    # Set current thresholds 
    cur_th1<-l2_grid[i,1]
    cur_th2<-l2_grid[i,2]
    # Run the model 25 times in the 12 environments and take the average
    for (j in 1:25){
      if (j==1){
        out_per_run_list<-list()
      }
      # Run the model 
      if(model_type==12){
        env_func_1_2_par_hpc(days = days, N= N, th_forage_sc1 = cur_th1 , th_forage_sc2 = cur_th2 , daylight_h = daylight_h, modelType = 12)
      }else if(model_type==22){
        env_func_2_2_par_hpc(days = days, N= N, th_forage_fr1 = cur_th1 , th_forage_fr2 = cur_th2 , daylight_h = daylight_h, modelType = 22)
      }else if(model_type==32){
        env_func_3_2_par_hpc(days = days, N= N, th_forage_flr1 = cur_th1 , th_forage_flr2 = cur_th2 , daylight_h = daylight_h, modelType = 32)
      }else{
        print('problem with model_type settings')
      }
      out_per_run_list[j]<-cbind(output_env_func[[1]][1], cur_th1, cur_th2)
      print(paste(j))
    } # end of loop for 25 run s
    
    # bind together
    out_per_run_df<-data.frame(matrix(unlist(out_per_run_list), nrow=25, byrow=TRUE),stringsAsFactors=FALSE)
    colnames(out_per_run_df)<-"HL"
    # Now take the average for the threshold 
    mean_perf<-c(mean(out_per_run_df$HL), cur_th1, cur_th2)
    # add to list
    level_2_results[[i]]<-mean_perf
    
    # mark that things are working
    print(paste("Done with level 2 ", i))
  } # end of 1-4 branching loop
  
  halflife_out_l2<-data.frame(matrix(unlist(level_2_results), nrow=nrow(l2_grid), byrow=TRUE),stringsAsFactors=FALSE)
  names<-c("HL",  "th1", "th2")
  colnames(halflife_out_l2)<-names
  
  # select highest outcome
  max_HL_l2<-max(halflife_out_l2$HL)
  best_l2<-halflife_out_l2[halflife_out_l2$HL==max_HL_l2, ]
  
  
  #### LEVEL 3 
  # Calculate the new ranges for level 2 
  # For threshold 1 
  range_min_l3_th1<-(best_l2$th1-(step_size_l2_th1/2))
  range_max_l3_th1<-(best_l2$th1+(step_size_l2_th1/2))
  # Same for threshold 2
  range_min_l3_th2<-(best_l2$th2-(step_size_l2_th2/2))
  range_max_l3_th2<-(best_l2$th2+(step_size_l2_th2/2))
  
  # Run the branch function 
  # for th 1
  vals_l3_th1<-branch_func(range_min = range_min_l3_th1, range_max = range_max_l3_th1, branch_num = 4)
  # stepseize
  step_size_l3_th1<-step_size
  # run for th 2
  vals_l3_th2<-branch_func(range_min = range_min_l3_th2, range_max = range_max_l3_th2, branch_num = 4)
  # save the stepsize for later
  step_size_l3_th2<-step_size
  
  # Generate the possible combinations 
  # Take out the stepsize 
  vals_l3_th1<-vals_l3_th1[1:4]
  vals_l3_th2<-vals_l3_th2[1:4]
  
  # Generate a grid with the possible combinations 
  l3_grid<-expand.grid(vals_l3_th1, vals_l3_th2)
  names<-c("th1", "th2")
  colnames(l3_grid)<-names
  
  # For models x.1 of the first subset, th2 needs to be higher than th1 
  l3_grid<-l3_grid[l3_grid$th1<l3_grid$th2, ]
  
  # Now run the model 
  for (i in 1:nrow(l3_grid)){
    if (i==1){
      # Create empty list for results 
      level_3_results<-list()
    }
    # Set current thresholds 
    cur_th1<-l3_grid[i,1]
    cur_th2<-l3_grid[i,2]
    # Run the model 25 times in the 12 environments and take the average
    for (j in 1:5){
      if (j==1){
        out_per_run_list<-list()
      }
      # Run the model 
      if(model_type==12){
        env_func_1_2_par_hpc(days = days, N= N, th_forage_sc1 = cur_th1 , th_forage_sc2 = cur_th2 , daylight_h = daylight_h, modelType = 12)
      }else if(model_type==22){
        env_func_2_2_par_hpc(days = days, N= N, th_forage_fr1 = cur_th1 , th_forage_fr2 = cur_th2 , daylight_h = daylight_h, modelType = 22)
      }else if(model_type==32){
        env_func_3_2_par_hpc(days = days, N= N, th_forage_flr1 = cur_th1 , th_forage_flr2 = cur_th2 , daylight_h = daylight_h, modelType = 32)
      }else{
        print('problem with model_type settings')
      }
      out_per_run_list[j]<-cbind(output_env_func[[1]][1], cur_th1, cur_th2)
      print(paste(j))
    } # end of loop for 25 run s
    
    # bind together
    out_per_run_df<-data.frame(matrix(unlist(out_per_run_list), nrow=25, byrow=TRUE),stringsAsFactors=FALSE)
    colnames(out_per_run_df)<-"HL"
    # Now take the average for the threshold 
    mean_perf<-c(mean(out_per_run_df$HL), cur_th1, cur_th2)
    # add to list
    level_3_results[[i]]<-mean_perf
    
    # mark that things are working
    print(paste("Done with level 3 ", i))
  } # end of 1-4 branching loop - level 3
  
  halflife_out_l3<-data.frame(matrix(unlist(level_3_results), nrow=nrow(l3_grid), byrow=TRUE),stringsAsFactors=FALSE)
  names<-c("HL", "th1", "th2")
  colnames(halflife_out_l3)<-names
  
  # select highest outcome
  max_HL_l3<-max(halflife_out_l3$HL)
  best_l3<-halflife_out_l3[halflife_out_l3$HL==max_HL_l3, ]
  
  # Output the best threshold 
  return_list<-list(level_1_results, level_2_results, level_3_results, best_l3)
  return(return_list)
  
} # end of function for hpc 

# For use on the HPC
branch_4_optimization_x3_hpc<-function(model_type, range_min_start_th1, range_max_start_th1, range_min_start_th2, range_max_start_th2, range_min_start_th3, range_max_start_th3){
  
  ##### LEVEL 1 
  # Run the branch function 
  vals_l1_th1<-branch_func(range_min = range_min_start_th1, range_max = range_max_start_th1, branch_num = 4)
  # save the stepsize used for this
  step_size_l1_th1<-step_size
  # run for second threshold 
  vals_l1_th2<-branch_func(range_min = range_min_start_th2, range_max = range_max_start_th2, branch_num = 4)
  # save the stepsize for this 
  step_size_l1_th2<-step_size
  # run for thrid threshold 
  vals_l1_th3<-branch_func(range_min = range_min_start_th3, range_max = range_max_start_th3, branch_num = 4)
  # save the stepsize for this 
  step_size_l1_th3<-step_size
  
  # Generate the possible combinations 
  # Take out the stepsize 
  vals_l1_th1<-vals_l1_th1[1:4]
  vals_l1_th2<-vals_l1_th2[1:4]
  vals_l1_th3<-vals_l1_th3[1:4]
  
  # Generate a grid with the possible combinations 
  l1_grid<-expand.grid(vals_l1_th1, vals_l1_th2, vals_l1_th3)
  names<-c("th1", "th2", "th3")
  colnames(l1_grid)<-names
  
  # For models x.2 of the first subset, th2 needs to be higher than th1 and th3 higher than th2
  l1_grid<-l1_grid[((l1_grid$th1<l1_grid$th2)&(l1_grid$th3>l1_grid$th2)), ]
  
  # Now run the model 
  for (i in 1:nrow(l1_grid)){
    if (i==1){
      # Create empty list for results 
      level_1_results<-list()
    }
    # Set current thresholds 
    cur_th1<-l1_grid[i,1]
    cur_th2<-l1_grid[i,2]
    cur_th3<-l1_grid[i,3]
    
    # Run the model 25 times in the 12 environments and take the average
    for (j in 1:25){
      if (j==1){
        out_per_run_list<-list()
      }
      # Run the model 
      if(model_type==131){
        env_func_1_3_1_par_hpc(days = days, N= N, th_forage_sc1 = cur_th1 , th_forage_sc2 = cur_th2 , th_forage_sc3 = cur_th3, daylight_h = daylight_h, modelType = 131)
      }else if(model_type==132){
        env_func_1_3_2_par_hpc(days = days, N= N, th_forage_sc1 = cur_th1 , th_forage_sc2 = cur_th2 , th_forage_sc3 = cur_th3, daylight_h = daylight_h, modelType = 132)
        
      }else if(model_type==231){
        env_func_2_3_1_par_hpc(days = days, N= N, th_forage_fr1 = cur_th1 , th_forage_fr2 = cur_th2 , th_forage_fr3 = cur_th3, daylight_h = daylight_h, modelType = 231)
        
      }else if(model_type==232){
        env_func_2_3_2_par_hpc(days = days, N= N, th_forage_fr1 = cur_th1 , th_forage_fr2 = cur_th2 , th_forage_fr3 = cur_th3, daylight_h = daylight_h, modelType = 232)
        
      }else if(model_type==331){
        env_func_3_3_1_par_hpc(days = days, N= N, th_forage_flr1 = cur_th1 , th_forage_flr2 = cur_th2 , th_forage_flr3 = cur_th3, daylight_h = daylight_h, modelType = 331)
        
      }else if(model_type==332){
        env_func_3_3_2_par_hpc(days = days, N= N, th_forage_flr1 = cur_th1 , th_forage_flr2 = cur_th2 , th_forage_flr3 = cur_th3, daylight_h = daylight_h, modelType = 332)
        
      }else{
        print('problem with model_type settings')
      }
      out_per_run_list[j]<-cbind(output_env_func[[1]][1], cur_th1, cur_th2, cur_th3)
      print(paste(j))
    } # end of loop for 25 run s
    
    # bind together
    out_per_run_df<-data.frame(matrix(unlist(out_per_run_list), nrow=25, byrow=TRUE),stringsAsFactors=FALSE)
    colnames(out_per_run_df)<-"HL"
    # Now take the average for the threshold 
    mean_perf<-c(mean(out_per_run_df$HL), cur_th1, cur_th2, cur_th3)
    # add to list
    level_1_results[[i]]<-mean_perf
    # mark that things are working
    print(paste("Done with level 1 ", i))
  } # end of 1-4 branching loop
  
  halflife_out_l1<-data.frame(matrix(unlist(level_1_results), nrow=nrow(l1_grid), byrow=TRUE),stringsAsFactors=FALSE)
  names<-c("HL",  "th1", "th2", "th3")
  colnames(halflife_out_l1)<-names
  
  # select highest outcome
  max_HL_l1<-max(halflife_out_l1$HL)
  best_l1<-halflife_out_l1[halflife_out_l1$HL==max_HL_l1, ]
  #best_ths_l1<-best_l1[1,2]
  
  #### LEVEL 2 
  # Calculate the new ranges for level 2 
  # For threshold 1 
  range_min_l2_th1<-(best_l1$th1-(step_size_l1_th1/2))
  range_max_l2_th1<-(best_l1$th1+(step_size_l1_th1/2))
  # Same for threshold 2
  range_min_l2_th2<-(best_l1$th2-(step_size_l1_th2/2))
  range_max_l2_th2<-(best_l1$th2+(step_size_l1_th2/2))
  # and threshold 3
  range_min_l2_th3<-(best_l1$th3-(step_size_l1_th3)/2)
  range_max_l2_th3<-(best_l1$th3+(step_size_l1_th3)/2)
  
  # Run the branch function 
  vals_l2_th1<-branch_func(range_min = range_min_l2_th1, range_max = range_max_l2_th1, branch_num = 4)
  # save the stepsize used for this
  step_size_l2_th1<-step_size
  # run for second threshold 
  vals_l2_th2<-branch_func(range_min = range_min_l2_th2, range_max = range_max_l2_th2, branch_num = 4)
  # save the stepsize for this 
  step_size_l2_th2<-step_size
  # run for thrid threshold 
  vals_l2_th3<-branch_func(range_min = range_min_l2_th3, range_max = range_max_l2_th3, branch_num = 4)
  # save the stepsize for this 
  step_size_l2_th3<-step_size
  
  # Generate the possible combinations 
  # Take out the stepsize 
  vals_l2_th1<-vals_l2_th1[1:4]
  vals_l2_th2<-vals_l2_th2[1:4]
  vals_l2_th3<-vals_l2_th3[1:4]
  
  # Generate a grid with the possible combinations 
  l2_grid<-expand.grid(vals_l2_th1, vals_l2_th2, vals_l2_th3)
  names<-c("th1", "th2", "th3")
  colnames(l2_grid)<-names
  
  # For models x.1 of the first subset, th2 needs to be higher than th1 and th3 higher than th2 
  l2_grid<-l2_grid[((l2_grid$th1<l2_grid$th2)&(l2_grid$th3>l2_grid$th2)), ]
  
  # Now run the model 
  for (i in 1:nrow(l2_grid)){
    if (i==1){
      # Create empty list for results 
      level_2_results<-list()
    }
    
    # Set current thresholds 
    cur_th1<-l2_grid[i,1]
    cur_th2<-l2_grid[i,2]
    cur_th3<-l2_grid[i,3]
    
    # Run the model 25 times in the 12 environments and take the average
    for (j in 1:5){
      if (j==1){
        out_per_run_list<-list()
      }
      # Run the model 
      if(model_type==131){
        env_func_1_3_1_par_hpc(days = days, N= N, th_forage_sc1 = cur_th1 , th_forage_sc2 = cur_th2 , th_forage_sc3 = cur_th3, daylight_h = daylight_h, modelType = 131)
      }else if(model_type==132){
        env_func_1_3_2_par_hpc(days = days, N= N, th_forage_sc1 = cur_th1 , th_forage_sc2 = cur_th2 , th_forage_sc3 = cur_th3, daylight_h = daylight_h, modelType = 132)
        
      }else if(model_type==231){
        env_func_2_3_1_par_hpc(days = days, N= N, th_forage_fr1 = cur_th1 , th_forage_fr2 = cur_th2 , th_forage_fr3 = cur_th3, daylight_h = daylight_h, modelType = 231)
        
      }else if(model_type==232){
        env_func_2_3_2_par_hpc(days = days, N= N, th_forage_fr1 = cur_th1 , th_forage_fr2 = cur_th2 , th_forage_fr3 = cur_th3, daylight_h = daylight_h, modelType = 232)
        
      }else if(model_type==331){
        env_func_3_3_1_par_hpc(days = days, N= N, th_forage_flr1 = cur_th1 , th_forage_flr2 = cur_th2 , th_forage_flr3 = cur_th3, daylight_h = daylight_h, modelType = 331)
        
      }else if(model_type==332){
        env_func_3_3_2_par_hpc(days = days, N= N, th_forage_flr1 = cur_th1 , th_forage_flr2 = cur_th2 , th_forage_flr3 = cur_th3, daylight_h = daylight_h, modelType = 332)
        
      }else{
        print('problem with model_type settings')
      }
      # add the outcome 
      out_per_run_list[j]<-cbind(output_env_func[[1]][1], cur_th1, cur_th2, cur_th3)
      print(paste(j))
    } # end of loop for 25 run s
    
    # bind together
    out_per_run_df<-data.frame(matrix(unlist(out_per_run_list), nrow=25, byrow=TRUE),stringsAsFactors=FALSE)
    colnames(out_per_run_df)<-"HL"
    # Now take the average for the threshold 
    mean_perf<-c(mean(out_per_run_df$HL), cur_th1, cur_th2, cur_th3)
    # add to list
    level_2_results[[i]]<-mean_perf
    
    # mark that things are working
    print(paste("Done with level 2 ", i))
  } # end of 1-4 branching loop
  
  halflife_out_l2<-data.frame(matrix(unlist(level_2_results), nrow=nrow(l2_grid), byrow=TRUE),stringsAsFactors=FALSE)
  names<-c("HL",  "th1", "th2", "th3")
  colnames(halflife_out_l2)<-names
  
  # select highest outcome
  max_HL_l2<-max(halflife_out_l2$HL)
  best_l2<-halflife_out_l2[halflife_out_l2$HL==max_HL_l2, ]
  
  
  #### LEVEL 3 
  # Calculate the new ranges for level 3
  # For threshold 1 
  range_min_l3_th1<-(best_l2$th1-(step_size_l2_th1/2))
  range_max_l3_th1<-(best_l2$th1+(step_size_l2_th1/2))
  # Same for threshold 2
  range_min_l3_th2<-(best_l2$th2-(step_size_l2_th2/2))
  range_max_l3_th2<-(best_l2$th2+(step_size_l2_th2/2))
  # and threshold 3
  range_min_l3_th3<-(best_l2$th3-(step_size_l2_th3)/2)
  range_max_l3_th3<-(best_l2$th3+(step_size_l2_th3)/2)
  
  
  # Run the branch function 
  # for th 1
  vals_l3_th1<-branch_func(range_min = range_min_l3_th1, range_max = range_max_l3_th1, branch_num = 4)
  # stepseize
  step_size_l3_th1<-step_size
  # run for th 2
  vals_l3_th2<-branch_func(range_min = range_min_l3_th2, range_max = range_max_l3_th2, branch_num = 4)
  # save the stepsize for later
  step_size_l3_th2<-step_size
  # run for thrid threshold 
  vals_l3_th3<-branch_func(range_min = range_min_l3_th3, range_max = range_max_l3_th3, branch_num = 4)
  # save the stepsize for this 
  step_size_l3_th3<-step_size
  
  # Generate the possible combinations 
  # Take out the stepsize 
  vals_l3_th1<-vals_l3_th1[1:4]
  vals_l3_th2<-vals_l3_th2[1:4]
  vals_l3_th3<-vals_l3_th3[1:4]
  
  # Generate a grid with the possible combinations 
  l3_grid<-expand.grid(vals_l3_th1, vals_l3_th2, vals_l3_th3)
  names<-c("th1", "th2", "th3")
  colnames(l3_grid)<-names
  
  # For models x.2 of the first subset, th2 needs to be higher than th1 and th3 higher than th2
  l3_grid<-l3_grid[((l2_grid$th1<l3_grid$th2)&(l3_grid$th3>l3_grid$th2)), ]
  
  # Now run the model 
  for (i in 1:nrow(l3_grid)){
    if (i==1){
      # Create empty list for results 
      level_3_results<-list()
    }
    # Set current thresholds 
    cur_th1<-l3_grid[i,1]
    cur_th2<-l3_grid[i,2]
    cur_th3<-l3_grid[i,3]
    # Run the model 25 times in the 12 environments and take the average
    for (j in 1:5){
      if (j==1){
        out_per_run_list<-list()
      }
      # Run the model 
      if(model_type==131){
        env_func_1_3_1_par_hpc(days = days, N= N, th_forage_sc1 = cur_th1 , th_forage_sc2 = cur_th2 , th_forage_sc3 = cur_th3, daylight_h = daylight_h, modelType = 131)
      }else if(model_type==132){
        env_func_1_3_2_par_hpc(days = days, N= N, th_forage_sc1 = cur_th1 , th_forage_sc2 = cur_th2 , th_forage_sc3 = cur_th3, daylight_h = daylight_h, modelType = 132)
        
      }else if(model_type==231){
        env_func_2_3_1_par_hpc(days = days, N= N, th_forage_fr1 = cur_th1 , th_forage_fr2 = cur_th2 , th_forage_fr3 = cur_th3, daylight_h = daylight_h, modelType = 231)
        
      }else if(model_type==232){
        env_func_2_3_2_par_hpc(days = days, N= N, th_forage_fr1 = cur_th1 , th_forage_fr2 = cur_th2 , th_forage_fr3 = cur_th3, daylight_h = daylight_h, modelType = 232)
        
      }else if(model_type==331){
        env_func_3_3_1_par_hpc(days = days, N= N, th_forage_flr1 = cur_th1 , th_forage_flr2 = cur_th2 , th_forage_flr3 = cur_th3, daylight_h = daylight_h, modelType = 331)
        
      }else if(model_type==332){
        env_func_3_3_2_par_hpc(days = days, N= N, th_forage_flr1 = cur_th1 , th_forage_flr2 = cur_th2 , th_forage_flr3 = cur_th3, daylight_h = daylight_h, modelType = 332)
        
      }else{
        print('problem with model_type settings')
      }
      # add the output 
      out_per_run_list[j]<-cbind(output_env_func[[1]][1], cur_th1, cur_th2, cur_th3)
      print(paste(j))
    } # end of loop for 25 run s
    
    # bind together
    out_per_run_df<-data.frame(matrix(unlist(out_per_run_list), nrow=25, byrow=TRUE),stringsAsFactors=FALSE)
    colnames(out_per_run_df)<-"HL"
    # Now take the average for the threshold 
    mean_perf<-c(mean(out_per_run_df$HL), cur_th1, cur_th2, cur_th3)
    # add to list
    level_3_results[[i]]<-mean_perf
    
    # mark that things are working
    print(paste("Done with level 3 ", i))
  } # end of 1-4 branching loop - level 3
  
  halflife_out_l3<-data.frame(matrix(unlist(level_3_results), nrow=nrow(l3_grid), byrow=TRUE),stringsAsFactors=FALSE)
  names<-c("HL", "th1", "th2", "th3")
  colnames(halflife_out_l3)<-names
  
  # select highest outcome
  max_HL_l3<-max(halflife_out_l3$HL)
  best_l3<-halflife_out_l3[halflife_out_l3$HL==max_HL_l3, ]
  
  # Output the best threshold 
  return_list<-list(level_1_results, level_2_results, level_3_results, best_l3)
  return(return_list)
  
} # end of function for hpc 
  

