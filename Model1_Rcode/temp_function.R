# 08/11/2022
# try and write the temperature function 
# Vera Vinken 



###################################

t<-seq(0,24, 0.1)

y<-sin(t)

plot(t,y, type='l', xlab='time', ylab='wave')


y<-(sin(t)+sin(2*t))
plot(t,y, type='l', xlab='time', ylab='wave')


y<-sin(0.5*t)



y<-0.5*(sin(t)+sin(t))



y<-0.5*(sin(t)+|sin(t)|)


#########################################


# As in Welton et al. 2002 

# input for this function: 

    # 1. length of daylight hours (yes)
    # 2. Constant 1: looks like this is the time between peaks 
    # 3. Constant 2: looks like this is the midline 

# Lets assume a world with 3 20minute intervals in each hour 
# t=1 is the first 20 minutes after midnight 

# For a 6 hour day from 9.00-15.00 
#<-27
#end_day<-45

# calulate the number of daylight timesteps that you have 
#nDaylight<-end_day-start_day

# The variable that continas how many daylight timesteps: 

# timesteps in a day 
t<-(1:72)

# how long is it light for? 
# 6 hours means 18 timesteps 
hours<-6
n_daylight_timestep<-(3*hours)

# Identify the first constant
# I think this is for the distance between extremes 

# maximum reached on daytime
amp_day<<-(5)
# minimum reached on nighttime 
amp_night<<-(-10)

# Identify the second constant
# I think this is the mean temperature 
# My model currently has a day-temp and a night-temp 
# Lets say that from now on I give in the max and min 
#maxTemp<- -5
#minTemp<- -10

# I'll have to calculate the midline
#midTemp<-((maxTemp+minTemp)/2)
# put it in the constant 
# the mean temperature you want should be here: 
# this is the vertical shift 
v_shift<-(0)
v_shift<-(amp_day+amp_night)/2

# Now i have all the necessary variables available 
# Time to implement the functions 


# Make some place for the temperaturs to go in



temps<<-c()

for (timestep in 1:length(t)){
  # First determine the timestep within 1 day we are in 
  t_in24<-timestep  # not very necesasry here but should be in the final code 
  
  # First determine if youre in night or day 
  if (timestep<= n_daylight_timestep){
    # this means it is day 
    curr_temp<<-((amp_day*sin((t_in24*pi)/n_daylight_timestep))+v_shift)
    print(paste('it is day and the current temp is', curr_temp))
    # add the temperature to the vector 
    temps<<-append(temps,curr_temp)
  } else{                                          
    # this means it is night 
    curr_temp<<-(((-amp_night*sin(((t_in24+72-(2*n_daylight_timestep))*pi)/(72-n_daylight_timestep)))+v_shift))
    print(paste0('it is night', curr_temp))
    temps<<-append(temps,curr_temp)
  }
  
} # end of for loop 

yellow<-adjustcolor( 'lightgoldenrod', alpha.f = 0.5)
blue<-adjustcolor('skyblue', alpha.f=0.5)
plot(t,temps, main = paste('Temperature during the dayand night daylength=', hours, 'hours', 'day_amp= ', amp_day, ' night_amp= ', amp_night), ylab='Temperature in degrees Celcius',  xlab='Timestep (20min intervals)')
abline(v=n_daylight_timestep)
abline(h=v_shift, col='blue', lty=2)
polygon(x=c(0,0, n_daylight_timestep, n_daylight_timestep), y=c((min(temps)-2), (max(temps)+2), (max(temps)+2), (min(temps)-2)), col=yellow, border=F)
polygon(x=c(n_daylight_timestep, n_daylight_timestep, (length(t)+2), (length(t)+2)), y=c((min(temps)-2), (max(temps)+2), (max(temps)+2), ( min(temps)-2)), col=blue, border=F)


#polygon(x=c(0,0,n_daylight_timestep, n_daylight_timestep), y=c((amp_night-2),(amp_day+2),(amp_day+2),(amp_night-2)), col="#0000FF22", border=F)

temps



#############################

# new version wehere the amplitude is calcualted based on the midline and the min/max temperatures. 
# The amplitude is not the same as this min/max  values 

Temp_func1<-function(t_max, t_min, daylight_hours){
  # timesteps in a day 
  t<-(1:72)
  
  # how long is it day  in timesteps 
  n_daylight_timestep<-(3*daylight_hours)
  
  # Calculate the midline 
  midline<-((t_max+t_min)/2) 
  
  # Now calcualte the amplitudes 
  amp_day<-(t_max-midline)
  amp_night<-(t_min-midline)
  
  temps<<-c()
  
  for (timestep in 1:length(t)){
    # First determine the timestep within 1 day we are in 
    t_in24<-timestep  # not very necesasry here but should be in the final code 
    
    # First determine if youre in night or day 
    if (timestep<= n_daylight_timestep){
      # this means it is day 
      curr_temp<<-((amp_day*sin((t_in24*pi)/n_daylight_timestep))+midline)
      print(paste('it is day and the current temp is', curr_temp))
      # add the temperature to the vector 
      temps<<-append(temps,curr_temp)
    } else{                                          
      # this means it is night 
      curr_temp<<-(((-amp_night*sin(((t_in24+72-(2*n_daylight_timestep))*pi)/(72-n_daylight_timestep)))+midline))
      print(paste0('it is night', curr_temp))
      temps<<-append(temps,curr_temp)
    }
    
  } # end of for loop 
  
  yellow<-adjustcolor( 'lightgoldenrod', alpha.f = 0.5)
  blue<-adjustcolor('skyblue', alpha.f=0.5)
  plot(t,temps, main = paste('Temperature during the dayand night daylength=', daylight_hours, 'hours', 'day_amp= ', amp_day, ' night_amp= ', amp_night), ylab='Temperature in degrees Celcius',  xlab='Timestep (20min intervals)')
  abline(v=n_daylight_timestep)
  abline(h=midline, col='blue', lty=2)
  polygon(x=c(0,0, n_daylight_timestep, n_daylight_timestep), y=c((min(temps)-2), (max(temps)+2), (max(temps)+2), (min(temps)-2)), col=yellow, border=F)
  polygon(x=c(n_daylight_timestep, n_daylight_timestep, (length(t)+2), (length(t)+2)), y=c((min(temps)-2), (max(temps)+2), (max(temps)+2), ( min(temps)-2)), col=blue, border=F)
  
  # check some things 
  print(paste('mean temperature = ' ,  mean(temps)))
  
}

Temp_func1(5, -10, 6)
Temp_func1(1, -5, 8)


###############################

# Another version, but here the midline depends more on the night as this is longer 

Temp_func2<-function(t_max, t_min, daylight_hours){
  # timesteps in a day 
  t<-(1:72)
  
  # how long is it day  in timesteps 
  n_daylight_timestep<-(3*daylight_hours)
  
  # Calculate the midline 
  # midline<-((t_max+t_min)/2) 
  prop_day<-n_daylight_timestep/72
  prop_night<-1-prop_day
  midline<<-(((t_max*prop_day)+(t_min*prop_night))/2)
  
  
  # Now calcualte the amplitudes 
  amp_day<-(t_max-midline)
  amp_night<-(t_min-midline)
  
  temps<<-c()
  
  for (timestep in 1:length(t)){
    # First determine the timestep within 1 day we are in 
    t_in24<-timestep  # not very necesasry here but should be in the final code 
    
    # First determine if youre in night or day 
    if (timestep<= n_daylight_timestep){
      # this means it is day 
      curr_temp<<-((amp_day*sin((t_in24*pi)/n_daylight_timestep))+midline)
      print(paste('it is day and the current temp is', curr_temp))
      # add the temperature to the vector 
      temps<<-append(temps,curr_temp)
    } else{                                          
      # this means it is night 
      curr_temp<<-(((-amp_night*sin(((t_in24+72-(2*n_daylight_timestep))*pi)/(72-n_daylight_timestep)))+midline))
      print(paste0('it is night', curr_temp))
      temps<<-append(temps,curr_temp)
    }
    
  } # end of for loop 
  
  
  # code to plot 
      yellow<-adjustcolor( 'lightgoldenrod', alpha.f = 0.5)
      blue<-adjustcolor('skyblue', alpha.f=0.5)
      plot(t,temps, main = paste('Temperature during the dayand night daylength=', daylight_hours, 'hours', 'tmax= ', t_max, ' t_min= ', t_min), ylab='Temperature in degrees Celcius',  xlab='Timestep (20min intervals)')
      abline(v=n_daylight_timestep)
      abline(h=midline, col='blue', lty=2)
      polygon(x=c(0,0, n_daylight_timestep, n_daylight_timestep), y=c((min(temps)-2), (max(temps)+2), (max(temps)+2), (min(temps)-2)), col=yellow, border=F)
      polygon(x=c(n_daylight_timestep, n_daylight_timestep, (length(t)+2), (length(t)+2)), y=c((min(temps)-2), (max(temps)+2), (max(temps)+2), ( min(temps)-2)), col=blue, border=F)
      
  # check some things 
   print(paste('mean temperature = ' ,  mean(temps)))
}

Temp_func2(5, -10, 6)
Temp_func2(1, -5, 8)


# ################################

# input will be a string of temperatures 
days<-30
#num_timesteps<-days*72 # for 30 days 

# the following should be put in in the function 
max_range_low<-3
max_range_high<-8
min_range_low<--13
min_range_high<--8



# start the function that needs to happen before any for loops start 
Temp_func3<-function(max_range_low, max_range_high, min_range_low, min_range_high, days){
  
  # daylength 
  hours_daylight<<-6
  daylength_timesteps<<-(3*hours_daylight)
  nightlength_timesteps<<-(72-daylength_timesteps)
  
  num_timesteps<<-days*72 # for 30 days 
  # par(mfrow=c(1,1))
  # now create the vectors 
  # for an even random distribution: 
  
#### CHECK IF THIS IS WHAT WE WANT ### 
  max_temp_vect<<-sample(seq(max_range_low, max_range_high, length=(days)))
  min_temp_vect<<-sample(seq(min_range_low, min_range_high, length=(days)))
  plot(max_temp_vect, min_temp_vect)
  #now we have a temperature set for each day 
  
  # create an empty list where the day-profiles are going to go 
  total_temp_profile<<-c()

  
  # START THE FOR LOOP FOR EACH DAY 
      # Now for every day, we need to create a temperature PROFILE 
      for (i in (1:(length(max_temp_vect)))){
        
        # establish the current max and min for each loop (day)
        cur_Tmax<<-max_temp_vect[i]
        cur_Tmin<<-min_temp_vect[i]
        
        # The easy part first: create the line from max to min 
        # This will be the same for every day
              
              # first calculate the max temp time of day 
              
              # This is the code for 75% 
              #cur_maxT_time<-daylength_timesteps*0.75
              
              #Alternatively, we could do -2 horus from the end of daylight (in timesteps)
              cur_maxT_time<<-daylength_timesteps-(2*3)
              # minimum temp time of day (in timesteps)
              cur_minT_time<<-72-3 # Were just setting this at 1 hour before sunrise 
              
              # How far ar the max and min apart in time? 
              d_time_maxToMin<<-cur_minT_time-cur_maxT_time
              # how far are they apart in temperature? 
              d_temp_maxToMin<<-cur_Tmax-cur_Tmin
              # Step per timestep: 
              temp_steps_maxToMin<<-d_temp_maxToMin/d_time_maxToMin
              #Now generate a vector with a temperature for each timestep
              max_to_min_vector<<-seq(cur_Tmax,cur_Tmin, by=-temp_steps_maxToMin)   # notice the '-' because we're going down in temp
              # Notice that the vector is too long (it includes both the min and the max, and the vector below will do the same)
              # We start at max and end at min, here we cut of the last value 
              # so this vector includes the maximum 
              max_to_min_vector<<-max_to_min_vector[1:(length(max_to_min_vector)-1)]
              
          
              
        # NOW IT WILL SPLIT UP FOR DAY 1, DAY-END VS ALL THE OTHER DAYS
              
              if (i==1){
                
                # WRITE THE CODE FOR WHAT NEEDS TO HAPPEN AT DAY 1 
                print('help it is day 1, what do we do?')
                
                # just set them to the same ones as current? 
                prev_tempMin<<-min_temp_vect[i]
                next_tempMax<<-max_temp_vect[i]
              }
              
              else if (i==(length(max_temp_vect))){
                
                # WRITE THE CODE FOR WHAT NEEDS TO HAPPEN AT THE LAST DAY 
                print('help it is the last day, what do we do?')
                # just set them to the same ones as current? 
                prev_tempMin<<-min_temp_vect[i]
                next_tempMax<<-max_temp_vect[i]
              }
              
              else {
              
                print(' it is a normal day, yay! ')
              
                # previous day minimum 
                prev_tempMin<<-min_temp_vect[(i-1)]
                # next day maximum 
                next_tempMax<<-max_temp_vect[(i+1)]
                
              } # else for 'normal' days ends (excludes day 1 and the last day )
                
              # The res tof the code should be the same for everyone 
        
          # Frist decide what happens before the maximum (BEFORE MAXIMUM)
              # How far ar the min and max apart in time? 
              d_time_minToMax<<-72-d_time_maxToMin
              # how far are they apart in temperature? 
              d_temp_before<<-cur_Tmax-prev_tempMin
              # Step per timestep: 
              temp_steps_before<-d_temp_before/d_time_minToMax
              #Now generate a vector with a temperature for each timestep
              before_max_vector<<-seq(prev_tempMin,cur_Tmax, by=temp_steps_before)
              # Here we start at min and end at max, so we cut of the max value to not double it 
              # So this vector includes the minimum 
              before_max_vector<<-before_max_vector[1:(length(before_max_vector)-1)]
              
          # Now do the same but for min to max (AFTER THE MINIMUM)
              
              # How far ar the min and mas apart in time? (not needed, specified above)
              #d_time_minToMax<-72-d_time_maxToMin
             
               # how far are they apart in temperature? 
              d_temp_after<<-next_tempMax-cur_Tmin
              # Step per timestep: 
              temp_steps_after<<-d_temp_after/d_time_minToMax
              #Now generate a vector with a temperature for each timestep
              after_min_vector<<-seq(cur_Tmin,next_tempMax, by=temp_steps_after)
              # Here we start at min and end at max, so we cut of the max value to not double it 
              # So this vector includes the minimum 
              after_min_vector<<-after_min_vector[1:(length(after_min_vector)-1)]
              
        # Now we need to amend the vectors together in the right fashion 
              
              # First determine how many timesteps before the maxTemp
              # we already know this: cur_maxT_time 
              # so we need to select the last temperatures of the min_to_max vector 
              before_max_vector<<-before_max_vector[5:(length(before_max_vector))]   # note that the '4' is hardcoded. The time of minimum is set at 69. 
              #The min-t-max vector includes the minimum (set above) so the first 3 numbers will be at the end of the day
              # now do the same but for the temperatures at the end of the day (after min but before sunrise)
              after_min_vector<<-after_min_vector[1:4]
              # Glue the 3 vectors together 
              cur_day_temp_vector<<-c(before_max_vector, max_to_min_vector, after_min_vector)
              
              
              # just print a day profile 
              if (i==15){
                timesteps_day<-1:72
                yellow<-adjustcolor( 'lightgoldenrod', alpha.f = 0.5)
                blue<-adjustcolor('skyblue', alpha.f=0.5)
                plot(timesteps_day, cur_day_temp_vector, main=(paste('Temp profile for day', i, 'max-range=', max_range_low, 'to', max_range_high, ', min-range=', min_range_low, 'to', min_range_high)), type='l', col='red', ylab='Temp in degrees C', xlab='Timesteps in 20 min intervals')
                abline(v=daylength_timesteps)
                polygon(x=c(0,0, daylength_timesteps, daylength_timesteps), y=c((min(cur_day_temp_vector)-2), (max(cur_day_temp_vector)+2), (max(cur_day_temp_vector)+2), (min(cur_day_temp_vector)-2)), col=yellow, border=F)
                polygon(x=c(daylength_timesteps, daylength_timesteps, (length(timesteps_day)), (length(timesteps_day))), y=c((min(cur_day_temp_vector)-2), (max(cur_day_temp_vector)+2), (max(cur_day_temp_vector)+2), ( min(cur_day_temp_vector)-2)), col=blue, border=F)
                print(paste('minimum temp =',cur_Tmin))
                print(paste('minimum temp prev day=',prev_tempMin))
                print(cur_day_temp_vector)
                
                }
    
        # Ok cool, I have a vector that has the temperatures for every day. 
        # Now save them somewhere so they can all be amended 
              total_temp_profile<<-c(total_temp_profile, cur_day_temp_vector)
      } # for loop per day ends 
    
  # plot the thing 
  timesteps<-1:num_timesteps
  vert_lines<-seq(0, 2160, by=30)
  plot(timesteps, total_temp_profile, type='l', main=(paste('Temp profile for ', days, 'days', ', max-range=', max_range_low, 'to', max_range_high, ', min-range=', min_range_low, 'to', min_range_high)), col='red', ylab = 'Temperature in degrees Celsius', xlab='timesteps (20min)')
 
} # temp function ends 


Temp_func3(3, 8, -13, -8, 30)

Temp_func3(2, 3, -3, -1, 30)



########################################

# now make the minimum at start of day 

Temp_func4<-function(max_range_low, max_range_high, min_range_low, min_range_high, days, daylength_h){
  
  # daylength 
  hours_daylight<<-daylength_h
  daylength_timesteps<<-(3*hours_daylight)
  nightlength_timesteps<<-(72-daylength_timesteps)
  
  num_timesteps<<-days*72 # for 30 days 
  
  # As we will set the minimum at the end of the day/start of the day
  # we can set the following before the for-loop: 
  minTemp_time_end<<-72
  #minTemp_time_start<<-1
  # par(mfrow=c(1,1))
  # now create the vectors 
  # for an even random distribution: 
  
  #### CHECK IF THIS IS WHAT WE WANT ### 
  max_temp_vect<<-sample(seq(max_range_low, max_range_high, length=(days)))
  min_temp_vect<<-sample(seq(min_range_low, min_range_high, length=(days)))
  plot(max_temp_vect, min_temp_vect)
  #now we have a temperature set for each day 
  
  # create an empty list where the day-profiles are going to go 
  total_temp_profile<<-c()
  
  
  # START THE FOR LOOP FOR EACH DAY 
  # Now for every day, we need to create a temperature PROFILE 
  for (i in (1:days)){
    
    # establish the current max and min for each loop (day)
    cur_Tmax<<-max_temp_vect[i]
    cur_Tmin<<-min_temp_vect[i]
    
    # The easy part first: create the line from max to min 
    # This will be the same for every day
    
    # first calculate the max temp time of day 
    
    # This is the code for 75% 
    cur_maxT_time<<-round((daylength_timesteps*0.75))
    
    #Alternatively, we could do -2 horus from the end of daylight (in timesteps)
    #cur_maxT_time<<-daylength_timesteps-(2*3)
    
    # minimum temp time of day (in timesteps)
    # cur_minT_time<<-72 # Were just setting this at sunrise
    
    # How far ar the max and min apart in time? 
    d_time_maxToMin<<-minTemp_time_end-cur_maxT_time
    # how far are they apart in temperature? 
    d_temp_maxToMin<<-cur_Tmax-cur_Tmin
    # Step per timestep: 
    temp_steps_maxToMin<<-d_temp_maxToMin/d_time_maxToMin
    #Now generate a vector with a temperature for each timestep
    max_to_min_vector<<-seq(cur_Tmax,cur_Tmin, by=-temp_steps_maxToMin)   # notice the '-' because we're going down in temp
    # Notice that the vector is too long (it includes both the min and the max, and the vector below will do the same)
    # We start at max and end at min, here we cut of the last value 
    # so this vector includes the maximum but not the minimum 
    #max_to_min_vector<<-max_to_min_vector[1:(length(max_to_min_vector)-1)]
    
    
    
    # NOW IT WILL SPLIT UP FOR DAY 1, DAY-END VS ALL THE OTHER DAYS
    
    if (i==1){
      
      # WRITE THE CODE FOR WHAT NEEDS TO HAPPEN AT DAY 1 
      print('help it is day 1, what do we do?')
      
      # just set them to the same ones as current? 
      prev_tempMin<<-min_temp_vect[i]
      next_tempMax<<-max_temp_vect[i]
    }
    
    else if (i==(length(max_temp_vect))){
      
      # WRITE THE CODE FOR WHAT NEEDS TO HAPPEN AT THE LAST DAY 
      print('help it is the last day, what do we do?')
      # just set them to the same ones as current? 
      prev_tempMin<<-min_temp_vect[i]
      next_tempMax<<-max_temp_vect[i]
    }
    
    else {
      
      print(' it is a normal day, yay! ')
      
      # previous day minimum 
      prev_tempMin<<-min_temp_vect[(i-1)]
      # next day maximum 
      next_tempMax<<-max_temp_vect[(i+1)]
      
    } # else for 'normal' days ends (excludes day 1 and the last day )
    
    # The res tof the code should be the same for everyone 
    
    # Frist decide what happens before the maximum (BEFORE MAXIMUM)
    # How far ar the min and max apart in time? 
    #d_time_minToMax<<-72-d_time_maxToMin
    
    # how far are they apart in temperature? 
    d_temp_before<<-cur_Tmax-prev_tempMin
    # Step per timestep: 
    temp_steps_before<-d_temp_before/cur_maxT_time
    #Now generate a vector with a temperature for each timestep
    before_max_vector<<-seq(prev_tempMin,cur_Tmax, by=temp_steps_before)
    # Here we start at min and end at max, so we cut of both min and max (they are include din the other vector)
    # So this vector includes the minimum 
    before_max_vector<<-before_max_vector[2:(length(before_max_vector)-1)]
    
    # Now do the same but for min to max (AFTER THE MINIMUM)
    
    # How far ar the min and mas apart in time? (not needed, specified above)
    #d_time_minToMax<-72-d_time_maxToMin
    
    # how far are they apart in temperature? 
    #d_temp_after<<-next_tempMax-cur_Tmin
    # Step per timestep: 
    #temp_steps_after<<-d_temp_after/d_time_minToMax
    #Now generate a vector with a temperature for each timestep
   # after_min_vector<<-seq(cur_Tmin,next_tempMax, by=temp_steps_after)
    # Here we start at min and end at max, so we cut of the max value to not double it 
    # So this vector includes the minimum 
    #after_min_vector<<-after_min_vector[1:(length(after_min_vector)-1)]
    
    # Now we need to amend the vectors together in the right fashion 
    
    # First determine how many timesteps before the maxTemp
    # we already know this: cur_maxT_time 
    
    # so we need to select the last temperatures of the min_to_max vector 
    #before_max_vector<<-before_max_vector  # note that the '4' is hardcoded. The time of minimum is set at 69. 
    #The min-t-max vector includes the minimum (set above) so the first 3 numbers will be at the end of the day
    # now do the same but for the temperatures at the end of the day (after min but before sunrise)
    #after_min_vector<<-after_min_vector[1:4]
    # Glue the 3 vectors together 
    cur_day_temp_vector<<-c(before_max_vector, max_to_min_vector)
    
    
    # just print a day profile 
    if (i==15){
      timesteps_day<-1:72
      yellow<-adjustcolor( 'lightgoldenrod', alpha.f = 0.5)
      blue<-adjustcolor('skyblue', alpha.f=0.5)
      plot(timesteps_day, cur_day_temp_vector, main=(paste('Temp profile for day', i, 'max-range=', max_range_low, 'to', max_range_high, ', min-range=', min_range_low, 'to', min_range_high)), type='l', col='red', ylab='Temp in degrees C', xlab='Timesteps in 20 min intervals')
      abline(v=daylength_timesteps)
      polygon(x=c(0,0, daylength_timesteps, daylength_timesteps), y=c((min(cur_day_temp_vector)-2), (max(cur_day_temp_vector)+2), (max(cur_day_temp_vector)+2), (min(cur_day_temp_vector)-2)), col=yellow, border=F)
      polygon(x=c(daylength_timesteps, daylength_timesteps, (length(timesteps_day)), (length(timesteps_day))), y=c((min(cur_day_temp_vector)-2), (max(cur_day_temp_vector)+2), (max(cur_day_temp_vector)+2), ( min(cur_day_temp_vector)-2)), col=blue, border=F)
      print(paste('minimum temp =',cur_Tmin))
      print(paste('minimum temp prev day=',prev_tempMin))
      print(cur_day_temp_vector)
      
    }
    
    # Ok cool, I have a vector that has the temperatures for every day. 
    # Now save them somewhere so they can all be amended 
    total_temp_profile<<-c(total_temp_profile, cur_day_temp_vector)
  } # for loop per day ends 
  
  # plot the thing 
  timesteps<-1:num_timesteps
  vert_lines<-seq(0, 2160, by=30)
  plot(timesteps, total_temp_profile, type='l', main=(paste('Temp profile for ', days, 'days', ', max-range=', max_range_low, 'to', max_range_high, ', min-range=', min_range_low, 'to', min_range_high)), col='red', ylab = 'Temperature in degrees Celsius', xlab='timesteps (20min)')
  
} # temp function ends 


# Try it out 
Temp_func4(max_range_low = 2, max_range_high = 4, min_range_low = -5, min_range_high = -2, days = 30, daylength_h = 10)

Temp_func4(max_range_low = 2, max_range_high = 4, min_range_low = -5, min_range_high = -2, days = 30, daylength_h = 8)

Temp_func4(max_range_low = 2, max_range_high = 4, min_range_low = -5, min_range_high = -2, days = 30, daylength_h = 6)


###############################################

# prepare the previous function 4 to go into the model 

Temp_func4<-function(max_range_low, max_range_high, min_range_low, min_range_high, days, daylength_h){
  
  # daylength 
  hours_daylight<<-daylength_h
  daylength_timesteps<<-(3*hours_daylight)
  nightlength_timesteps<<-(72-daylength_timesteps)
  
  num_timesteps<<-days*72 # for 30 days 
  
  # As we will set the minimum at the end of the day/start of the day
  # we can set the following before the for-loop: 
  minTemp_time_end<<-72
  #minTemp_time_start<<-1
  # par(mfrow=c(1,1))
  # now create the vectors 
  # for an even random distribution: 
  
  #### CHECK IF THIS IS WHAT WE WANT ### 
  max_temp_vect<<-sample(seq(max_range_low, max_range_high, length=(days)))
  min_temp_vect<<-sample(seq(min_range_low, min_range_high, length=(days)))
  plot(max_temp_vect, min_temp_vect)
  #now we have a temperature set for each day 
  
  # create an empty list where the day-profiles are going to go 
  total_temp_profile<<-c()
  
  
  # START THE FOR LOOP FOR EACH DAY 
  # Now for every day, we need to create a temperature PROFILE 
  for (i in (1:days)){
    
    # establish the current max and min for each loop (day)
    cur_Tmax<<-max_temp_vect[i]
    cur_Tmin<<-min_temp_vect[i]
    
    # The easy part first: create the line from max to min 
    # This will be the same for every day
    
    # first calculate the max temp time of day 
    
    # This is the code for 75% 
    cur_maxT_time<<-round((daylength_timesteps*0.75))
    
    #Alternatively, we could do -2 horus from the end of daylight (in timesteps)
    #cur_maxT_time<<-daylength_timesteps-(2*3)
    
    # minimum temp time of day (in timesteps)
    # cur_minT_time<<-72 # Were just setting this at sunrise
    
    # How far ar the max and min apart in time? 
    d_time_maxToMin<<-minTemp_time_end-cur_maxT_time
    # how far are they apart in temperature? 
    d_temp_maxToMin<<-cur_Tmax-cur_Tmin
    # Step per timestep: 
    temp_steps_maxToMin<<-d_temp_maxToMin/d_time_maxToMin
    #Now generate a vector with a temperature for each timestep
    max_to_min_vector<<-seq(cur_Tmax,cur_Tmin, by=-temp_steps_maxToMin)   # notice the '-' because we're going down in temp
    # Notice that the vector is too long (it includes both the min and the max, and the vector below will do the same)
    # We start at max and end at min, here we cut of the last value 
    # so this vector includes the maximum but not the minimum 
    #max_to_min_vector<<-max_to_min_vector[1:(length(max_to_min_vector)-1)]
    
    
    
    # NOW IT WILL SPLIT UP FOR DAY 1, DAY-END VS ALL THE OTHER DAYS
    
    if (i==1){
      
      # WRITE THE CODE FOR WHAT NEEDS TO HAPPEN AT DAY 1 
      print('help it is day 1, what do we do?')
      
      # just set them to the same ones as current? 
      prev_tempMin<<-min_temp_vect[i]
      next_tempMax<<-max_temp_vect[i]
    }
    
    else if (i==(length(max_temp_vect))){
      
      # WRITE THE CODE FOR WHAT NEEDS TO HAPPEN AT THE LAST DAY 
      print('help it is the last day, what do we do?')
      # just set them to the same ones as current? 
      prev_tempMin<<-min_temp_vect[i]
      next_tempMax<<-max_temp_vect[i]
    }
    
    else {
      
      print(' it is a normal day, yay! ')
      
      # previous day minimum 
      prev_tempMin<<-min_temp_vect[(i-1)]
      # next day maximum 
      next_tempMax<<-max_temp_vect[(i+1)]
      
    } # else for 'normal' days ends (excludes day 1 and the last day )
    
    # The res tof the code should be the same for everyone 
    
    # Frist decide what happens before the maximum (BEFORE MAXIMUM)
    # How far ar the min and max apart in time? 
    #d_time_minToMax<<-72-d_time_maxToMin
    
    # how far are they apart in temperature? 
    d_temp_before<<-cur_Tmax-prev_tempMin
    # Step per timestep: 
    temp_steps_before<-d_temp_before/cur_maxT_time
    #Now generate a vector with a temperature for each timestep
    before_max_vector<<-seq(prev_tempMin,cur_Tmax, by=temp_steps_before)
    # Here we start at min and end at max, so we cut of both min and max (they are include din the other vector)
    # So this vector includes the minimum 
    before_max_vector<<-before_max_vector[2:(length(before_max_vector)-1)]
    
    # Now do the same but for min to max (AFTER THE MINIMUM)
    
    # How far ar the min and mas apart in time? (not needed, specified above)
    #d_time_minToMax<-72-d_time_maxToMin
    
    # how far are they apart in temperature? 
    #d_temp_after<<-next_tempMax-cur_Tmin
    # Step per timestep: 
    #temp_steps_after<<-d_temp_after/d_time_minToMax
    #Now generate a vector with a temperature for each timestep
    # after_min_vector<<-seq(cur_Tmin,next_tempMax, by=temp_steps_after)
    # Here we start at min and end at max, so we cut of the max value to not double it 
    # So this vector includes the minimum 
    #after_min_vector<<-after_min_vector[1:(length(after_min_vector)-1)]
    
    # Now we need to amend the vectors together in the right fashion 
    
    # First determine how many timesteps before the maxTemp
    # we already know this: cur_maxT_time 
    
    # so we need to select the last temperatures of the min_to_max vector 
    #before_max_vector<<-before_max_vector  # note that the '4' is hardcoded. The time of minimum is set at 69. 
    #The min-t-max vector includes the minimum (set above) so the first 3 numbers will be at the end of the day
    # now do the same but for the temperatures at the end of the day (after min but before sunrise)
    #after_min_vector<<-after_min_vector[1:4]
    # Glue the 3 vectors together 
    cur_day_temp_vector<<-c(before_max_vector, max_to_min_vector)
    
    
    # just print a day profile 
    if (i==15){
      timesteps_day<-1:72
      yellow<-adjustcolor( 'lightgoldenrod', alpha.f = 0.5)
      blue<-adjustcolor('skyblue', alpha.f=0.5)
      plot(timesteps_day, cur_day_temp_vector, main=(paste('Temp profile for day', i, 'max-range=', max_range_low, 'to', max_range_high, ', min-range=', min_range_low, 'to', min_range_high)), type='l', col='red', ylab='Temp in degrees C', xlab='Timesteps in 20 min intervals')
      abline(v=daylength_timesteps)
      polygon(x=c(0,0, daylength_timesteps, daylength_timesteps), y=c((min(cur_day_temp_vector)-2), (max(cur_day_temp_vector)+2), (max(cur_day_temp_vector)+2), (min(cur_day_temp_vector)-2)), col=yellow, border=F)
      polygon(x=c(daylength_timesteps, daylength_timesteps, (length(timesteps_day)), (length(timesteps_day))), y=c((min(cur_day_temp_vector)-2), (max(cur_day_temp_vector)+2), (max(cur_day_temp_vector)+2), ( min(cur_day_temp_vector)-2)), col=blue, border=F)
      print(paste('minimum temp =',cur_Tmin))
      print(paste('minimum temp prev day=',prev_tempMin))
      print(cur_day_temp_vector)
      
    }
    
    # Ok cool, I have a vector that has the temperatures for every day. 
    # Now save them somewhere so they can all be amended 
    total_temp_profile<<-c(total_temp_profile, cur_day_temp_vector)
  } # for loop per day ends 
  
  # plot the thing 
  timesteps<-1:num_timesteps
  vert_lines<-seq(0, 2160, by=30)
  plot(timesteps, total_temp_profile, type='l', main=(paste('Temp profile for ', days, 'days', ', max-range=', max_range_low, 'to', max_range_high, ', min-range=', min_range_low, 'to', min_range_high)), col='red', ylab = 'Temperature in degrees Celsius', xlab='timesteps (20min)')
  
} # temp function ends 




