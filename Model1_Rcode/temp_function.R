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
