### 27/10/2024
# Vera Vinken
# creating a temp profile graph

# copied this function and un-hashed the graph part 
vera_theme <- function(bg) {
  if(bg == "white"){
    bg<-"white"
  } else if (bg == "yellow"){
    bg<-"ivory"
  }
  theme(
    # add border 1)
    panel.border = element_rect(colour = "darkgrey", fill = NA, linetype = 1),
    # color background 2)
    panel.background = element_rect(fill = paste(bg)),
    # modify grid 3)
    panel.grid.major.x = element_line(colour = "darkgrey", linetype = 3, size = 0.5),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y =  element_line(colour = "darkgrey", linetype = 3, size = 0.5),
    panel.grid.minor.y = element_blank(),
    # modify text, axis and colour 4) and 5)
    axis.text = element_text(colour = "#484537", face = "italic"),
    axis.title = element_text(colour = "#484537", 
                              face="bold"),
    axis.title.y = element_text(vjust = +3), 
    axis.title.x = element_text(vjust = -2.5),
    axis.ticks = element_line(colour = "#484537"),
    # legend at the bottom 6)
    legend.position = "right", 
    legend.title = element_text(face="bold", colour="#484537"),
    legend.text = element_text( colour="#484537"),
    # title
    plot.title = element_text(colour="#484537", face="bold"),
    # plot margins 
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
  )
}



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
    
    # print a day profile
    if (i==15){
      timesteps_day<-1:72
      yellow<-adjustcolor( 'lightgoldenrod', alpha.f = 0.5)
      blue<-adjustcolor('skyblue', alpha.f=0.5)
      plot(timesteps_day, cur_day_temp_vector, type='l', col='red', ylab='Temp in degrees C', xlab='Timesteps in 20 min intervals')
      abline(v=n_daylight_timestep)
      polygon(x=c(0,0, n_daylight_timestep, n_daylight_timestep), y=c((min(cur_day_temp_vector)-2), (max(cur_day_temp_vector)+2), (max(cur_day_temp_vector)+2), (min(cur_day_temp_vector)-2)), col=yellow, border=F)
      polygon(x=c(n_daylight_timestep, n_daylight_timestep, (length(timesteps_day)), (length(timesteps_day))), y=c((min(cur_day_temp_vector)-2), (max(cur_day_temp_vector)+2), (max(cur_day_temp_vector)+2), ( min(cur_day_temp_vector)-2)), col=blue, border=F)
      print(paste('minimum temp =',cur_Tmin))
      print(paste('minimum temp prev day=',prev_tempMin))
      print(cur_day_temp_vector)

    }


    # ATTACH THE CURRENT DAY PROFILE TO THE BIG ONE 
    # Ok cool, I have a vector that has the temperatures for every day. 
    # Now save them somewhere so they can all be amended 
    total_temp_profile<<-c(total_temp_profile, cur_day_temp_vector)
    
    #Create a matrix alternative, which can be saved 
    mat_temp<<-t(as.matrix(total_temp_profile))
    
  } # for loop per day ends 
  
  
} # temp function ends 

temp_func(TS = 2160, Tmax_range_low = 7, Tmax_range_high = 13, Tmin_range_low = -3, Tmin_range_high = 3, days = 30, daylight_h = 8, n_daylight_timestep = 24)

timesteps_day<-1:72
n_daylight_timestep = 24
df<-as.data.frame(cbind(timesteps_day, cur_day_temp_vector))
ggplot(df, aes(x = timesteps_day, y = cur_day_temp_vector)) +
  geom_line(lwd = 1) +
  ylab("Temperature in degrees Celsius") +
  xlab("Timestep within day (20 min)") +
  vera_theme(bg = "white")+
  geom_polygon( aes(x = c(0, 0, 24, 24),
                   y = c(-14.17241, 0.6551724,
                         0.6551724, -14.17241),
               fill = "yellow", color = NA))# +


# Define a separate data frame for the polygon
polygon_data <- data.frame(
  x = c(0, 0, 24, 24),
  y = c(-0.03448276, 11.68966, 11.68966, -0.03448276)
)

polygon_data2 <- data.frame(
  x = c(24, 24, 72, 72),
  y = c(-0.03448276, 11.68966, 11.68966, -0.03448276)
)

# Plot with polygon
p<-ggplot(df, aes(x = timesteps_day, y = cur_day_temp_vector)) +
  geom_line(lwd = 1) +
  ylab("Temperature in degrees Celsius") +
  xlab("Timestep within day (20 min)") +
  vera_theme(bg = "white") +
  geom_polygon(data = polygon_data, aes(x = x, y = y), fill = alpha("lightgoldenrod", 0.6), color = NA)+
  geom_polygon(data = polygon_data2, aes(x = x, y = y), fill = alpha("skyblue", 0.6), color = NA)+
  geom_line(lwd = 1)


## save 
ggsave("standardized_plot_graph_general.png", plot = p, width = 7, height = 4, dpi = 500)  # Specify the size in inches and resolution

ggsave("temp_profile_fig4.3_svg.svg", plot = p, width = 7, height = 4, dpi = 500)  # Specify the size in inches and resolution


