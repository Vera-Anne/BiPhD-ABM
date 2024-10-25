b_size<-24
num_food_mean<-3
cur_forage_type<-"forage-b"

# calculate the number of times 'nothing' is found given the bonanza size and teh mean
num_zero<<-round(((b_size-num_food_mean)/num_food_mean))
# string of options
item_options<<-c(b_size, (rep(0, num_zero)))

vect<-NA

for (i in 1:1000){
  
  # if within bonanza, are yo ugong to B forage or P forage? 
  cur_forage_type<<-sample(c('forage-b', 'forage-p'), size=1, replace=TRUE, prob=c(prob_b_forage, (1-prob_b_forage)))
  
  
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
  vect<-c(vect, food_item_found)
}

vect<-as.data.frame(vect)


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

p<-ggplot(vect, aes(x = vect))+
  geom_bar(col = "goldenrod", fill = "goldenrod")+
  vera_theme(bg = "white")+
  xlab(label = "Number of food items found")+
  ylab(label = "Frequency")


#### the same for poisson 
vect_p<-NA

for (i in 1:1000){
  
 
    food_item_found<<-sample(rpois(100, num_food_mean),1)
    #print(paste('Normal foraging with', food_item_found, 'items found'))

  vect_p<-c(vect_p, food_item_found)
}

vect_p<-as.data.frame(vect_p)

p<-ggplot(vect_p, aes(x = vect_p))+
  geom_bar(col = "goldenrod", fill = "goldenrod")+
  vera_theme(bg = "white")+
  xlim(-1,24)+
  ylim(0, 500)+
  xlab(label = "Number of food items found")+
  ylab(label = "Frequency")


## save 
ggsave("standardized_plot_graph_general.png", plot = p, width = 6, height = 4, dpi = 500)  # Specify the size in inches and resolution


##################################
means_p<-NA

for (i in 1:1000){
# loop
vect_p<-NA
for (i in 1:1000){
  

    
    
    food_item_found<<-sample(rpois(100, num_food_mean),1)
    #print(paste('Normal foraging with', food_item_found, 'items found'))
    
    vect_p<-c(vect_p, food_item_found)

}
vect_p<-vect_p[2:1001]
mean_items<-mean(vect_p)

means_p<-c(means_p, mean_items)

}

# the means of 1000 simulations of a 1000 foraging attempts 
hist(means_p)

means_p<-as.data.frame(means_p)

p<-ggplot(means_p, aes(x = means_p))+
  geom_histogram()

##################################
means<-NA

for (i in 1:1000){
  # loop
  vect<-NA
  for (i in 1:1000){
    
    # if within bonanza, are yo ugong to B forage or P forage? 
    cur_forage_type<<-sample(c('forage-b', 'forage-p'), size=1, replace=TRUE, prob=c(prob_b_forage, (1-prob_b_forage)))
    
    
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
    vect<-c(vect, food_item_found)
  }
  vect<-vect[2:1001]
  mean_items<-mean(vect)
  
  means<-c(means, mean_items)
  
}

# the means of 1000 simulations of a 1000 foraging attempts 
hist(means)
