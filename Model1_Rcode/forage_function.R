# foraging function 


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
library(withr)
library('plyr')


# input of the function 

mean_items_found<-3


distribution<-rpois(100, mean_items_found)
hist(distribution, main='Food items found (100)', xlab='food items', ylab = 'Frequency')
####distribution<-rpois(100, mean_items_found)


test<-0.5

for (i in 1:1000){
  test<-cbind(test, sample(c(1,0), size=1, replace=TRUE, prob=c(0.8, 0.2)))
}

mean(test)


set.seed(1) 
test<-

  
  # temporary vars 
prob_b_forage<-0.5
mean_food<-3
bonanza_size<-25
b_prob<-0.2
  
  
  
#######################  FORAGE FUNCTION WITH BONANZA-SIZE KNOWN   ######################
  
forage_function_sizeKnown<-function(mean_food, prob_b_forage, bonanza_size){
  # First decide if you're going into bonanza chances or normal foraging 
  cur_forage_type<<-sample(c('forage-b', 'forage-n'), size=1, replace=TRUE, prob=c(prob_b_forage, (1-prob_b_forage)))
  
  if (cur_forage_type=='forage-b'){ 
    # Write code for bonanza here
    # we know the mean
    # set the size of your bonanza (should be input)
    # calculate the amount of times you need to find 0
    # pull a number from that distribution
        # calculate the number of times 'nothing' is found given the bonanza size and teh mean
    num_zero<<-round(((bonanza_size-mean_food)/mean_food))
    # string of options
    item_options<<-c(bonanza_size, (rep(0, num_zero)))
    # pick an item from these randomly
    food_item_found<<-sample(item_options,1)
    print(paste('Bonanza foraging with ', food_item_found, 'items found'))

  }
  
  else {
    # Write code for normal foraging here 
    # Can be the poisson distributuion 
    # pull a number from the poisson 
    food_item_found<<-sample(rpois(100, mean_food),1)
    print(paste('Normal foraging with', food_item_found, 'items found'))
    
  }
  
  
} # end of the foraging function 

# test the function 
forage_function_sizeKnown(mean_food=3, prob_b_forage=0.5, bonanza_size = 25)


# Check how the probabilities work out 

# empty variable 
items_found_100s<-c()
# Run the forage function 100x
for (i in 1:100){
  forage_function_sizeKnown(mean_food=3, prob_b_forage=0.2, bonanza_size =33)
  items_found_100s<-c(items_found_100s, food_item_found)
  
}

# Plot the outcome 
count_s<-count(items_found_100s)
#pie(pie$freq, pie$x, main='Bonanza vs zero')
#barplot(count_s$freq, count_s$x)
count_s$x<-as.factor(count_s$x)
plot_s<-ggplot(count_s, aes(x, freq, fill=x))+
  geom_col()+
  labs(
    title=paste('# items found in 100x forage  with mean=', mean(items_found_100s))
  )
plot_s



#####################   FORAGE FUNCTION WITH BONANZA PROB KNOWN #####################

forage_function_probKnown<-function(mean_food, prob_b_forage, b_prob){
  # First decide if you're going into bonanza chances or normal foraging 
  cur_forage_type<<-sample(c('forage-b', 'forage-n'), size=1, replace=TRUE, prob=c(prob_b_forage, (1-prob_b_forage)))
  
  if (cur_forage_type=='forage-b'){ 

    # Alternatively: calculate the size of teh bonanza, given the bonanza probability and the mean 
    # calcualte how many options in total
    total_options<-round((1/b_prob))
    # calcualte how many 0 
    num_zero<<-(total_options-1)
    # calculate bonanza size
    bonanza_size<<-(mean_food*total_options)
    # string of optoins 
    item_options<<-c(bonanza_size, (rep(0, num_zero)))
    # pick an item from these randomly 
    food_item_found<<-sample(item_options,1)
    print(paste('Bonanza foraging with ', food_item_found, 'items found'))
  }
  
  else {
    # Write code for normal foraging here 
    # Can be the poisson distributuion 
    # pull a number from the poisson 
    # Write code for normal foraging here 
    # Can be the poisson distributuion 
    # pull a number from the poisson 
    food_item_found<<-sample(rpois(100, mean_food),1)
    print(paste('Normal foraging with', food_item_found, 'items found'))
    
  }
  
  
} # end of the foraging function 



forage_function_probKnown(mean_food=3, prob_b_forage=0.2, b_prob=0.1)
# Check how the probabilities work out 

# empty variable 
items_found_100p<-c()
# Run the forage function 100x
for (i in 1:100){
  forage_function_probKnown(mean_food=3, prob_b_forage=0.2, b_prob=0.3)
  items_found_100p<-c(items_found_100p, food_item_found)
  
}
# Plot the outcome 
# Plot the outcome 
count_p<-count(items_found_100p)
#pie(pie$freq, pie$x, main='Bonanza vs zero')
#barplot(count_s$freq, count_s$x)
count_p$x<-as.factor(count_p$x)
plot_p<-ggplot(count_p, aes(x, freq, fill=x))+
  geom_col()+
  labs(
    title=paste('# items found in 100x forage  with mean=', mean(items_found_100p))
  )
plot_p


