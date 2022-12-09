# foraging function 



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
prob_for_b<-0.5
mean_food<-3
bonanza_size<-25
  
  
  
  
  
forage_function<-function(mean_food, prob_for_b, bonanza_size){
  # First decide if you're going into bonanza chances or normal foraging 
  cur_forage_type<-sample(c('forage-b', 'forage-n'), size=1, replace=TRUE, prob=c(prob_for_b, (1-prob_for_b)))
  
  if (cur_forage_type=='forage-b'){ 
    # Write code for bonanza here 
    
    # we know the mean 
    # set the size of your bonanza (should be input)
    # calculate the amount of times you need to find 0 
    # pull a number from that distribution 
    num_zero<-round(((bonanza_size-mean_food)/mean_food))
    item_options<-c(bonanza_size, (rep(0, num_zero)))
    food_item_found<-sample(item_options,1)
  }
  
  else {
    # Write code for normal foraging here 
    # Can be the poisson distributuion 
    # pull a number from the poisson 
  }
  
  
  
  
  
  
  
  
  
} # end of the foraging function 