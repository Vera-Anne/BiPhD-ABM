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

prob_bonanza<-0.5

  
  
  
  
  
forage_function<-function(mean_food, prob_bonanza){
  # First decide if you're going into bonanza chances or normal foraging 
  set.seed(1)
  cur_forage_type<-sample(c('forage-b', 'forage-n'), size=1, replace=TRUE, prob=c(prob_bonanza, (1-prob_bonanza)))
  
  if (cur_forage_type=='forage-b'){ 
    # Write code for bonanza here 
    
    # we know the mean 
    # set the size of your bonanza (should be input)
    # calculate the amount of times you need to find 0 
    # pull a number from that distribution 
  }
  
  else {
    # Write code for normal foraging here 
    # Can be the poisson distributuion 
    # pull a number from the poisson 
  }
  
  
  
  
  
  
  
  
  
} # end of the foraging function 