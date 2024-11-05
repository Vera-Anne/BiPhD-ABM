# 25/10/24
# Vera Vinken
# calculating the range of the base mass 


library(truncnorm)




for (i in 1:1000){
  

  mass_base<<-8+(rtruncnorm(1, a=0.01, b=0.2, mean=0.1, sd=0.01))
  if (i==1){
    range<-mass_base }
  else{
  
  range<-c(range, mass_base)}
  
}

range(range)


# stomach content range: 0-0.4
# Fat reserve range: 0-4 