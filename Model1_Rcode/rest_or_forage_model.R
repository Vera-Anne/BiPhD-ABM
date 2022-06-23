#################################
# Small bird in winter - ABM 
# 20/06/2022
# Vera Vinken 
#################################

##    description     ##

# Simple model where birds live in an environment specified below 
# each timestep birds either forage or rest
# This decision is based on the stomach content (below or above threshold)
# foraging is rewarded with food, but costs energy
# resting as a lower energy expenditure 
# For each timestep the behaviours, variables and number of birds alive is registered 



##    Steps for next model      ## 

# Build in night and day, which includes sleeping behaviour 
# Make sure that the initial mass in the first timestep includes fr and sc 
# threshold for behaviour could depend on more things than stomach content 
# Check the distributions of initial values. Some of them are not normal distributuions 
# check how the bmr is applied, hardcoded number in there right now. 
# work temp in (into mbr; see point above) --> mass needs to be implemented here too 


##    adressed in this version  ## 

# In the previous version, birds kept doing behaviours whilst they should be dead 
# so i rewrote the killing part of the loop 
# also moved the transferring of variables to the next step to the individual loops. 
# this means that birds that are dead do not get any transer of values 
# also rectified the mistake in the transfer: mass was labeled as sc 


##############################
# initialize the population  #
##############################

# input parameters 
# can be put into a function later 
N<-100              # number of individuals 
T<-100              # number of timesteps 
temp<-(-5)          # Temperature 

food_item<-0.064    # value of a food item 
stom_size<-0.4      # stomach size of the bird 
th_forage_sc<-0.2  # threshold: if SC below this you forage 

stom_to_fat<-0.132  # variable that determines gram of sc goes to fat
fat_max<-4          # maximum fat reserve in gram 
num_food<-2         # number of food items found (this should be a distribution)

plot_interval<-5   # every x timestep a dot on the graph is added 


# create individual matrices 
mat_alive<-matrix(NA, N, (T))           # matrix to keep track of who's alive 
mat_sc<-matrix(NA, N, (T))              # matrix to keep track of stomach contents
mat_fr<-matrix(NA, N, (T))              # matrix to keep track of fat reserves 
mat_mass<-matrix(NA,N,(T))              # matrix to keep track of mass of birds 


# fill in some initial values for agent variables  
mass_init<-8+(rnorm(N, mean=0.2, sd=0.01))        # Gives initial mass from normal distribution
sc_init<-0+(runif(N, min=0, max=stom_size))       # gives initial stomach content from equal distribution
fr_init<-0+(runif(N, min=0, max=fat_max))         # gives initial fat reserves for random number between 0-4
alive_init<-rep(1, 100)                           # all birds are alive at the start 
# Put these in first column of the matrices  
mat_alive[,1]<-alive_init
mat_sc[,1]<-sc_init
mat_fr[,1]<-fr_init
mat_mass[,1]<-mass_init

# Create empty matrices to keep track of numbers
# keep track of means 
sc_mean<-matrix(NA, 1, (T))
fr_mean<-matrix(NA, 1, (T))
mass_mean<-matrix(NA,1, (T))
alive_mean<-matrix(NA,1, T)
# count what the birds are up to 
forage_count<-matrix(NA, N, (T))
rest_count<-matrix(NA, N, (T))
# total number of birds doing behaviours 
total_forage<-matrix(NA, 1, T)                  # total number of birds foraging each timestep
total_rest<-matrix(NA,1, T)                     # total number of birds resting each timestep 
total_alive<-matrix(NA,1,T)                     # total number of birds alive each timestep 

# Set up the food distribution 
# this doesnt work cause it gets negative values
# food_distr<-rnorm(100, mean=0.192, sd=0.4)

# Sort metabolic rates 
mr<-45.65-(1.33*temp)



###################################
#   start the for loop  timesteps # 
###################################

# Start a for loop for each timestep 
for (t in 1:T){
  
  
  
  ################################
  #      individual loops        # 
  ################################
  
  # now start a loop for every individual 
  for (i in (1:N)){
    
    # Check if individual is alive? 
    # in step 1 all birds are alive 
    if (t==1){
      mat_alive[i,t]==1
    }
    # if not step 1, check if bird was previously dead
    # if previously dead, it needs to be dead now 
    else if (mat_alive[i,(t-1)]==0){
        mat_alive[i,t]<-0
    }
    # if not step 1 and not previously dead 
    # check if the bird should die now 
    else if (mat_fr[i,t]==0){
      mat_alive[i,t]<-0
    }
    # in all other cases the bird is alive 
    else{
      mat_alive[i,t]<-1
    }
    
    
    ################
    #  DEAD BIRDS  #
    ################
    if(mat_alive[i,t]==0){
      # these are the dead birds 
      print(paste0(' bird ', i, 'is dead'))
    }else{
      
      #################
      #  ALIVE BIRDS  #
      #################
      
      # Check what behavior the bird should do 
      
      #################
      #     FORAGE    # 
      #################
      
      # CHECK IF FORAGING 
      if ((mat_sc[i,t]) < th_forage_sc){
        # SET COUNTING MATRICES 
        # In this case the bird should forage
        # set the forage to 1
        forage_count[i, t]<-1
        # set the resting matrix to 0
        rest_count[i,t]<-0
        
        # UPDATE AGENT OWNED VARIABLES 
        # Now, increase the stomach content
        mat_sc[i,(t)]<-(mat_sc[i,t])+(num_food*food_item)
        # now check if this doesnt exceed the stomach size 
        # if so, set the stomach content to stomach size 
        if (mat_sc[i,(t)]>stom_size){
          mat_sc[i,(t)]<-stom_size
        }
        # Set the BMR to the right level: cost of foraging
        BMR_multi<-8
      } # ends the foraging statement
      
      
      ##################
      #    RESTING     # 
      ##################
      
      # CHECK IF RESTING 
      # note that the threshold is coded in twice (both in forage and resting, could be an if-else)
      if ((mat_sc[i,t])>=th_forage_sc){
        # SET COUNTING MATRICES 
        # in the other case the bird should rest 
        # set the forage matrix to 0
        forage_count[i,t]<-0
        # set the rest matrix to 1
        rest_count[i,t]<-1
        
        # SET AGENT OWNED VARIABLES 
        # Set the BMR to the right level: cost of foraging
        BMR_multi<-1.95
        # the stomach content stays the same (initial value)
        
      } # end resting statement 
      
      ###################
      #    EVERYONE     # 
      ###################
      # No matter what behaviour you've done, these need updating 
      
      # UPDATE THE FAT RESERVES 
      # SC down and FR up 
      # first check if stomach has enough to actually move
      # move food out of stomach into fat 
      if (mat_sc[i,(t)]>= stom_to_fat){
        # new sc from resting/foraging can be used
        mat_sc[i,(t)]<-(mat_sc[i,(t)]-stom_to_fat)
        # the new fat reserve has not been determined yet
        mat_fr[i,(t)]<-(mat_fr[i,t]+stom_to_fat)
      }
      
      # ENERGY EXPENDITURE 
      # Set the fat reserves down depending on bmr-multi
      # first subtract the amount
      mat_fr[i,(t)]<-(mat_fr[i,t]-(0.0134*BMR_multi))
      # then make sure if this doesnt go below 0 
      if((mat_fr[i,(t)]<0)){
        mat_fr[i,(t)]<-0
      }
      # or above the maximum for fat-reserves 
      if((mat_fr[i,(t)]>fat_max)){
        mat_fr[i,(t)]<-fat_max
      }
      # check if the stomach content is above 0 
      if((mat_sc[i, (t)]<0)){
        mat_sc[i, (t)]<-0
      }
      # check if it is not above the stomach size either
      if((mat_sc[i,t]>stom_size)){
        mat_sc[i,t]<-stom_size
      }

      # SET MASS 
      # set the new mass for all individuals 
      mat_mass[i,t]<-((mass_init[i])+(mat_fr[i,t])+(mat_sc[i,t]))
      
      
      # MOVE ALL VARAIBLES TO T+1 
      # Note that this should only happen if youre not in the last timestep 
      if(t<T){
        # For the fr matrix 
        mat_fr[,(t+1)]<-mat_fr[,t]
        # For the mass matrix 
        mat_mass[,(t+1)]<-mat_mass[,t]
        # For the sc matrix 
        mat_sc[,(t+1)]<-mat_sc[,t]
      }
      
    } # end of loop for alive individuals 
    
  } # end of loop for each individual 
  
  ##########################
  #    wrap up timestep    # 
  ##########################
  
  
  # COUNT WHAT HAPPENED 
  # For each timestep, count what the birds are doing 
  total_forage[1,t]<-sum(forage_count[,t], na.rm=TRUE)  # counts how many birds foraged this timestep
  total_rest[1,t]<-sum(rest_count[,t], na.rm=TRUE)      # counts how many birds rested this timestep 
  total_alive[1,t]<-sum(mat_alive[,t], na.rm=TRUE)      # counts how many birds are alive this timestep
  
  # CALCULATE MEANS 
  sc_mean[t]<-mean(mat_sc[,t], na.rm = TRUE)        # adds mean stomach content for this timestep to matrix
  fr_mean[t]<-mean(mat_fr[,t], na.rm = TRUE)        # adds mean fat reserve for this timestep to matrix 
  mass_mean[t]<-mean(mat_mass[,t], na.rm = TRUE)    # adds mean mass for this timestep to mean-matrix
  alive_mean[t]<-mean(mat_alive[,t], na.rm= TRUE)
  
  # PLOT 
  # Make sure to plot every so often 
   if ((t/plot_interval)==floor(t/plot_interval)){
    Sys.sleep(0.05)          # forces an update to the plotting window 
    par(mfrow=c(3,2))
    # 1
    plot(1:t, sc_mean[1,(1:t)], ylim=c(0,(stom_size+0.1)), ylab='Mean stomach content', xlab='timestep', main='Mean Sc', type='l')
    abline(h=stom_size, col='red')
    # 2
    plot(1:t, fr_mean[1,(1:t)], ylim=c(0,(fat_max+0.5)), ylab='Mean fat reserve', xlab='timestep', main='Mean Fr', type='l')
    abline(h=fat_max, col='red')
    # 3
    plot(1:t, mass_mean[1,(1:t)], ylim=c(0,(20)), ylab='Mean mass', xlab='timestep', main='Mean mass', type='l')
    # 4
    plot(1:t, total_alive[1,(1:t)], ylim=c(0, N), ylab='Number of birds alive', xlab='Timestep', main='Number birds alive', type='l')
    # 5
    plot(1:t, total_forage[1,(1:t)], ylim=c(0, N), ylab='Number of birds foraging', xlab='Timestep', main='Number birds foraging', type='l')
    # 6
    plot(1:t, total_rest[1,(1:t)], ylim=c(0, N), ylab='Number of birds resting', xlab='Timestep', main='Nuber birds resting', type='l')
    Sys.sleep(0)             # turns that back off 
   }# end if statement 
  

  
  
} # end of big timestep loop 


### notes Tom 21/6/22
# loop 1: entire 24 hours
# loop per individual 
# loop 2: day part
# loop 3: neight part 
