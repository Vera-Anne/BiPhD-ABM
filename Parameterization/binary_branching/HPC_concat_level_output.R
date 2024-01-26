######################################################################################
# 30/12/2023
# concatenate output from the 100 simulations from a level 
# Vera Vinken 
# Last changes: 31/12/2023
######################################################################################

# Goal is to create a script that can run per level
# This should reduce memory requirement and hopefully incrase speed for running. 




##############################
#      load packages         #
##############################
library('parallel')
library('doParallel')
library(dplyr)



#############################
#  COMMAND LINE ARGUMENTS   #
#############################

args<-commandArgs(trailingOnly=TRUE)

# model type 
modelType<-as.numeric(args[1])

# Input direcotry 
in_dir<-args[2]

# Output directory 
out_dir<-args[3]

# level 
level<-as.numeric(args[4])

# previous stepsize 
previous_stepsize<-as.numeric(args[5])

####################################
#  Concatenate the output files   # 
###################################
# Set the folder where the sperate files are 
# set wd 
setwd(in_dir)

# Load the filenames in this folder 
filenames <- list.files(pattern="*.Rda", full.names=TRUE)
  
# make halflife list
halflife_list<-list()


outcome_concat<- foreach(j=1:length(filenames)) %do% {
    # load the current file 
    load(filenames[j])
    # to add this to teh 'outcome_concat' 
    output<-as.data.frame(output)
    # Call output so it gets added to theoutcome file 
    output
  } # end for loop that runs through files in a batch-folder
  

  
# Turn the list into a dataframe 
halflife_df<-as.data.frame(do.call(rbind, outcome_concat))

# calculate mean performance per threshold on average across the 25 repetitions 
if (modelType==11|modelType==21|modelType==31){
  hl_per_th<-halflife_df%>%
  group_by(cur_th)%>%
  summarise(mean_hl=mean(mean))%>%
  arrange(desc(mean_hl))
  best_th<-hl_per_th$cur_th[1]
  
  # Then determine the new range 
  # pull the previous stepsize 
  
  range_min_next<-(best_th-(previous_stepsize/2))
  range_max_next<-(best_th+(previous_stepsize/2))
  
  # range to be used for the next level 
  next_range<-cbind(range_min_next, range_max_next)
  
}

# set directory 
setwd(out_dir)
# make sure to attach the threshold to the dataframe 
#save(next_range, file=paste0('out_concat_level_mod_', modelType, '_level', level, "_", format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'.Rda'))


# make sure to attach the threshold to the dataframe 
write.table(next_range, sep=" ", file=paste0('out_concat_mod_', modelType, '_level', level, "_", format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'.csv'), row.names = F, col.names = F)
#write.csv(vals, file="test.csv")

  
  