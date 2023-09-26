#######################################################
#    Concatenating HPC optimization output  PHASE 2   # 
######################################################

#  25/09/2023
#  Vera Vinken 

###############################################


# load packages 
library('parallel')
library('doParallel')
library(dplyr)


# Set the folder 
working_folder<- 'C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/HPC/12/12_environments/2023-09-24/phase2'

# Set the working directory 
setwd(working_folder)

# Load the filenames in this folder (there should be 1000*25)
filenames <- list.files(pattern="*.Rda", full.names=TRUE)
  
# make halflife list
halflife_list<-list()

# Set up for parallel   
numCores<-(detectCores()-1)
registerDoParallel(numCores)

# Run in parallel 
outcome_concat<- foreach(j=1:length(filenames), .combine='c') %dopar% {
    
    # load the current file 
    load(filenames[j])
    # extract the current halflife and put in list 
    halflife_list[1]<-env_results[1]
    halflife_list[[1]][3]<-env_results[[3]]$th_comb_input
    halflife_list[[1]][4]<-env_results[[3]]$total_num_per_th                #   change this once we're using the correct scripts !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    # to add this to teh 'outcome_concat' 
    halflife_list
  } # end for loop that runs through files in the working folder 
  
  # stop the cluster
  stopImplicitCluster()
  
  # Turn the list into a dataframe 
  halflife_df<-as.data.frame(do.call(rbind, outcome_concat))
  colnames(halflife_df)<-c('mean', 'sd','th_num', 'rep_num')
  
  # Turn into numeric 
  halflife_df$mean<-as.numeric(halflife_df$mean)
  halflife_df$sd<-as.numeric(halflife_df$sd)
  halflife_df$th_num<-as.numeric(halflife_df$th_num)
  halflife_df$rep_num<-as.numeric(halflife_df$rep_num)
  
  # Now I need to aggregate this dataframe by threshold combination 
  sum_HL_df<-halflife_df%>%
    group_by(th_num)%>%
    summarise(sum_HL=sum(mean))%>%
    arrange(desc(sum_HL))
  
  # Retrieve the best 
  opt_comb<-sum_HL_df[which.max(sum_HL_df$sum_HL),]
  # add to list 
  phase2_concat_outcome<-list(sum_HL_df, opt_comb)
  
  # save
  setwd(paste0(working_folder, '/concat_results'))
  # Save in the folder
  save(phase2_concat_outcome, file=paste('opt_outcome_concat_HPC_phase2', format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.Rda'))
  
  