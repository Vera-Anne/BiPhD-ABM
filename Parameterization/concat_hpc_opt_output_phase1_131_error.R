###############################################
#    Concatenating HPC optimization output    # 
###############################################

#  14/04/2024 
#  Vera Vinken 

###############################################

## BACKGROUND ###
# When i was making the graphs to compare the optimisation methods, I noticed that the values for model 131 phase 1 did not make sense. 
# I turnts out all the files that come from batch 3 -6 in phase 1 subset 1 model 131 have the same issue. 
# When I look at a single output file, in the 3rd dataframe of teh list (env_results), I see that the varaible names and their values are shifted. 
# When looking at the logbook of my HPC jobs, I can see that I made changes in the script soon after these jobs were ran. 
# The repetition number was added to the variables, but the names were not matching up, hence the shift. 
# When concatenating this all togehter for batches 3 , 4 5, and 6, the th1 value shows "50" which is the total number of values within a thrshold range. 
# I need to run the concatenations again, but now so that th1 actually grabs the value named th2. Th2 needs to grab the value named th3. 
# Th3 needs to grab the value named "min_th_sc1". That is what this script is for. 


# load packages 
library('parallel')
library('doParallel')
library(dplyr)
library(plotly)         # For the 3D scatterplot 
library(ggplot2)

# set opt_type
opt_type=131


# Set the folder in which the results are (this is the folder that contains the batches with results) - for .x.3.y models 
batch_folder<-'C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/HPC/131/12_environments/20-09-25/phase_1'

  
    print(paste('1.3 start phase 1 opt_type = ', opt_type))
    ############################
    # FOR X.3.X OPTIMIZATIONS  #
    ############################

      # navigate to folder where the 10 folders with the batches are (specified above)
      setwd(paste0(batch_folder))
      
      # Retrieve the names of the folders 
      batch_names<-list.dirs(full.names=TRUE, recursive = F)
      batch_names<-batch_names[grep("batch", batch_names, ignore.case=T)]
      
      # create list where the halflife lists from each batch can be stored
      halflife_per_batch_list<-list()
      
      # for each batch-folder in this list 
      for (i in 1:length(batch_names)){
        
            # set the current folder name 
            cur_batch<-batch_names[i]
        
            # set the wd to that folder 
            # setwd(paste0(batch_folder, cur_batch))
      
            # Load the filenames in this folder 
            filenames <- list.files(paste(cur_batch), pattern="*.Rda", full.names=TRUE)
            
            # make halflife list
            halflife_list<-list()
            
    ##################################################################################################
            
            # This is where the main changes are 
            
    ##################################################################################################
            if(i<=2 | i>=7){
            
            numCores<-(detectCores()-1)
            registerDoParallel(numCores)
            
            outcome_concat<- foreach(j=1:length(filenames), .combine='c') %dopar% {
            
            # For each of the files in the current batch: extract the halflife 
            # for (j in 1:length(filenames)){
                # load the current file 
                load(filenames[j])
                # extract the current halflife and put in list 
                halflife_list[1]<-env_results[1]
                halflife_list[[1]][3]<-env_results[[3]]$th1
                halflife_list[[1]][4]<-env_results[[3]]$th2
                halflife_list[[1]][5]<-env_results[[3]]$th3
                halflife_list[[1]][6]<-env_results[[3]]$th_comb_input
                #print(paste('batch',i ,'file', j))
                # to add this to teh 'outcome_concat' 
                halflife_list
            } # end for loop that runs through files in a batch-folder
            
            # stop the cluster
            stopImplicitCluster()
            # end of the good batches 
            
            }else if(i>=3 && i<=6){
              numCores<-(detectCores()-1)
              registerDoParallel(numCores)
              
              outcome_concat<- foreach(j=1:length(filenames), .combine='c') %dopar% {
                
                # For each of the files in the current batch: extract the halflife 
                # for (j in 1:length(filenames)){
                # load the current file 
                load(filenames[j])
                # extract the current halflife and put in list 
                halflife_list[1]<-env_results[1]
                halflife_list[[1]][3]<-env_results[[3]]$th2
                halflife_list[[1]][4]<-env_results[[3]]$th3
                halflife_list[[1]][5]<-env_results[[3]]$min_th_sc1
                halflife_list[[1]][6]<-env_results[[3]]$th_comb_input
                #print(paste('batch',i ,'file', j))
                # to add this to teh 'outcome_concat' 
                halflife_list
              } # end for loop that runs through files in a batch-folder
              
              # stop the cluster
              stopImplicitCluster()
            } # end of the faulty batches 
            
            # Turn the list into a dataframe 
            halflife_df<-as.data.frame(do.call(rbind, outcome_concat))
            colnames(halflife_df)<-c('mean', 'sd', 'th1', 'th2', 'th3', 'th_num')
            
            # Add this halflife_list to the list that collects the halflifes per batch 
            halflife_per_batch_list[[i]]<-halflife_df
            
            print(paste('batch done #', i))
            
      } # end for loop through all batch-folder names 
      
      # The result should be a list that has (in the case of 131) 10 lists inside with the halflifes 
      
      # concatenate them all 
      HL_df<-as.data.frame(do.call(rbind, halflife_per_batch_list))
      
      # Calculate the best value 
      HL_best<-HL_df[(which.max(HL_df$mean)),]
      

      HL_df$th1<-as.numeric(HL_df$th1)
      HL_df$th2<-as.numeric(HL_df$th2)
      HL_df$th3<-as.numeric(HL_df$th3)
      HL_df$mean<-as.numeric(HL_df$mean)
      HL_df$th_num<-as.numeric(HL_df$th_num)
      
      # save
      setwd(paste0(batch_folder, '/concat_results'))
      # Save in the folder
      save(HL_df, file=paste('opt_outcome_concat_HPC_', opt_type,'_', format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.Rda'))
      
      
      # order the data 
      HL_df_ordered<-HL_df[order(HL_df$th_num),]
      
      # halflife
      plot_ly(HL_df_ordered, x = ~th1, y = ~th2, z = ~th3, color = ~mean) %>%
        add_markers(size=~mean, marker=list(sizeref=0.02, sizemode='area')) %>%
        layout(scene = list(xaxis = list(range=c(0, 0.4),title = 'TH1'),
                            yaxis = list(range=c(0, 0.4),title = 'TH2'),
                            zaxis = list(range=c(0, 0.4),title = 'TH3')),
               title = list(text='1.3.x HPC Mean Halflife - 3 thresholds ', y=0.95))    
      
      HL_df_best_mean<-HL_df[order(HL_df$mean, decreasing=T),]
      
      # SAve the best 1000 threshold values in a vector 
      best_1000<-head(HL_df_best_mean, 1000)
      best_1000<-t(best_1000[,6])
      # save it 
      write.table(best_1000, file=paste('best_1000_', opt_type, '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.csv'), row.names = F, col.names = F)
  

