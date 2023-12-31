#######################################################
#    Concatenating HPC branching per level  output    # 
#######################################################

#  25/12/2023
#  Vera Vinken 

###############################################


# load packages 
library('parallel')
library('doParallel')
library(dplyr)
library(plotly)         # For the 3D scatterplot 
library(ggplot2)

# set opt_type
modelType=11



# Set the folder where the sperate files are 
file_folder<-"C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/HPC/branching/11/level_1/4branch_th_19571752"


# set wd 
setwd(paste0(file_folder))

# Load the filenames in this folder 
filenames <- list.files(pattern="*.Rda", full.names=TRUE)
  
# make halflife list
halflife_list<-list()

# set up parallel cores   
numCores<-(detectCores()-1)
registerDoParallel(numCores)
  
outcome_concat<- foreach(j=1:length(filenames)) %dopar% {
    # load the current file 
    load(filenames[j])
    # to add this to teh 'outcome_concat' 
    output<-as.data.frame(output)
    # Call output so it gets added to theoutcome file 
    output
  } # end for loop that runs through files in a batch-folder
  
  # stop the cluster
  stopImplicitCluster()
  
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
  # Now calculate the ranges for the next level 
  # Calculate the new ranges for level 2 (this uses the stepsize that comes out of the branching function in the previous level)
  
  # to retrieve the previous stepsize
  setwd(paste0(file_folder, "/start_th/"))
  # load the filename 
  filenames_th <- list.files( full.names=TRUE)
  previous_th<-read.table(filenames_th)
  # pull the previous stepsize 
  step_size<-previous_th[5]
  
  range_min_next<-(best_th-(step_size/2))
  range_max_next<-(best_th+(step_size/2))
  
  # range to be used for the next level 
  next_range<-cbind(range_min_next, range_max_next)
  
}



  
  
  
  
  
  
  
  
  # rename for rest of code
  HL_df<-halflife_df
  
  # Calculate the best value 
  HL_best<-HL_df[(which.max(HL_df$mean)),]
  
  # Turn to numeric 
  HL_df$th<-as.numeric(HL_df$th)
  HL_df$mean<-as.numeric(HL_df$mean)
  HL_df$th_num<-as.numeric(HL_df$th_num)
  
  # order in order of performance 
  HL_df<-HL_df %>%
    arrange(desc(mean))
  
  # save
  setwd(paste0(file_folder, '/concat_results'))
  # Save in the folder
  save(HL_df, file=paste('opt_outcome_concat_HPC_', opt_type,'_', format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.Rda'))
  
  # SAve the best 1000 threshold values in a vector 
  best_50<-head(HL_df, 50)
  best_50<-t(best_50[,4])
  # save it 
  write.table(best_50, file=paste('best_50_', opt_type, '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.csv'), row.names = F, col.names = F)
  
  # order the data 
  HL_df_ordered<-HL_df[order(HL_df$th_num),]
  
  print(paste(opt_type, " concatenation phase 1 has ended"))
  
} else if (opt_type==12 | opt_type==22 | opt_type==32){
      
      print(paste('start concatenation phase 1 for opt type = ', opt_type))
  
      # navigate to folder where the seperate files are
      setwd(paste0(file_folder))
      # Load the filenames in this folder 
      filenames <- list.files(pattern="*.Rda", full.names=TRUE)
      
      # make halflife list
      halflife_list<-list()
      
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
        halflife_list[[1]][5]<-env_results[[3]]$th_comb_input
        #print(paste('batch',i ,'file', j))
        # to add this to teh 'outcome_concat' 
        halflife_list
      } # end for loop that runs through files in a batch-folder
      
      # stop the cluster
      stopImplicitCluster()
      
      # Turn the list into a dataframe 
      halflife_df<-as.data.frame(do.call(rbind, outcome_concat))
      colnames(halflife_df)<-c('mean', 'sd', 'th1', 'th2', 'th_num')
      
      # rename for rest of code
      HL_df<-halflife_df
      
      # Calculate the best value 
      HL_best<-HL_df[(which.max(HL_df$mean)),]
      
      # Turn to numeric 
      HL_df$th1<-as.numeric(HL_df$th1)
      HL_df$th2<-as.numeric(HL_df$th2)
      HL_df$mean<-as.numeric(HL_df$mean)
      HL_df$th_num<-as.numeric(HL_df$th_num)
      
      # order in order of performance 
      HL_df<-HL_df %>%
        arrange(desc(mean))
      
      # save
      setwd(paste0(file_folder, '/concat_results'))
      # Save in the folder
      save(HL_df, file=paste('opt_outcome_concat_HPC_', opt_type,'_', format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.Rda'))
      
      # SAve the best 1000 threshold values in a vector 
      best_1000<-head(HL_df, 1000)
      best_1000<-t(best_1000[,5])
      # save it 
      write.table(best_1000, file=paste('best_1000_', opt_type, '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.csv'), row.names = F, col.names = F)
      
      # order the data 
      HL_df_ordered<-HL_df[order(HL_df$th_num),]
      
      # halflife
      # plot_ly(HL_df_ordered, x = ~th1, y = ~th2, z = ~th3, color = ~mean) %>%
      #   add_markers(size=~mean, marker=list(sizeref=0.02, sizemode='area')) %>%
      #   layout(scene = list(xaxis = list(range=c(0, 0.4),title = 'TH1'),
      #                       yaxis = list(range=c(0, 0.4),title = 'TH2'),
      #                       zaxis = list(range=c(0, 0.4),title = 'TH3')),
      #          title = list(text='1.3.1 HPC Mean Halflife - 3 thresholds ', y=0.95))  
      # 
      
      print(paste('end concatenation phase 1 opt type = ', opt_type))
  
} else if(opt_type==131 | opt_type==132 | opt_type==231 | opt_type==232 | opt_type==331 | opt_type==332){
    
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
  } else if (opt_type==41){
    
      print(paste('start concatenation phase 1 for opt type = ', opt_type))
      
      # navigate to folder where the seperate files are
      setwd(paste0(file_folder))
      # Load the filenames in this folder 
      filenames <- list.files(pattern="*.Rda", full.names=TRUE)
      
      # make halflife list
      halflife_list<-list()
      
      numCores<-(detectCores()-1)
      registerDoParallel(numCores)
      
      outcome_concat<- foreach(j=1:length(filenames), .combine='c') %dopar% {
        
        # For each of the files in the current batch: extract the halflife 
        # for (j in 1:length(filenames)){
        # load the current file 
        load(filenames[j])
        # extract the current halflife and put in list 
        halflife_list[1]<-env_results[1]
        halflife_list[[1]][3]<-env_results[[3]]$th11
        halflife_list[[1]][4]<-env_results[[3]]$th21
        halflife_list[[1]][5]<-env_results[[3]]$th_comb_input
        #print(paste('batch',i ,'file', j))
        # to add this to teh 'outcome_concat' 
        halflife_list
      } # end for loop that runs through files in a batch-folder
      
      # stop the cluster
      stopImplicitCluster()
      
      # Turn the list into a dataframe 
      halflife_df<-as.data.frame(do.call(rbind, outcome_concat))
      colnames(halflife_df)<-c('mean', 'sd', 'th11', 'th21', 'th_num')
      
      # rename for rest of code
      HL_df<-halflife_df
      
      # Calculate the best value 
      HL_best<-HL_df[(which.max(HL_df$mean)),]
      
      # Turn to numeric 
      HL_df$th1<-as.numeric(HL_df$th11)
      HL_df$th2<-as.numeric(HL_df$th21)
      HL_df$mean<-as.numeric(HL_df$mean)
      HL_df$th_num<-as.numeric(HL_df$th_num)
      
      # order in order of performance 
      HL_df<-HL_df %>%
        arrange(desc(mean))
      
      # save
      setwd(paste0(file_folder, '/concat_results'))
      # Save in the folder
      save(HL_df, file=paste('opt_outcome_concat_HPC_', opt_type,'_', format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.Rda'))
      
      # SAve the best 1000 threshold values in a vector 
      best_1000<-head(HL_df, 1000)
      best_1000<-t(best_1000[,5])
      # save it 
      write.table(best_1000, file=paste('best_1000_', opt_type, '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.csv'), row.names = F, col.names = F)
      
      # order the data 
      HL_df_ordered<-HL_df[order(HL_df$th_num),]
      
      print(paste('end concatenation phase 1 opt type = ', opt_type))
    
  
  
  } else {
  print(paste('mistake'))
}

