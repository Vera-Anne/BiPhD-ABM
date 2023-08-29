###############################################
#    Concatenating HPC optimization output    # 
###############################################

#  25/08/2023
#  Vera Vinken 

###############################################


# load packages 



# set type of optimization 
opt_type<-131

# Set the folder in which the results are (this is the folder that contains the batches with results)
batch_folder<-'C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/HPC/131/2023-08-26/'




# IF statement : depenidng on opt-type this code differs 

if (opt_type==131){

    # If opt_type == 131 
    
      # navigate to folder where the 10 folders with the batches are (specified above)
      setwd(paste0(batch_folder))
      
      # Retrieve the names of the folders 
      batch_names<-list.dirs(full.names=TRUE, recursive = F)
      
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
            
            # For each of the files in the current batch: extract the halflife 
            for (j in 1:length(filenames)){
                # load the current file 
                load(filenames[j])
                # extract the current halflife and put in list 
                halflife_list[j]<-env_results[1]
                print(paste('batch',i ,'file', j))
              
            } # end for loop that runs through files in a batch-folder
            
            # Turn the list into a dataframe 
            halflife_df<-as.data.frame(do.call(rbind, halflife_list))
            
            # Add this halflife_list to the list that collects the halflifes per batch 
            halflife_per_batch_list[[i]]<-halflife_df
            
      } # end for loop through all batch-folder names 
      
      # The result should be a list that has (in the case of 131) 10 lists inside with the halflifes 
      
      # concatenate them all 
      HL_df<-as.data.frame(do.call(rbind, halflife_per_batch_list))
      
      # Now add the threshold values that were used for this to the dataframe 
      require(pracma)
      num_th<-50
      # set the minima
      min_th_1<-0
      min_th_2<-0
      min_th_3<-0
      # set the maxima
      max_th_1<-0.4
      max_th_2<-0.4
      max_th_3<-0.4
      # create the vectors
      th1_vec<-linspace(x1=min_th_1, x2=max_th_1, n=num_th)
      th2_vec<-linspace(x1=min_th_2, x2=max_th_2, n=num_th)
      th3_vec<-linspace(x1=min_th_3, x2=max_th_3, n=num_th)
      # Turn into dataframe 
      th1_th2_th3_comb<-as.data.frame(as.matrix(expand.grid(th1_vec, th2_vec, th3_vec)))
      
      
      # Add a column that checks if these are 'sensible' combinations 
      for (i in 1:nrow(th1_th2_th3_comb)){
        if (th1_th2_th3_comb$Var3[i] > th1_th2_th3_comb$Var2[i] && th1_th2_th3_comb$Var2[i] > th1_th2_th3_comb$Var1[i]){
          th1_th2_th3_comb$test[i]<-1
        } else{
          th1_th2_th3_comb$test[i]<-0
        }
      }
      
      # now select 
      sensible_combs<-subset(th1_th2_th3_comb, th1_th2_th3_comb$test>0)
      
      # Bind them together 
      HL_df<-cbind(HL_df, sensible_combs)
      
      # calculate the best HL across the different combinations 
      HL_best<-HL_df[(which.max(HL_df)),]
      
} else if (opt_type==132){
  
} else if (opt_type==231){
  
} else {
  print( 'define opt_type')
}
      
      