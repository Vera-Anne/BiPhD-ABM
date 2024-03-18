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
library(ggplot2)
library(pracma)# linspace 

# set opt type 
opt_type<-332

# Set the folder 
working_folder<-"C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/HPC/332/12_environments/2023-10-10/phase_2/"
  # 331: "C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/HPC/331/12_environments/2023-10-10/phase_2/"
  # 32: "C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/HPC/32/12_environments/2023-10-03/phase_2"
  # 232: "C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/HPC/231/12_environments/2023-10-04/phase_2"
  # 22: "C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/HPC/22/12_environments/2023-10-03/phase_2/"
  # 132: "C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/HPC/132/12_environments/2023-10-02/phase_2"
  # 21: "C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/HPC/21/12_environments/2023-10-03/phase_2"
  # 31: "C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/HPC/31/12_environments/2023-10-03/phase_2"
  # 131: "C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/HPC/131/12_environments/20-09-25/phase_2"
  # 12: 'C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/HPC/12/12_environments/2023-09-24/phase2'
  # 11: "C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/HPC/11/12_environments/2023-09-24/phase_2"
  # 12:  'C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/HPC/12/12_environments/2023-09-24/phase2'


# Set the working directory 
setwd(working_folder)

# Load the filenames in this folder (there should be 1000*25)
filenames <- list.files(pattern="*.Rda", full.names=TRUE)
  
# make halflife list
halflife_list<-list()



if (opt_type==11 | opt_type==21 | opt_type==31){
      print(paste('start phase 2 concat opt_type =', opt_type))
      # Set up for parallel   
      numCores<-(detectCores()-1)
      registerDoParallel(numCores)
      
      # Run in parallel 
      outcome_concat<- foreach(j=1:length(filenames), .combine='c') %dopar% {
          # load the current file 
          load(filenames[j])
          # extract the current halflife and put in list 
          halflife_list[1]<-env_results[1]
          halflife_list[[1]][3]<-env_results[[3]]$th_comb_input      # th num 
          halflife_list[[1]][4]<-env_results[[3]]$rep_num           # rep number 
          halflife_list[[1]][5]<-env_results[[3]]$total_num_th      # total number of options possible for a threshold (default = 50)
          halflife_list[[1]][6]<-env_results[[3]]$th                # threshold 
          # to add this to teh 'outcome_concat' 
          halflife_list
          
      } # end for loop that runs through files in the working folder 
        
        # stop the cluster
        stopImplicitCluster()
        # Turn the list into a dataframe 
        halflife_df<-as.data.frame(do.call(rbind, outcome_concat))
        colnames(halflife_df)<-c('mean', 'sd','th_num', 'rep_num', 'total_th_num', 'th')
        # Turn into numeric 
        halflife_df$mean<-as.numeric(halflife_df$mean)
        halflife_df$sd<-as.numeric(halflife_df$sd)
        halflife_df$th_num<-as.numeric(halflife_df$th_num)
        halflife_df$rep_num<-as.numeric(halflife_df$rep_num)
        halflife_df$total_th_num<-as.numeric(halflife_df$total_th_num)
        halflife_df$th<-as.numeric(halflife_df$th)
        # Now I need to aggregate this dataframe by threshold combination 
        sum_HL_df<-halflife_df%>%
          group_by(th_num)%>%
          summarise(sum_HL=sum(mean))%>%
          arrange(desc(sum_HL))
        # match the threshold number 
        sum_HL_df$th<-halflife_df$th[match(sum_HL_df$th_num, halflife_df$th_num)]
        # Calculate the mean survival in timesteps 
        sum_HL_df<-sum_HL_df%>%
          mutate(mean_HL=sum_HL/25)
        # Retrieve the best threshold combination 
        opt_comb<-sum_HL_df[which.max(sum_HL_df$sum_HL),]
        # add to list 
        phase2_concat_outcome<-list(sum_HL_df, opt_comb)
        # save
        setwd(paste0(working_folder, '/concat_results'))
        # Save in the folder
        save(phase2_concat_outcome, file=paste('opt_outcome_concat_HPC_phase2_',opt_type, '_', format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.Rda'))
        
        # Visualise
            # Filter for the maximum 
            highlight_df<-sum_HL_df%>%
              filter(mean_HL==max(mean_HL))
            # plot 
            ggplot_x1_opt<-ggplot(data=sum_HL_df, aes(x=th_num, y=mean_HL))+
              geom_point()+
              ggtitle(label='Survival in TS for model x.1 across 50 thresholds (mean over 25 reps)')+
              geom_point(data =highlight_df, aes(x=th_num, y=mean_HL), color='darkseagreen', size=3)
            ggplot_x1_opt  
        print(paste('End phase 2 concat opt_type =', opt_type))
} else if (opt_type==12 | opt_type ==22 | opt_type==32){
      
  print(paste('start phase 2 concat x.2 opt_type =', opt_type))
      # Set up for parallel   
      numCores<-(detectCores()-1)
      registerDoParallel(numCores)
      # Run in parallel 
      outcome_concat<- foreach(j=1:length(filenames), .combine='c') %dopar% {
        # load the current file 
        load(filenames[j])
        # extract the current halflife and put in list 
        halflife_list[1]<-env_results[1]
        halflife_list[[1]][3]<-env_results[[3]]$th_comb_input           # th num 
        halflife_list[[1]][4]<-env_results[[3]]$rep_num                 # rep number 
        halflife_list[[1]][5]<-env_results[[3]]$total_num_per_th        #  total number of options possible for a threshold (default = 50)
        halflife_list[[1]][6]<-env_results[[3]]$th1                     # threshold 1 
        halflife_list[[1]][7]<-env_results[[3]]$th2                     # threshold 2 
        # to add this to teh 'outcome_concat' 
        halflife_list
        #print(paste(j))
      } # end for loop that runs through files in the working folder 
      
      # stop the cluster
      stopImplicitCluster()
      
      # Turn the list into a dataframe 
      halflife_df<-as.data.frame(do.call(rbind, outcome_concat))
      colnames(halflife_df)<-c('mean', 'sd','th_num', 'rep_num', 'total_th_num', 'th1', 'th2')
      
      # Turn into numeric 
      halflife_df$mean<-as.numeric(halflife_df$mean)
      halflife_df$sd<-as.numeric(halflife_df$sd)
      halflife_df$th_num<-as.numeric(halflife_df$th_num)
      halflife_df$rep_num<-as.numeric(halflife_df$rep_num)
      halflife_df$total_th_num<-as.numeric(halflife_df$total_th_num)
      halflife_df$th1<-as.numeric(halflife_df$th1)
      halflife_df$th2<-as.numeric(halflife_df$th2)
      
      # Now I need to aggregate this dataframe by threshold combination 
      sum_HL_df<-halflife_df%>%
        group_by(th_num)%>%
        summarise(sum_HL=sum(mean))%>%
        arrange(desc(sum_HL))
      
      sum_HL_df$th1<-halflife_df$th1[match(sum_HL_df$th_num, halflife_df$th_num)]
      sum_HL_df$th2<-halflife_df$th2[match(sum_HL_df$th_num, halflife_df$th_num)]
      
      sum_HL_df<-sum_HL_df%>%
        mutate(mean_HL=sum_HL/25)
      
      # Retrieve the best 
      opt_comb<-sum_HL_df[which.max(sum_HL_df$sum_HL),]
      # add to list 
      phase2_concat_outcome<-list(sum_HL_df, opt_comb)
      
      # save
      setwd(paste0(working_folder, '/concat_results'))
      # Save in the folder
      save(phase2_concat_outcome, file=paste('opt_outcome_concat_HPC_phase2_',opt_type, '_', format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.Rda'))
      print(paste('End phase 2 concat x.2 opt_type =', opt_type))
      
} else if (opt_type ==131 | opt_type==132 | opt_type==231 | opt_type==232 | opt_type==331 | opt_type==332){
    print(paste('start phase 2 concat opt_type =', opt_type))
    # Set up for parallel   
    numCores<-(detectCores()-1)
    registerDoParallel(numCores)
  
  # Run in parallel 
  outcome_concat<- foreach(j=1:length(filenames), .combine='c') %dopar% {
    # load the current file 
    load(filenames[j])
    # extract the current halflife and put in list 
    halflife_list[1]<-env_results[1]
    halflife_list[[1]][3]<-env_results[[3]]$th_comb_input           # th num 
    halflife_list[[1]][4]<-env_results[[3]]$rep_num                 # rep number 
    halflife_list[[1]][5]<-env_results[[3]]$total_num_per_th        #  total number of options possible for a threshold (default = 50)
    halflife_list[[1]][6]<-env_results[[3]]$th1                     # threshold 1 
    halflife_list[[1]][7]<-env_results[[3]]$th2                     # threshold 2 
    halflife_list[[1]][8]<-env_results[[3]]$th3                     # threshold 3 
    
    # to add this to teh 'outcome_concat' 
    halflife_list
  
  } # end for loop that runs through files in the working folder 
  
      # stop the cluster
      stopImplicitCluster()
      
      # Turn the list into a dataframe 
      halflife_df<-as.data.frame(do.call(rbind, outcome_concat))
      colnames(halflife_df)<-c('mean', 'sd','th_num', 'rep_num', 'total_th_num', 'th1', 'th2', 'th3')
      
      # Turn into numeric 
      halflife_df$mean<-as.numeric(halflife_df$mean)
      halflife_df$sd<-as.numeric(halflife_df$sd)
      halflife_df$th_num<-as.numeric(halflife_df$th_num)
      halflife_df$rep_num<-as.numeric(halflife_df$rep_num)
      halflife_df$total_th_num<-as.numeric(halflife_df$total_th_num)
      halflife_df$th1<-as.numeric(halflife_df$th1)
      halflife_df$th2<-as.numeric(halflife_df$th2)
      halflife_df$th3<-as.numeric(halflife_df$th3)
      
      # Now I need to aggregate this dataframe by threshold combination 
      sum_HL_df<-halflife_df%>%
        group_by(th_num)%>%
        summarise(sum_HL=sum(mean))%>%
        arrange(desc(sum_HL))
      
      sum_HL_df$th1<-halflife_df$th1[match(sum_HL_df$th_num, halflife_df$th_num)]
      sum_HL_df$th2<-halflife_df$th2[match(sum_HL_df$th_num, halflife_df$th_num)]
      sum_HL_df$th3<-halflife_df$th3[match(sum_HL_df$th_num, halflife_df$th_num)]
      
      sum_HL_df<-sum_HL_df%>%
        mutate(mean_HL=sum_HL/25)
      
      # Retrieve the best 
      opt_comb<-sum_HL_df[which.max(sum_HL_df$sum_HL),]
      # add to list 
      phase2_concat_outcome<-list(sum_HL_df, opt_comb)
      
      # save
      setwd(paste0(working_folder, '/concat_results'))
      # Save in the folder
      save(phase2_concat_outcome, file=paste('opt_outcome_concat_HPC_phase2_',opt_type, '_', format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.Rda'))
      
      print(paste('end phase 2 concat opt_type =', opt_type))
} else{print('propblem with opt type ')}
  

  
  