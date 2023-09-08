# Vera Vinken 
# 04/09/2023
# comparing HPC and local output for optimizations 

#################################################################################

# 1.3.1 
    # HPC results:  best HL 
    
      # set wd 
      setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/HPC/131/2023-08-26")
      load('opt_outcome_concat_HPC_ 131 .Rda')
      
      # find best HL 
      HL_best_hpc<-HL_df[(which.max(HL_df$mean)),]

    # LOCAL results: best HL 
      setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/Results_June23/MOD_1_3_1/3-Optimization/2023-06-22_start")
      load('2023-08-01_12_05_38opt_out131d30N1000dayh8num_th50.Rda')

      # take out the NA's 
      outcome_opt_df<-na.omit(outcome_opt_df)
      # add threshold number
      outcome_opt_df$th<-1:nrow(outcome_opt_df)
      # find best HL 
      HL_best_local<-outcome_opt_df[(which.max(outcome_opt_df$mean)),]
      
    
    # Now, I want to look at these specific runs and see how survival was over the specific environments 
      
      # For the hpc 
          setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/HPC/131/2023-08-26/09-batch")
          load("outcome_1_3_1_HPC_th 16302 .Rda")
          
          # Select the life-status information 
          alive_per_env<-lapply(env_results[[2]], subset, id=='alive')
          
          # Now do an overview image 
          # determine modeltype
          modelType<-131
          # subset data 
          survival_graph<-env_results[2]
          # loop through and create figures 
          for (i in 1:length(survival_graph[[1]])){
            if(i==1){
              plot_list<-list()
            }
            cur_df<-subset(survival_graph[[1]][[i]], survival_graph[[1]][[i]]$id=="alive")
            cur_plot<-ggplot(data=cur_df, aes(x=timestep, y=value))+
              geom_line()+
              ggtitle(label=paste('env=', i, ' MOD', modelType))+
              ylim(0,1)
            plot_list[[i]]<-cur_plot
          }
          
          # plot it 
          do.call(what = grid.arrange, args=c(plot_list, ncol=3))
          
      # Locally 
          # set wd
          setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/Results_June23/MOD_1_3_1/3-Optimization/2023-06-22_start/Environments")
          # load
          load("2023-07-11_02_54_36_env_func_out_131d30N1000th10.0163265306122449th20.0408163265306122th30.310204081632653dayh8.Rda")
      
          
          # Select the life-status information 
          alive_per_env<-lapply(output_env_func[[2]], subset, id=='alive')
          
          # Now do an overview image 
          # determine modeltype
          modelType<-131
          # subset data 
          survival_graph<-output_env_func[2]
          # loop through and create figures 
          for (i in 1:length(survival_graph[[1]])){
            if(i==1){
              plot_list<-list()
            }
            cur_df<-subset(survival_graph[[1]][[i]], survival_graph[[1]][[i]]$id=="alive")
            cur_plot<-ggplot(data=cur_df, aes(x=timestep, y=value))+
              geom_line()+
              ggtitle(label=paste('env=', i, ' MOD', modelType))+
              ylim(0,1)
            plot_list[[i]]<-cur_plot
          }
          
          # plot it 
          do.call(what = grid.arrange, args=c(plot_list, ncol=3))
          
  #############################################################################
  
  # 2.3.2 
  
          # HPC results:  best HL 
          
          # set wd 
          setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/HPC/131/2023-08-26")
          load('opt_outcome_concat_HPC_ 131 .Rda')
          
          # find best HL 
          HL_best_hpc<-HL_df[(which.max(HL_df$mean)),]
          
          # LOCAL results: best HL 
          setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/Results_June23/MOD_1_3_1/3-Optimization/2023-06-22_start")
          load('2023-08-01_12_05_38opt_out131d30N1000dayh8num_th50.Rda')
          
          # take out the NA's 
          outcome_opt_df<-na.omit(outcome_opt_df)
          # add threshold number
          outcome_opt_df$th<-1:nrow(outcome_opt_df)
          # find best HL 
          HL_best_local<-outcome_opt_df[(which.max(outcome_opt_df$mean)),]
          
          
          # Now, I want to look at these specific runs and see how survival was over the specific environments 
          
          # For the hpc 
          setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/HPC/131/2023-08-26/09-batch")
          load("outcome_1_3_1_HPC_th 16302 .Rda")
          
          # Select the life-status information 
          alive_per_env<-lapply(env_results[[2]], subset, id=='alive')
          
          # Now do an overview image 
          # determine modeltype
          modelType<-131
          # subset data 
          survival_graph<-env_results[2]
          # loop through and create figures 
          for (i in 1:length(survival_graph[[1]])){
            if(i==1){
              plot_list<-list()
            }
            cur_df<-subset(survival_graph[[1]][[i]], survival_graph[[1]][[i]]$id=="alive")
            cur_plot<-ggplot(data=cur_df, aes(x=timestep, y=value))+
              geom_line()+
              ggtitle(label=paste('env=', i, ' MOD', modelType))+
              ylim(0,1)
            plot_list[[i]]<-cur_plot
          }
          
          # plot it 
          do.call(what = grid.arrange, args=c(plot_list, ncol=3))
          
          # Locally 
          # set wd
          setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/Results_June23/MOD_1_3_1/3-Optimization/2023-06-22_start/Environments")
          # load
          load("2023-07-11_02_54_36_env_func_out_131d30N1000th10.0163265306122449th20.0408163265306122th30.310204081632653dayh8.Rda")
          
          
          # Select the life-status information 
          alive_per_env<-lapply(output_env_func[[2]], subset, id=='alive')
          
          # Now do an overview image 
          # determine modeltype
          modelType<-131
          # subset data 
          survival_graph<-output_env_func[2]
          # loop through and create figures 
          for (i in 1:length(survival_graph[[1]])){
            if(i==1){
              plot_list<-list()
            }
            cur_df<-subset(survival_graph[[1]][[i]], survival_graph[[1]][[i]]$id=="alive")
            cur_plot<-ggplot(data=cur_df, aes(x=timestep, y=value))+
              geom_line()+
              ggtitle(label=paste('env=', i, ' MOD', modelType))+
              ylim(0,1)
            plot_list[[i]]<-cur_plot
          }
          
          # plot it 
          do.call(what = grid.arrange, args=c(plot_list, ncol=3))
          
          