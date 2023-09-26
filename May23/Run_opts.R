#################################
# Small bird in winter - ABM 
# Start date: 16/05/2023
# Vera Vinken 
# all models - for running optimisations 
#################################

##############################
#      load packages         #
##############################
# library(usethis)
# library(devtools)
library(truncnorm) 
library(pracma)         # For the linspace option. Selecting a collection of linearally spaces sequences 
library(ggplot2)
# library(plotly)       # for 3D surface plot 
# library(rgl)
library(plot3D)         # for the 3d surface plot (simple basic one) & the 4D one 
# library(htmlwidgets)
# library(webshot)
# library(withr)
library('plyr')
# library('gridExtra')
# library(grid)
# library(lattice)
library(dplyr)
library(data.table)    # reorganising data for plotting etc. 
# library(tidyverse)
# library(viridis)
library(foreach)
library(doParallel)
library(purrr)
library(beepr)
# library(tidyr)
library(reshape2)
library(hrbrthemes)     # for themes in ggplot 
#library(threejs)        # testing this for the 4d plot 
library(plotly)         # For the 3D scatterplot 


###########################
##     OPTIMIZATIONS     ##
###########################


system.time({
  
  # link to the function file 
  # This contains all the general, smaller funcitons needed for the models 
  setwd("C:/Local_R/BiPhD-ABM/May23")
  source('MOD_1_FuncSource.R')
  source('ModelSource.R')
  
  # SET PARAMETERS FOR OPTIMIZATION 
  
  # DAYLIGHT HOURS 
  daylight_h<-8
  # set days
  days<-30
  # set individuals
  N<-1000
  # Set the model type: 
  modelType<-41
  # number of threshold values for each 
  num_th<-50
  
  #############
  print(paste(modelType))
  
  
  # set up the values for which you want to optimise 
  # This needs to be different for the different models 
  if (modelType==11){
    
    # Set the number of thresholds you want to test for
    num_th<-num_th
    # The minimum 
    min_th_val<-0
    # And the maximum 
    max_th_val<-0.4
    # create the vector that has the actual threshold values in it 
    th_vec<-linspace( x1=min_th_val, x2=max_th_val, num_th)
    
    for (i in 1:length(th_vec)){
      if (i==1){
        list_1_1<-list()
      }
      
      cur_th<-th_vec[i]
      
      # print('debug here  11')
      
      env_func_1_1_par(days = days, N= N, th_forage_sc = cur_th, daylight_h = daylight_h, modelType=modelType)
      
      
      list_1_1[[length(list_1_1)+1]]<-output_env_func[[1]]
      
      
      print(paste('model 1.1 opt par-env threshold number =', i))
    }
    
    # put it in a dataframe 
    outcome_opt_df<-ldply(list_1_1, data.frame)
    # add the threshold combinations 
    outcome_opt_df$threshold<-th_vec
    
    # calculate the best 
    HL_best<-outcome_opt_df[(which.max(outcome_opt_df$mean)),]
    
    outcome_opt_df_11<<-outcome_opt_df
    
    # save the data 
    setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/MOD_1_1/12_environments/Optimization")
    save(outcome_opt_df_11, file=paste0(format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_opt_out', modelType, 'd', days, 'N', N, 'dayh', daylight_h, 'numTh', num_th,  '.Rda'))
    
    halflife_col<-"#69b3a2"
    
    # Make a plot 
    opt_plot<-ggplot(outcome_opt_df, aes(x=threshold))+
      geom_line(aes(y=mean), size=2, col=halflife_col)+
      geom_point(aes(x=HL_best$threshold, y=(HL_best$mean)), colour='#ffcc00', size=5)+
      #geom_ribbon(aes(y = mean, ymin=mean-SD, ymax=mean+SD), color = "#69b3a2", fill = alpha("#69b3a2", .5))+
      theme(
        axis.title.y = element_text(color = 'black', size=13, face='bold'),
        axis.title.x=element_text(size=13, face='bold')
      ) +
      ggtitle(paste('Mean timestep where halflife is reached per threshold, N=',N, ' days=',days, 'num_th=',num_th))
    opt_plot
    
    
    # Now, its time to run the different combinations in parallel 
    # end of if modeltype = 1.1 
  } else if (modelType==12){
    print('debug 12 here')
    # Set the number of options for which each trheshold needs to be tested 
    num_th<-num_th
    # set the minima 
    min_th_sc1<-0
    min_th_sc2<-0
    # set the maxima
    max_th_sc1<-0.4
    max_th_sc2<-0.4
    # create the vectors
    th1_vec<-linspace(x1=min_th_sc1, x2=max_th_sc1, n=num_th)
    th2_vec<-linspace(x1=min_th_sc2, x2=max_th_sc2, n=num_th)
    # create a matrix that contains all possible combinations 
    # var 1 = th 1
    # var 2 = th 2 
    th1_th2_comb<-as.matrix(expand.grid(th1_vec, th2_vec))
    
    # Now, make a for loop 
    
    for (i in 1:nrow(th1_th2_comb)){
      if (i==1){
        list_1_2<-list()
      }
      
      cur_th1<-th1_th2_comb[i,1]
      cur_th2<-th1_th2_comb[i,2]
      
      # but only do this in the case that th2 is actually larger than th 1 
      if (cur_th2>cur_th1){
        env_func_1_2_par(days = days, N= N, th_forage_sc1 = cur_th1, th_forage_sc2 = cur_th2, daylight_h = daylight_h, modelType=modelType)
        # put it in the list 
        list_1_2[[length(list_1_2)+1]]<-output_env_func[[1]]
        
      } else{
        # Fill the variables wiht 0
        # generate the average average HL
        mean<-0
        # and now for the sd
        SD<-0
        # do the same for the 
        output_env_func<-cbind(mean, SD)
        # put it in the list 
        list_1_2[[length(list_1_2)+1]]<-output_env_func
        
      }

      print(paste('model 1.2 opt par-env combination =', i))
    }
    
    
    # put it in a dataframe 
    outcome_opt_df<-ldply(list_1_2, data.frame)
    
    outcome_opt_df$threshold1<-th1_th2_comb[,1]
    outcome_opt_df$threshold2<-th1_th2_comb[,2]
    
    # calculate the best HL across the different combinations 
    HL_best<-outcome_opt_df[(which.max(outcome_opt_df$mean)),]
    
    outcome_opt_df_12<<-outcome_opt_df
    
    # save the data 
    setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/MOD_1_2/12_environments/Optimization")
    save(outcome_opt_df_12, file=paste0(format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_opt_out', modelType, 'd', days, 'N', N, 'dayh', daylight_h, 'numTh', num_th,  '.Rda'))
    
    # create a matrix with the values for ES
    #ES_matrix<-matrix(data=outcome_opt_df$mean_ES_cur_th, ncol=length(th2_vec))
    
    # create a matrix with the values for HL
    HL_matrix<-matrix(data=outcome_opt_df$mean, ncol=length(th2_vec))
    # plot it 
    HL_plot<-persp3D(z=HL_matrix, xlab='th_sc1', ylab='th_sc2', zlab='Timesteps at 50% alive', main='Optimal survival for th_sc1 and th_sc2 - Halflife') #, zlim= c(0, (days*72)))
    
    
  } else if (modelType==131){
    print('debug 131 here')
    #Set the number of options for which each trheshold needs to be tested
    num_th<-num_th
    # set the minima
    min_th_sc1<-0
    min_th_sc2<-0
    min_th_sc3<-0
    # set the maxima
    max_th_sc1<-0.4
    max_th_sc2<-0.4
    max_th_sc3<-0.4
    # create the vectors
    th1_vec<-linspace(x1=min_th_sc1, x2=max_th_sc1, n=num_th)
    th2_vec<-linspace(x1=min_th_sc2, x2=max_th_sc2, n=num_th)
    th3_vec<-linspace(x1=min_th_sc3, x2=max_th_sc3, n=num_th)
    # create a matrix that contains all possible combinations
    # var 1 = th 1
    # var 2 = th 2
    # var 3 = th 3
    th1_th2_th3_comb<-as.matrix(expand.grid(th1_vec, th2_vec, th3_vec))
    
    # Now, make a for loop 
    
    for (i in 1:nrow(th1_th2_th3_comb)){
      if (i==1){
        list_1_3_1<-list()
      }
      
      cur_th1<-th1_th2_th3_comb[i,1]
      cur_th2<-th1_th2_th3_comb[i,2]
      cur_th3<-th1_th2_th3_comb[i,3]
      
      # but only do this in the case that th2 is actually larger than th 1 
      if (cur_th2>cur_th1 && cur_th3>cur_th2){
        env_func_1_3_1_par(days = days, N= N, th_forage_sc1 = cur_th1, th_forage_sc2 = cur_th2, th_forage_sc3=cur_th3, daylight_h = daylight_h, modelType=modelType)
        # put it in the output list
        list_1_3_1[[length(list_1_3_1)+1]]<-output_env_func[[1]]
      } else{
        # Fill the variables wiht 0
        # generate the average average end-survival for this threshold, across all the environments 
        mean<-NA
        # and now for the average time till half life 
        SD<-NA
        # do the same for the th
        output_env_func<-cbind(mean, SD)
        # put in th elist 
        list_1_3_1[[length(list_1_3_1)+1]]<-output_env_func
        
      }
      
      
      
      print(paste('model 1.3.1 opt par-env combination =', i))
    }
    
    # put it in a dataframe 
    outcome_opt_df<-ldply(list_1_3_1, data.frame)
    
    outcome_opt_df$threshold1<-th1_th2_th3_comb[,1]
    outcome_opt_df$threshold2<-th1_th2_th3_comb[,2]
    outcome_opt_df$threshold3<-th1_th2_th3_comb[,3]
    
    # best ES (old code)
    #ES_best<-outcome_opt_df[(which.max(outcome_opt_df$mean_ES_cur_th)),]
    
    # best HL (new code)
    HL_best<-outcome_opt_df[(which.max(outcome_opt_df$mean)),]
    
    outcome_opt_df_131<<-outcome_opt_df 
    
    # save the data 
    setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/MOD_1_3_1/12_environments/Optimization")
    save(outcome_opt_df_131, file=paste0(format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'opt_out', modelType, 'd', days, 'N', N, 'dayh', daylight_h, 'num_th', num_th,  '.Rda'))
    
    
    # Change the dataframe so that 'NA' for both HL and ES are not plotted
    outcome_opt_df_plot<-subset(outcome_opt_df, (!is.na(outcome_opt_df[,1])) & (!is.na(outcome_opt_df[,2])))

    # halflife
    plot_ly(outcome_opt_df_plot, x = ~threshold1, y = ~threshold2, z = ~threshold3, color = ~mean) %>%
      add_markers(size=~mean, marker=list(sizeref=0.02, sizemode='area')) %>%
      layout(scene = list(xaxis = list(range=c(0, 0.4),title = 'TH1'),
                          yaxis = list(range=c(0, 0.4),title = 'TH2'),
                          zaxis = list(range=c(0, 0.4),title = 'TH3')),
             title = list(text='1.3.1 Mean Halflife - 3 thresholds ', y=0.95))
    
    
    print('131 opt done')
  } else if (modelType==132){
    print('debug 132 here')
    #Set the number of options for which each trheshold needs to be tested
    num_th<-num_th
    # set the minima
    min_th_sc1<-0
    min_th_sc2<-0
    min_th_sc3<-0
    # set the maxima
    max_th_sc1<-0.4
    max_th_sc2<-0.4
    max_th_sc3<-0.4
    # create the vectors
    th1_vec<-linspace(x1=min_th_sc1, x2=max_th_sc1, n=num_th)
    th2_vec<-linspace(x1=min_th_sc2, x2=max_th_sc2, n=num_th)
    th3_vec<-linspace(x1=min_th_sc3, x2=max_th_sc3, n=num_th)
    # create a matrix that contains all possible combinations
    # var 1 = th 1
    # var 2 = th 2
    # var 3 = th 3
    th1_th2_th3_comb<-as.matrix(expand.grid(th1_vec, th2_vec, th3_vec))
    
    # Now, make a for loop 
    
    for (i in 1:nrow(th1_th2_th3_comb)){
      if (i==1){
        list_1_3_2<-list()
      }
      
      cur_th1<-th1_th2_th3_comb[i,1]
      cur_th2<-th1_th2_th3_comb[i,2]
      cur_th3<-th1_th2_th3_comb[i,3]
      
      # but only do this in the case that th2 is actually larger than th 1 
      if (cur_th2>cur_th1 && cur_th3>cur_th2){
        env_func_1_3_2_par(days = days, N= N, th_forage_sc1 = cur_th1, th_forage_sc2 = cur_th2, th_forage_sc3=cur_th3, daylight_h = daylight_h, modelType=modelType)
        # put it in the output list
        list_1_3_2[[length(list_1_3_2)+1]]<-output_env_func[[1]]
      } else{
        # Fill the variables wiht 0
        # generate the average average end-survival for this threshold, across all the environments 
        mean<-NA
        # and now for the average time till half life 
        SD<-NA
        # do the same for the th
        output_env_func<-cbind(mean, SD)
        # put in th elist 
        list_1_3_2[[length(list_1_3_2)+1]]<-output_env_func
        
      }
      
      
      
      print(paste('model 1.3.2 opt par-env combination =', i))
    }
    
    # put it in a dataframe 
    outcome_opt_df<-ldply(list_1_3_2, data.frame)
    
    outcome_opt_df$threshold1<-th1_th2_th3_comb[,1]
    outcome_opt_df$threshold2<-th1_th2_th3_comb[,2]
    outcome_opt_df$threshold3<-th1_th2_th3_comb[,3]
    

    # best HL (new code)
    HL_best<-outcome_opt_df[(which.max(outcome_opt_df$mean)),]
    
    outcome_opt_df_132<<-outcome_opt_df 
    
    # save the data 
    setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/MOD_1_3_2/12_environments/Optimization")
    save(outcome_opt_df_132, file=paste0(format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'opt_out', modelType, 'd', days, 'N', N, 'dayh', daylight_h, 'num_th', num_th,  '.Rda'))
    
    
    # Change the dataframe so that 'NA' for both HL and ES are not plotted
    outcome_opt_df_plot<-subset(outcome_opt_df, (!is.na(outcome_opt_df[,1])) & (!is.na(outcome_opt_df[,2])))
    
    # halflife
    plot_ly(outcome_opt_df_plot, x = ~threshold1, y = ~threshold2, z = ~threshold3, color = ~mean) %>%
      add_markers(size=~mean, marker=list(sizeref=0.02, sizemode='area')) %>%
      layout(scene = list(xaxis = list(range=c(0, 0.4),title = 'TH1'),
                          yaxis = list(range=c(0, 0.4),title = 'TH2'),
                          zaxis = list(range=c(0, 0.4),title = 'TH3')),
             title = list(text='1.3.2 Mean Halflife - 3 thresholds ', y=0.95))
    
    
    print('132 opt done')
  } else if (modelType==21){
    
    print('debug 21 here')
    
    # Set the number of thresholds you want to test for
    num_th<-num_th
    # The minimum 
    min_th_val<-0
    # And the maximum 
    max_th_val<-4
    # create the vector that has the actual threshold values in it 
    th_vec<-linspace( x1=min_th_val, x2=max_th_val, num_th)
    # For loop to go through all the combinations 
    for (i in 1:length(th_vec)){
      if (i==1){
        list_2_1<-list()
      }
      # Set the current threshold 
      cur_th<-th_vec[i]
      # Run the function with this threshold 
      env_func_2_1_par(days = days, N= N, th_forage_fr = cur_th, daylight_h = daylight_h, modelType = modelType)
      # Add the output to the outcome list 
      list_2_1[[length(list_2_1)+1]]<-output_env_func[[1]]
      # To keep track when running 
      print(paste('model 2.1 opt par-env threshold number =', i))
    }
    
    # put it in a dataframe 
    outcome_opt_df<-ldply(list_2_1, data.frame)
    # Create a column with the threshold that goes with the outcome
    outcome_opt_df$threshold<-th_vec
    # best ES
    #ES_best<-outcome_opt_df[(which.max(outcome_opt_df$mean_ES_cur_th)),]
    # best HL
    HL_best<-outcome_opt_df[(which.max(outcome_opt_df$mean)),]
    
    outcome_opt_df_21<<-outcome_opt_df
    
    # save the data 
    setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/MOD_2_1/12_environments/Optimization")
    save(outcome_opt_df_21, file=paste0(format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_opt_out', modelType, 'd', days, 'N', N, 'dayh', daylight_h, 'numTh', num_th,  '.Rda'))
    
    # colour for the graph 
    halflife_col<-"#69b3a2"
    
    # Make a plot 
    opt_plot<-ggplot(outcome_opt_df, aes(x=threshold))+
      geom_line(aes(y=mean), size=2, col=halflife_col)+
      geom_point(aes(x=HL_best$threshold, y=(HL_best$mean)), colour='#ffcc00', size=5)+
      #geom_ribbon(aes(y = mean, ymin=mean-SD, ymax=mean+SD), color = "#69b3a2", fill = alpha("#69b3a2", .5))+
      theme(
        axis.title.y = element_text(color = 'black', size=13, face='bold'),
        axis.title.x=element_text(size=13, face='bold')
      ) +
      ggtitle(paste('Mean timestep where halflife is reached per threshold, N=',N, ' days=',days, 'num_th=',num_th))
    opt_plot
    
    
  } else if (modelType==22){
    print('debug 22 here')
    # Set the number of options for which each trheshold needs to be tested 
    num_th<-num_th
    # set the minima 
    min_th_fr1<-0
    min_th_fr2<-0
    # set the maxima
    max_th_fr1<-4
    max_th_fr2<-4
    # create the vectors
    th1_vec<-linspace(x1=min_th_fr1, x2=max_th_fr1, n=num_th)
    th2_vec<-linspace(x1=min_th_fr2, x2=max_th_fr2, n=num_th)
    # create a matrix that contains all possible combinations 
    # var 1 = th 1
    # var 2 = th 2 
    th1_th2_comb<-as.matrix(expand.grid(th1_vec, th2_vec))
    
    # Now, make a for loop 
    
    for (i in 1:nrow(th1_th2_comb)){
      if (i==1){
        list_2_2<-list()
      }
      
      cur_th1<-th1_th2_comb[i,1]
      cur_th2<-th1_th2_comb[i,2]
      
      # but only do this in the case that th2 is actually larger than th 1 
      if (cur_th2>cur_th1){
        env_func_2_2_par(days = days, N= N, th_forage_fr1 = cur_th1, th_forage_fr2 = cur_th2, daylight_h = daylight_h, modelType=modelType)
        # Add the calculated mean and SD to the list 
        list_2_2[[length(list_2_2)+1]]<-output_env_func[[1]]
        
      } else{
        # Fill the variables wiht 0
        # generate the average average HL
        mean<-0
        # and now for the sd
        SD<-0
        # do the same for the 
        output_env_func<-cbind(mean, SD)
        # put it in the list 
        list_2_2[[length(list_2_2)+1]]<-output_env_func
        
      }
      
      print(paste('model 2.2 opt par-env combination =', i))
    }

    # put it in a dataframe 
    outcome_opt_df<-ldply(list_2_2, data.frame)
    
    outcome_opt_df$threshold1<-th1_th2_comb[,1]
    outcome_opt_df$threshold2<-th1_th2_comb[,2]

    
    # best HL
    HL_best<-outcome_opt_df[(which.max(outcome_opt_df$mean)),]
    
    outcome_opt_df_22<<-outcome_opt_df
    
    # save the data 
    setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/MOD_2_2/12_environments/Optimization")
    save(outcome_opt_df_22, file=paste0(format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_opt_out', modelType, 'd', days, 'N', N, 'dayh', daylight_h, 'numTh', num_th,  '.Rda'))
    
    # create a matrix with the values for ES
    #ES_matrix<-matrix(data=outcome_opt_df$mean_ES_cur_th, ncol=length(th2_vec))
    # create a matrix with the values for HL
    HL_matrix<-matrix(data=outcome_opt_df$mean, ncol=length(th2_vec))
    # plot it 
    HL_plot<-persp3D(z=HL_matrix, xlab='th_fr1', ylab='th_fr2', zlab='Timesteps at 50% alive', main='Optimal survival for th_fr1 and th_fr2 - Halflife') #, zlim= c(0, (days*72)))

  } else if(modelType==231) {
    
    #Set the number of options for which each trheshold needs to be tested
    num_th<-num_th
    # set the minima
    min_th_fr1<-0
    min_th_fr2<-0
    min_th_fr3<-0
    # set the maxima
    max_th_fr1<-4
    max_th_fr2<-4
    max_th_fr3<-4
    # create the vectors
    th1_vec<-linspace(x1=min_th_fr1, x2=max_th_fr1, n=num_th)
    th2_vec<-linspace(x1=min_th_fr2, x2=max_th_fr2, n=num_th)
    th3_vec<-linspace(x1=min_th_fr3, x2=max_th_fr3, n=num_th)
    # create a matrix that contains all possible combinations
    # var 1 = th 1
    # var 2 = th 2
    # var 3 = th 3
    th1_th2_th3_comb<-as.matrix(expand.grid(th1_vec, th2_vec, th3_vec))
    
    # Now, make a for loop 
    
    for (i in 1:nrow(th1_th2_th3_comb)){
      if (i==1){
        list_2_3_1<-list()
      }
      
      cur_th1<-th1_th2_th3_comb[i,1]
      cur_th2<-th1_th2_th3_comb[i,2]
      cur_th3<-th1_th2_th3_comb[i,3]
      
      # but only do this in the case that th2 is actually larger than th 1 
      if (cur_th2>cur_th1 && cur_th3>cur_th2){
        env_func_2_3_1_par(days = days, N= N, th_forage_fr1 = cur_th1, th_forage_fr2 = cur_th2, th_forage_fr3=cur_th3, daylight_h = daylight_h, modelType=modelType)
        # put it in the output list
        list_2_3_1[[length(list_2_3_1)+1]]<-output_env_func[[1]]
      } else{
        # Fill the variables wiht 0
        # generate the average average end-survival for this threshold, across all the environments 
        mean<-NA
        # and now for the average time till half life 
        SD<-NA
        # do the same for the th
        output_env_func<-cbind(mean, SD)
        # put in th elist 
        list_2_3_1[[length(list_2_3_1)+1]]<-output_env_func
        
      }
      
      print(paste('model 2.3.1 opt par-env combination =', i))
    }
    
    # put it in a dataframe 
    outcome_opt_df<-ldply(list_2_3_1, data.frame)
    # with the threshold combinations attached 
    outcome_opt_df$threshold1<-th1_th2_th3_comb[,1]
    outcome_opt_df$threshold2<-th1_th2_th3_comb[,2]
    outcome_opt_df$threshold3<-th1_th2_th3_comb[,3]
    
    
    # best HL (new code)
    HL_best<-outcome_opt_df[(which.max(outcome_opt_df$mean)),]
    
    outcome_opt_df_231<<-outcome_opt_df
    
    # save the data 
    setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/MOD_2_3_1/12_environments/Optimization")
    save(outcome_opt_df_231, file=paste0(format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'opt_out', modelType, 'd', days, 'N', N, 'dayh', daylight_h, 'num_th', num_th,  '.Rda'))
    
    
    # Change the dataframe so that 'NA' for both HL and ES are not plotted
    outcome_opt_df_plot<-subset(outcome_opt_df, (!is.na(outcome_opt_df[,1])) & (!is.na(outcome_opt_df[,2])))
    
    
    # halflife
    plot_ly(outcome_opt_df_plot, x = ~threshold1, y = ~threshold2, z = ~threshold3, color = ~mean) %>%
      add_markers(size=~mean, marker=list(sizeref=0.02, sizemode='diameter')) %>%
      layout(scene = list(xaxis = list(range=c(0, 4),title = 'TH1'),
                          yaxis = list(range=c(0, 4),title = 'TH2'),
                          zaxis = list(range=c(0, 4),title = 'TH3')),
             title = list(text='2.3.1 Mean Halflife - 3 thresholds ', y=0.95))
    
    
    print('231 opt done')
    
  } else if(modelType==232) {

    print('debug 232 here')
    #Set the number of options for which each trheshold needs to be tested
    num_th<-num_th
    # set the minima
    min_th_fr1<-0
    min_th_fr2<-0
    min_th_fr3<-0
    # set the maxima (for fat-reserves, so set this to 4)
    max_th_fr1<-4
    max_th_fr2<-4
    max_th_fr3<-4
    # create the vectors
    th1_vec<-linspace(x1=min_th_fr1, x2=max_th_fr1, n=num_th)
    th2_vec<-linspace(x1=min_th_fr2, x2=max_th_fr2, n=num_th)
    th3_vec<-linspace(x1=min_th_fr3, x2=max_th_fr3, n=num_th)
    # create a matrix that contains all possible combinations
    # var 1 = th 1
    # var 2 = th 2
    # var 3 = th 3
    th1_th2_th3_comb<-as.matrix(expand.grid(th1_vec, th2_vec, th3_vec))
    
    # Now, make a for loop 
    
    for (i in 1:nrow(th1_th2_th3_comb)){
      if (i==1){
        list_2_3_2<-list()
      }
      
      cur_th1<-th1_th2_th3_comb[i,1]
      cur_th2<-th1_th2_th3_comb[i,2]
      cur_th3<-th1_th2_th3_comb[i,3]
      
      # but only do this in the case that th2 is actually larger than th 1 
      if (cur_th2>cur_th1 && cur_th3>cur_th2){
        env_func_2_3_2_par(days = days, N= N, th_forage_fr1 = cur_th1, th_forage_fr2 = cur_th2, th_forage_fr3=cur_th3, daylight_h = daylight_h, modelType=modelType)
        # put it in the output list
        list_2_3_2[[length(list_2_3_2)+1]]<-output_env_func[[1]]
      } else{
        # Fill the variables wiht 0
        # generate the average average end-survival for this threshold, across all the environments 
        mean<-NA
        # and now for the average time till half life 
        SD<-NA
        # do the same for the th
        output_env_func<-cbind(mean, SD)
        # put in th elist 
        list_2_3_2[[length(list_2_3_2)+1]]<-output_env_func
        
      }
      
      
      
      print(paste('model 2.3.2 opt par-env combination =', i))
    }
    
    # put it in a dataframe 
    outcome_opt_df<-ldply(list_2_3_2, data.frame)
    
    outcome_opt_df$threshold1<-th1_th2_th3_comb[,1]
    outcome_opt_df$threshold2<-th1_th2_th3_comb[,2]
    outcome_opt_df$threshold3<-th1_th2_th3_comb[,3]
    
    
    # best HL (new code)
    HL_best<-outcome_opt_df[(which.max(outcome_opt_df$mean)),]
    
    outcome_opt_df_232<<-outcome_opt_df 
    
    # save the data 
    setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/MOD_2_3_2/12_environments/Optimization")
    save(outcome_opt_df_232, file=paste0(format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'opt_out', modelType, 'd', days, 'N', N, 'dayh', daylight_h, 'num_th', num_th,  '.Rda'))
    
    
    # Change the dataframe so that 'NA' for both HL and ES are not plotted
    outcome_opt_df_plot<-subset(outcome_opt_df, (!is.na(outcome_opt_df[,1])) & (!is.na(outcome_opt_df[,2])))
    
    # halflife
    plot_ly(outcome_opt_df_plot, x = ~threshold1, y = ~threshold2, z = ~threshold3, color = ~mean) %>%
      add_markers(size=~mean, marker=list(sizeref=0.02, sizemode='area')) %>%
      layout(scene = list(xaxis = list(range=c(0, 4),title = 'TH1'),
                          yaxis = list(range=c(0, 4),title = 'TH2'),
                          zaxis = list(range=c(0, 4),title = 'TH3')),
             title = list(text='2.3.2 Mean Halflife - 3 thresholds ', y=0.95))
    
    
    print('232 opt done')
  } else if(modelType=='31'){
      # Set the number of thresholds you want to test for
      num_th<-num_th
      # The minimum 
      min_th_val<-(-0.6)
      # And the maximum 
      max_th_val<-0.6
      # create the vector that has the actual threshold values in it 
      th_vec<-linspace( x1=min_th_val, x2=max_th_val, num_th)
      
      for (i in 1:length(th_vec)){
        if (i==1){
          list_3_1<-list()
        }
        
        cur_th<-th_vec[i]
        
        # run the parallel enviornment option here 
        env_func_3_1_par(days = days, N= N, th_forage_flr = cur_th, daylight_h = daylight_h, modelType=modelType)
        
        # Add to the list 
        list_3_1[[length(list_3_1)+1]]<-output_env_func[[1]]
        
        print(paste('model 3.1 opt par-env threshold number =', i))
      }
      
      # put it in a dataframe 
      outcome_opt_df<-ldply(list_3_1, data.frame)
      # add the threshold combinations 
      outcome_opt_df$threshold<-th_vec
      
      # calculate the best 
      HL_best<-outcome_opt_df[(which.max(outcome_opt_df$mean)),]
      
      outcome_opt_df_31<<-outcome_opt_df
      # save the data 
      setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/MOD_3_1/12_environments/Optimization")
      save(outcome_opt_df_31 , file=paste0(format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_opt_out', modelType, 'd', days, 'N', N, 'dayh', daylight_h, 'numTh', num_th,  '.Rda'))
      
      # Put the colour 
      halflife_col<-"#69b3a2"
      
      # Make a plot 
      opt_plot<-ggplot(outcome_opt_df, aes(x=threshold))+
        geom_line(aes(y=mean), size=2, col=halflife_col)+
        geom_point(aes(x=HL_best$threshold, y=(HL_best$mean)), colour='#ffcc00', size=5)+
        #geom_ribbon(aes(y = mean, ymin=mean-SD, ymax=mean+SD), color = "#69b3a2", fill = alpha("#69b3a2", .5))+
        theme(
          axis.title.y = element_text(color = 'black', size=13, face='bold'),
          axis.title.x=element_text(size=13, face='bold')
        ) +
        ggtitle(paste('Mean TS where halflife is reached per th MOD 3.1, N=',N, ' days=',days, 'num_th=',num_th))
      opt_plot
      
  }  else if(modelType=='32'){
        
        # print('debug 32 here')
        # Set the number of options for which each trheshold needs to be tested 
        num_th<-num_th
        # set the minima 
        min_th_flr1<-(-0.6)
        min_th_flr2<-(-0.6)
        # set the maxima
        max_th_flr1<-0.6
        max_th_flr2<-0.6
        # create the vectors
        th1_vec<-linspace(x1=min_th_flr1, x2=max_th_flr1, n=num_th)
        th2_vec<-linspace(x1=min_th_flr2, x2=max_th_flr2, n=num_th)
        # create a matrix that contains all possible combinations 
        # var 1 = th 1
        # var 2 = th 2 
        th1_th2_comb<-as.matrix(expand.grid(th1_vec, th2_vec))
        
        # Now, make a for loop 
        
        for (i in 1:nrow(th1_th2_comb)){
          if (i==1){
            list_3_2<-list()
          }
          
          cur_th1<-th1_th2_comb[i,1]
          cur_th2<-th1_th2_comb[i,2]
          
          # but only do this in the case that th2 is actually larger than th 1 
          if (cur_th2>cur_th1){
            env_func_3_2_par(days = days, N= N, th_forage_flr1 = cur_th1, th_forage_flr2 = cur_th2, daylight_h = daylight_h, modelType=modelType)
            # Add the calculated mean and SD to the list 
            list_3_2[[length(list_3_2)+1]]<-output_env_func[[1]]
            
          } else{
            # Fill the variables wiht 0
            # generate the average average HL
            mean<-0
            # and now for the sd
            SD<-0
            # do the same for the 
            output_env_func<-cbind(mean, SD)
            # put it in the list 
            list_3_2[[length(list_3_2)+1]]<-output_env_func
            
          }
          
          print(paste('model 3.2 opt par-env combination =', i))
        }
        
        # put it in a dataframe 
        outcome_opt_df<-ldply(list_3_2, data.frame)
        
        outcome_opt_df$threshold1<-th1_th2_comb[,1]
        outcome_opt_df$threshold2<-th1_th2_comb[,2]
        
        # best HL
        HL_best<-outcome_opt_df[(which.max(outcome_opt_df$mean)),]
        
        outcome_opt_df_32<<-outcome_opt_df
        
        # save the data 
        setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/MOD_3_2/12_environments/Optimization")
        save(outcome_opt_df_32, file=paste0(format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_opt_out', modelType, 'd', days, 'N', N, 'dayh', daylight_h, 'numTh', num_th,  '.Rda'))
        
        # create a matrix with the values for HL
        HL_matrix<-matrix(data=outcome_opt_df$mean, ncol=length(th2_vec))
        
        HL_plot<-persp3D(z=HL_matrix, xlab='th_flr1', ylab='th_flr2', zlab='Timesteps at 50% alive', main='Optimal survival for th_flr1 and th_flr2 - Halflife') #, zlim= c(0, (days*72)))
        
        # mark that opt is done 
        beep()
        print('run opt 3.2 is done')
        
  } else if(modelType=='331'){
    
        #Set the number of options for which each trheshold needs to be tested
        num_th<-num_th
        # set the minima
        min_th_fr1<-(-0.6)
        min_th_fr2<-(-0.6)
        min_th_fr3<-(-0.6)
        # set the maxima
        max_th_fr1<-0.6
        max_th_fr2<-0.6
        max_th_fr3<-0.6
        # create the vectors
        th1_vec<-linspace(x1=min_th_flr1, x2=max_th_flr1, n=num_th)
        th2_vec<-linspace(x1=min_th_flr2, x2=max_th_flr2, n=num_th)
        th3_vec<-linspace(x1=min_th_flr3, x2=max_th_flr3, n=num_th)
        # create a matrix that contains all possible combinations
        # var 1 = th 1
        # var 2 = th 2
        # var 3 = th 3
        th1_th2_th3_comb<-as.matrix(expand.grid(th1_vec, th2_vec, th3_vec))
        
        # Now, make a for loop 
        
        for (i in 1:nrow(th1_th2_th3_comb)){
          if (i==1){
            list_3_3_1<-list()
          }
          
          cur_th1<-th1_th2_th3_comb[i,1]
          cur_th2<-th1_th2_th3_comb[i,2]
          cur_th3<-th1_th2_th3_comb[i,3]
          
          # but only do this in the case that th2 is actually larger than th 1 
          if (cur_th2>cur_th1 && cur_th3>cur_th2){
            # run environment function 
            env_func_3_3_1_par(days = days, N= N, th_forage_flr1 = cur_th1, th_forage_flr2 = cur_th2, th_forage_flr3=cur_th3, daylight_h = daylight_h, modelType=modelType)
            # put it in the output list
            list_3_3_1[[length(list_3_3_1)+1]]<-output_env_func[[1]]
          } else{
            # Fill the variables wiht 0
            # generate the average average end-survival for this threshold, across all the environments 
            mean<-NA
            # and now for the average time till half life 
            SD<-NA
            # do the same for the th
            output_env_func<-cbind(mean, SD)
            # put in th elist 
            list_3_3_1[[length(list_3_3_1)+1]]<-output_env_func
            
          }
          
          print(paste('model 3.3.1 opt par-env combination =', i))
        }
        
        # put it in a dataframe 
        outcome_opt_df<-ldply(list_3_3_1, data.frame)
        # with the threshold combinations attached 
        outcome_opt_df$threshold1<-th1_th2_th3_comb[,1]
        outcome_opt_df$threshold2<-th1_th2_th3_comb[,2]
        outcome_opt_df$threshold3<-th1_th2_th3_comb[,3]
        
        
        # best HL (new code)
        HL_best<-outcome_opt_df[(which.max(outcome_opt_df$mean)),]
        
        outcome_opt_df_331<<-outcome_opt_df
        
        # save the data 
        setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/MOD_3_3_1/12_environments/Optimization")
        save(outcome_opt_df_331, file=paste0(format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'opt_out', modelType, 'd', days, 'N', N, 'dayh', daylight_h, 'num_th', num_th,  '.Rda'))
        
        
        # Change the dataframe so that 'NA' for both HL and ES are not plotted
        outcome_opt_df_plot<-subset(outcome_opt_df, (!is.na(outcome_opt_df[,1])) & (!is.na(outcome_opt_df[,2])))
        
        
        # halflife
        plot_ly(outcome_opt_df_plot, x = ~threshold1, y = ~threshold2, z = ~threshold3, color = ~mean) %>%
          add_markers(size=~mean, marker=list(sizeref=0.02, sizemode='diameter')) %>%
          layout(scene = list(xaxis = list(range=c(-0.6, 0.6),title = 'TH1'),
                              yaxis = list(range=c(-0.6, 0.6),title = 'TH2'),
                              zaxis = list(range=c(-0.6, 0.6),title = 'TH3')),
                 title = list(text='3.3.1 Mean Halflife - 3 thresholds ', y=0.95))
        
        # mark end of simulation 
        beep()
        print('331 opt done')
        
  }else if(modelType=='332'){
    #Set the number of options for which each threshold needs to be tested
    num_th<-num_th
    
    # set the minima
    min_th_fr1<-(-0.6)
    min_th_fr2<-(-0.6)
    min_th_fr3<-(-0.6)
    
    # set the maxima
    max_th_fr1<-0.6
    max_th_fr2<-0.6
    max_th_fr3<-0.6
    
    # create the vectors
    th1_vec<-linspace(x1=min_th_fr1, x2=max_th_fr1, n=num_th)
    th2_vec<-linspace(x1=min_th_fr2, x2=max_th_fr2, n=num_th)
    th3_vec<-linspace(x1=min_th_fr3, x2=max_th_fr3, n=num_th)
    
    # create a matrix that contains all possible combinations
    # var 1 = th 1
    # var 2 = th 2
    # var 3 = th 3
    th1_th2_th3_comb<-as.matrix(expand.grid(th1_vec, th2_vec, th3_vec))
    
    # Now, make a for loop 
    
    for (i in 1:nrow(th1_th2_th3_comb)){
      if (i==1){
        list_3_3_2<-list()
      }
      
      cur_th1<-th1_th2_th3_comb[i,1]
      cur_th2<-th1_th2_th3_comb[i,2]
      cur_th3<-th1_th2_th3_comb[i,3]
      
      # but only do this in the case that th2 is actually larger than th 1 
      if (cur_th2>cur_th1 && cur_th3>cur_th2){
        # Run the environment function 
        env_func_3_3_2_par(days = days, N= N, th_forage_flr1 = cur_th1, th_forage_flr2 = cur_th2, th_forage_flr3=cur_th3, daylight_h = daylight_h, modelType=modelType)
        # add to the list 
        list_3_3_2[[length(list_3_3_2)+1]]<-output_env_func[[1]]
      } else{
        # Fill the variables wiht 0
        # generate the average average end-survival for this threshold, across all the environments 
        mean<-NA
        # and now for the average time till half life 
        SD<-NA
        # do the same for the th
        output_env_func<-cbind(mean, SD)
        # add to the list 
        list_3_3_2[[length(list_3_3_2)+1]]<-output_env_func
      }
      print(paste('model 3.3.2 opt par-env combination =', i))
    }
    
    # put it in a dataframe 
    outcome_opt_df<-ldply(list_3_3_2, data.frame)
    
    outcome_opt_df$threshold1<-th1_th2_th3_comb[,1]
    outcome_opt_df$threshold2<-th1_th2_th3_comb[,2]
    outcome_opt_df$threshold3<-th1_th2_th3_comb[,3]
    
    # best HL (new code)
    HL_best<-outcome_opt_df[(which.max(outcome_opt_df$mean)),]
    
    outcome_opt_df_332<<-outcome_opt_df
    
    # save the data 
    setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/MOD_3_3_2/12_environments/Optimization")
    save(outcome_opt_df_332, file=paste0(format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'opt_out', modelType, 'd', days, 'N', N, 'dayh', daylight_h, 'num_th', num_th,  '.Rda'))
    
    # Change the dataframe so that 'NA' for both HL and ES are not plotted
    outcome_opt_df_plot<-subset(outcome_opt_df, (!is.na(outcome_opt_df[,1])) & (!is.na(outcome_opt_df[,2])))
    
    
    # halflife
    plot_ly(outcome_opt_df_plot, x = ~threshold1, y = ~threshold2, z = ~threshold3, color = ~mean) %>%
      add_markers(size=~mean, marker=list(sizeref=0.02, sizemode='area')) %>%
      layout(scene = list(xaxis = list(range=c(-0.6, 0.6),title = 'TH1'),
                          yaxis = list(range=c(-0.6, 0.6),title = 'TH2'),
                          zaxis = list(range=c(-0.6, 0.6),title = 'TH3')),
             title = list(text='3.3.2 Mean Halflife - 3 thresholds ', y=0.95))
    
    # Mark end of optimization simulation 
    beep()
    print('332 opt done')
    
  }else if (modelType==41){
      
      # Set the number of options for which each trheshold needs to be tested 
      num_th<-num_th
      # set the minima 
      min_th_fr<-0
      min_th_flr<-(-0.6)
      # set the maxima
      max_th_fr<-4
      max_th_flr<-0.6
      # create the vectors
      th_fr_vec<-linspace(x1=min_th_fr, x2=max_th_fr, n=num_th)
      th_flr_vec<-linspace(x1=min_th_flr, x2=max_th_flr, n=num_th)
      # create a matrix that contains all possible combinations 
      # var 1 = th 1
      # var 2 = th 2 
      thfr_thflr_comb<-as.matrix(expand.grid(th_fr_vec, th_flr_vec))
      
      # Now, make a for loop 
      for (i in 1:nrow(thfr_thflr_comb)){
        if (i==1){
          list_4_1<-list()
        }
        
        cur_th_fr<-thfr_thflr_comb[i,1]
        cur_th_flr<-thfr_thflr_comb[i,2]
        
        # Run the environment function 

          env_func_4_1_par(days = days, N= N, th_forage_fr = cur_th_fr, th_forage_flr = cur_th_flr, daylight_h = daylight_h, modelType=modelType)
          # Add the calculated mean and SD to the list 
          list_4_1[[length(list_4_1)+1]]<-output_env_func[[1]]
          
        
        
        print(paste('model 4.1 opt par-env combination =', i, 'fr=', cur_th_fr, ' flr=', cur_th_flr))
      }
      
      # put it in a dataframe 
      outcome_opt_df<-ldply(list_4_1, data.frame)
      # Add the threshold columsn 
      outcome_opt_df$threshold_fr<-thfr_thflr_comb[,1]
      outcome_opt_df$threshold_flr<-thfr_thflr_comb[,2]
      
      # best HL
      HL_best<-outcome_opt_df[(which.max(outcome_opt_df$mean)),]
      # Rename so when I import it back in, it is clear which model this is
      outcome_opt_df_41<<-outcome_opt_df
      # save the data 
      setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/MOD_4_1/12_environments/Optimization")
      save(outcome_opt_df_41, file=paste0(format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_opt_out', modelType, 'd', days, 'N', N, 'dayh', daylight_h, 'numTh', num_th,  '.Rda'))
      
      # create a matrix with the values for HL
      HL_matrix<-matrix(data=outcome_opt_df$mean, ncol=length(th_fr_vec))
      # Plot
      HL_plot<-persp3D(z=HL_matrix, xlab='th_fr', ylab='th_flr', zlab='Timesteps at 50% alive', main='Optimal survival for th_fr and th_flr - Halflife', zlim= c(0, (days*72)))
      
      # mark that opt is done 
      beep()
      print('run opt 4.1 is done')
      
  }else if(modelType=='42'){
    
        # Set the number of options for which each threshold needs to be tested 
        num_th<-num_th
        # set the minima 
        min_th_fr1<-0
        min_th_fr2<-0
        min_th_flr1<-(-0.6)
        min_th_flr2<-(-0.6)
        # set the maxima
        max_th_fr1<-4
        max_th_fr2<-4
        max_th_flr1<-0.6
        max_th_flr2<-0.6
        # create the vectors
        th_fr1_vec<-linspace(x1=min_th_fr1, x2=max_th_fr1, n=num_th)
        th_fr2_vec<-linspace(x1=min_th_fr2, x2=max_th_fr2, n=num_th)
        th_flr1_vec<-linspace(x1=min_th_flr1, x2=max_th_flr1, n=num_th)
        th_flr2_vec<-linspace(x1=min_th_flr2, x2=max_th_flr2, n=num_th)
        # create a matrix that contains all possible combinations 
        # var 1 = th fr1 
        # var 2 = th fr2
        # var 3 = th flr1 
        # var 4 = th flr2
        th_fr1_th_fr2_th_flr1_th_flr2_comb<-as.matrix(expand.grid(th_fr1_vec, th_fr2_vec, th_flr1_vec, th_flr2_vec))
        
        # Now, make a for loop 
        for (i in 1:nrow(th_fr1_th_fr2_th_flr1_th_flr2_comb)){
          if (i==1){
            list_4_2<-list()
          }
          
          cur_th_fr1<-th_fr1_th_fr2_th_flr1_th_flr2_comb[i,1]
          cur_th_fr2<-th_fr1_th_fr2_th_flr1_th_flr2_comb[i,2]
          cur_th_flr1<-th_fr1_th_fr2_th_flr1_th_flr2_comb[i,3]
          cur_th_flr2<-th_fr1_th_fr2_th_flr1_th_flr2_comb[i,4]
          
          # but only do this in the case that th2 (both fr or flr) is actually larger than th 1 (both fr and flr)
          if ((cur_th_fr2>cur_th_fr1) && (cur_th_flr2>cur_th_flr1)){
            # Run th eenvironment function 
            env_func_4_2_par(days = days, N= N, th_forage_fr1 = cur_th_fr1, th_forage_fr2 = cur_th_fr2, th_forage_flr1=cur_th_flr1, th_forage_flr2=cur_th_flr2, daylight_h = daylight_h, modelType=modelType)
            # put it in the list 
            list_4_2[[length(list_4_2)+1]]<-output_env_func[[1]]
            
          } else{
            # Fill the variables wiht 0
            # generate the average average HL
            mean<-0
            # and now for the sd
            SD<-0
            # do the same for the 
            output_env_func<-cbind(mean, SD)
            # put it in the list 
            list_4_2[[length(list_4_2)+1]]<-output_env_func
            
          }
          
          print(paste('model 4.2 opt par-env combination =', i))
        }
        
        
        # put it in a dataframe 
        outcome_opt_df<-ldply(list_4_2, data.frame)
        
        outcome_opt_df$fr_th1<-th_fr1_th_fr2_th_flr1_th_flr2_comb[,1]
        outcome_opt_df$fr_th2<-th_fr1_th_fr2_th_flr1_th_flr2_comb[,2]
        outcome_opt_df$flr_th1<-th_fr1_th_fr2_th_flr1_th_flr2_comb[,3]
        outcome_opt_df$flr_th2<-th_fr1_th_fr2_th_flr1_th_flr2_comb[,4]
        
        # calculate the best HL across the different combinations 
        HL_best<-outcome_opt_df[(which.max(outcome_opt_df$mean)),]
        # Name after the model so when I upload it from saved files, It indicates which modeltype was used 
        outcome_opt_df_42<<-outcome_opt_df
        
        # save the data 
        setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/MOD_4_2/12_environments/Optimization")
        save(outcome_opt_df_42, file=paste0(format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_opt_out', modelType, 'd', days, 'N', N, 'dayh', daylight_h, 'numTh', num_th,  '.Rda'))
        
        # create a matrix with the values for HL
        #HL_matrix<-matrix(data=outcome_opt_df$mean, ncol=length(th_fr1_vec))
        # plot it 
        #HL_plot<-persp3D(z=HL_matrix, xlab='th_sc1', ylab='th_sc2', zlab='Timesteps at 50% alive', main='Optimal survival for th_sc1 and th_sc2 - Halflife') #, zlim= c(0, (days*72)))
        
  }else if (modelType=='431'){
        
        # Set the number of options for which each threshold needs to be tested 
        num_th<-num_th
        # set the minima 
        min_th_fr1<-0
        min_th_fr2<-0
        min_th_fr3<-0
        min_th_flr1<-(-0.6)
        min_th_flr2<-(-0.6)
        min_th_flr3<-(-0.6)
        # set the maxima
        max_th_fr1<-4
        max_th_fr2<-4
        max_th_fr3<-4
        max_th_flr1<-0.6
        max_th_flr2<-0.6
        max_th_flr3<-0.6
        # create the vectors
        th_fr1_vec<-linspace(x1=min_th_fr1, x2=max_th_fr1, n=num_th)
        th_fr2_vec<-linspace(x1=min_th_fr2, x2=max_th_fr2, n=num_th)
        th_fr3_vec<-linspace(x1=min_th_fr3, x2=max_th_fr3, n=num_th)
        th_flr1_vec<-linspace(x1=min_th_flr1, x2=max_th_flr1, n=num_th)
        th_flr2_vec<-linspace(x1=min_th_flr2, x2=max_th_flr2, n=num_th)
        th_flr3_vec<-linspace(x1=min_th_flr3, x2=max_th_flr3, n=num_th)
        # create a matrix that contains all possible combinations 
        # var 1 = th fr1 
        # var 2 = th fr2
        # var 3 = th fr3 
        # var 4 = th flr1 
        # var 5 = th flr2
        # var 6 = th flr 3
        th_fr1_th_fr2_th_fr3_th_flr1_th_flr2_th_flr3_comb<-as.matrix(expand.grid(th_fr1_vec, th_fr2_vec, th_fr3_vec, th_flr1_vec, th_flr2_vec, th_flr3_vec))
        
        # Now, make a for loop 
        for (i in 1:nrow(th_fr1_th_fr2_th_fr3_th_flr1_th_flr2_th_flr3_comb)){
          if (i==1){
            list_4_3_1<-list()
          }
          
          cur_th_fr1<-th_fr1_th_fr2_th_fr3_th_flr1_th_flr2_th_flr3_comb[i,1]
          cur_th_fr2<-th_fr1_th_fr2_th_fr3_th_flr1_th_flr2_th_flr3_comb[i,2]
          cur_th_fr3<-th_fr1_th_fr2_th_fr3_th_flr1_th_flr2_th_flr3_comb[i,3]
          cur_th_flr1<-th_fr1_th_fr2_th_fr3_th_flr1_th_flr2_th_flr3_comb[i,4]
          cur_th_flr2<-th_fr1_th_fr2_th_fr3_th_flr1_th_flr2_th_flr3_comb[i,5]
          cur_th_flr3<-th_fr1_th_fr2_th_fr3_th_flr1_th_flr2_th_flr3_comb[i,6]
          
          # but only do this in the case that th2 (both fr or flr) is actually larger than th 1 (both fr and flr)
          if ((cur_th_fr2>cur_th_fr1) && (cur_th_fr3>cur_th_fr2) && (cur_th_flr2>cur_th_flr1) && (cur_th_flr3> cur_th_flr2)){
            # Run th eenvironment function 
            env_func_4_3_1_par(days = days, N= N, th_forage_fr1 = cur_th_fr1, th_forage_fr2 = cur_th_fr2, th_forage_fr3= cur_th_fr3, th_forage_flr1=cur_th_flr1, th_forage_flr2=cur_th_flr2, th_forage_flr3=cur_th_flr3, daylight_h = daylight_h, modelType=modelType)
            # put it in the list 
            list_4_3_1[[length(list_4_3_1)+1]]<-output_env_func[[1]]
            
          } else{
            # Fill the variables wiht 0
            # generate the average average HL
            mean<-0
            # and now for the sd
            SD<-0
            # do the same for the 
            output_env_func<-cbind(mean, SD)
            # put it in the list 
            list_4_3_1[[length(list_4_3_1)+1]]<-output_env_func
            
          }
          
          print(paste('model 4.3_1 opt par-env combination =', i))
        }
        
        # put it in a dataframe 
        outcome_opt_df<-ldply(list_4_3_1, data.frame)
        
        outcome_opt_df$fr_th1<-th_fr1_th_fr2_th_fr3_th_flr1_th_flr2_th_flr3_comb[,1]
        outcome_opt_df$fr_th2<-th_fr1_th_fr2_th_fr3_th_flr1_th_flr2_th_flr3_comb[,2]
        outcome_opt_df$fr_th3<-th_fr1_th_fr2_th_fr3_th_flr1_th_flr2_th_flr3_comb[,3]
        outcome_opt_df$flr_th1<-th_fr1_th_fr2_th_fr3_th_flr1_th_flr2_th_flr3_comb[,4]
        outcome_opt_df$flr_th2<-th_fr1_th_fr2_th_fr3_th_flr1_th_flr2_th_flr3_comb[,5]
        outcome_opt_df$flr_th3<-th_fr1_th_fr2_th_fr3_th_flr1_th_flr2_th_flr3_comb[,6]
        
        # calculate the best HL across the different combinations 
        HL_best<-outcome_opt_df[(which.max(outcome_opt_df$mean)),]
        # Name after the model so when I upload it from saved files, It indicates which modeltype was used 
        outcome_opt_df_431<<-outcome_opt_df
        
        # save the data 
        setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/MOD_4_3_1/12_environments/Optimization")
        save(outcome_opt_df_431, file=paste0(format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_opt_out', modelType, 'd', days, 'N', N, 'dayh', daylight_h, 'numTh', num_th,  '.Rda'))
        
        # create a matrix with the values for HL
        #HL_matrix<-matrix(data=outcome_opt_df$mean, ncol=length(th_fr1_vec))
        # plot it 
        #HL_plot<-persp3D(z=HL_matrix, xlab='th_sc1', ylab='th_sc2', zlab='Timesteps at 50% alive', main='Optimal survival for th_sc1 and th_sc2 - Halflife') #, zlim= c(0, (days*72)))
        
  } else if(modelType=='432'){
    
      # Set the number of options for which each threshold needs to be tested 
      num_th<-num_th
      # set the minima 
      min_th_fr1<-0
      min_th_fr2<-0
      min_th_fr3<-0
      min_th_flr1<-(-0.6)
      min_th_flr2<-(-0.6)
      min_th_flr3<-(-0.6)
      # set the maxima
      max_th_fr1<-4
      max_th_fr2<-4
      max_th_fr3<-4
      max_th_flr1<-0.6
      max_th_flr2<-0.6
      max_th_flr3<-0.6
      # create the vectors
      th_fr1_vec<-linspace(x1=min_th_fr1, x2=max_th_fr1, n=num_th)
      th_fr2_vec<-linspace(x1=min_th_fr2, x2=max_th_fr2, n=num_th)
      th_fr3_vec<-linspace(x1=min_th_fr3, x2=max_th_fr3, n=num_th)
      th_flr1_vec<-linspace(x1=min_th_flr1, x2=max_th_flr1, n=num_th)
      th_flr2_vec<-linspace(x1=min_th_flr2, x2=max_th_flr2, n=num_th)
      th_flr3_vec<-linspace(x1=min_th_flr3, x2=max_th_flr3, n=num_th)
      # create a matrix that contains all possible combinations 
      # var 1 = th fr1 
      # var 2 = th fr2
      # var 3 = th fr3 
      # var 4 = th flr1 
      # var 5 = th flr2
      # var 6 = th flr 3
      th_fr1_th_fr2_th_fr3_th_flr1_th_flr2_th_flr3_comb<-as.matrix(expand.grid(th_fr1_vec, th_fr2_vec, th_fr3_vec, th_flr1_vec, th_flr2_vec, th_flr3_vec))
      
      # Now, make a for loop 
      for (i in 1:nrow(th_fr1_th_fr2_th_fr3_th_flr1_th_flr2_th_flr3_comb)){
        if (i==1){
          list_4_3_2<-list()
        }
        
        cur_th_fr1<-th_fr1_th_fr2_th_fr3_th_flr1_th_flr2_th_flr3_comb[i,1]
        cur_th_fr2<-th_fr1_th_fr2_th_fr3_th_flr1_th_flr2_th_flr3_comb[i,2]
        cur_th_fr3<-th_fr1_th_fr2_th_fr3_th_flr1_th_flr2_th_flr3_comb[i,3]
        cur_th_flr1<-th_fr1_th_fr2_th_fr3_th_flr1_th_flr2_th_flr3_comb[i,4]
        cur_th_flr2<-th_fr1_th_fr2_th_fr3_th_flr1_th_flr2_th_flr3_comb[i,5]
        cur_th_flr3<-th_fr1_th_fr2_th_fr3_th_flr1_th_flr2_th_flr3_comb[i,6]
        
        # but only do this in the case that th2 (both fr or flr) is actually larger than th 1 (both fr and flr)
        if ((cur_th_fr2>cur_th_fr1) && (cur_th_fr3>cur_th_fr2) && (cur_th_flr2>cur_th_flr1) && (cur_th_flr3> cur_th_flr2)){
          # Run th eenvironment function 
          env_func_4_3_2_par(days = days, N= N, th_forage_fr1 = cur_th_fr1, th_forage_fr2 = cur_th_fr2, th_forage_fr3= cur_th_fr3, th_forage_flr1=cur_th_flr1, th_forage_flr2=cur_th_flr2, th_forage_fr3=cur_th_flr3, daylight_h = daylight_h, modelType=modelType)
          # put it in the list 
          list_4_3_2[[length(list_4_3_2)+1]]<-output_env_func[[1]]
          
        } else{
          # Fill the variables wiht 0
          # generate the average average HL
          mean<-0
          # and now for the sd
          SD<-0
          # do the same for the 
          output_env_func<-cbind(mean, SD)
          # put it in the list 
          list_4_3_2[[length(list_4_3_2)+1]]<-output_env_func
          
        }
        
        print(paste('model 4.3_2 opt par-env combination =', i))
      }
      
      # put it in a dataframe 
      outcome_opt_df<-ldply(list_4_3_2, data.frame)
      
      outcome_opt_df$fr_th1<-th_fr1_th_fr2_th_fr3_th_flr1_th_flr2_th_flr3_comb[,1]
      outcome_opt_df$fr_th2<-th_fr1_th_fr2_th_fr3_th_flr1_th_flr2_th_flr3_comb[,2]
      outcome_opt_df$fr_th3<-th_fr1_th_fr2_th_fr3_th_flr1_th_flr2_th_flr3_comb[,3]
      outcome_opt_df$flr_th1<-th_fr1_th_fr2_th_fr3_th_flr1_th_flr2_th_flr3_comb[,4]
      outcome_opt_df$flr_th2<-th_fr1_th_fr2_th_fr3_th_flr1_th_flr2_th_flr3_comb[,5]
      outcome_opt_df$flr_th3<-th_fr1_th_fr2_th_fr3_th_flr1_th_flr2_th_flr3_comb[,6]
      
      # calculate the best HL across the different combinations 
      HL_best<-outcome_opt_df[(which.max(outcome_opt_df$mean)),]
      # Name after the model so when I upload it from saved files, It indicates which modeltype was used 
      outcome_opt_df_431<<-outcome_opt_df
      
      # save the data 
      setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/MOD_4_3_2/12_environments/Optimization")
      save(outcome_opt_df_432, file=paste0(format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_opt_out', modelType, 'd', days, 'N', N, 'dayh', daylight_h, 'numTh', num_th,  '.Rda'))
      
      # create a matrix with the values for HL
      #HL_matrix<-matrix(data=outcome_opt_df$mean, ncol=length(th_fr1_vec))
      # plot it 
      #HL_plot<-persp3D(z=HL_matrix, xlab='th_sc1', ylab='th_sc2', zlab='Timesteps at 50% alive', main='Optimal survival for th_sc1 and th_sc2 - Halflife') #, zlim= c(0, (days*72)))
        
  }else {
     print('help stop, something is wrong with the modeltype ')
    
  }
  beep()
}) # end the sys.time function 










