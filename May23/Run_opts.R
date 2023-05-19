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
library(plot3D)         # for the 3d surface plot (simple basic one)
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



###########################
##     OPTIMIZATIONS     ##
###########################

# values for local use 
    # days<-30
    # N<-100
    # th_forage_sc<-0.2
    # daylight_h<-8
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
    days<-5
  # set individuals
    N<-10
  # Set the model type: 
    modelType<-12

# set the number of cores 
numCores<-(detectCores()-1)
registerDoParallel(numCores)

# set up the values for which you want to optimise 
# This needs to be different for the different models 
if (modelType==11){
  # Set the number of thresholds you want to test for
    num_th<-1000
  # The minimum 
    min_th_val<-0
  # And the maximum 
    max_th_val<-0.4
  # create the vector that has the actual threshold values in it 
    th_vec<-runif(n=num_th, min=min_th_val, max=max_th_val)
  
  # Run The optimization for model 1.1 and its thresholds 
    outcome_opt<-foreach(i=1:num_th, .packages = c("truncnorm", 'foreach', 'doParallel', 'purrr')) %dopar% {
      
      cur_th<-th_vec[i]
      
      env_func_1_1(days = days, N= N, th_forage_sc = cur_th, daylight_h = daylight_h, modelType=modelType)
      
    } # end of the foreach loop (individuals) 
    
    # clean up cluster 
    stopImplicitCluster()
    
    # Now check which threshold has the highest end-survival 
    outcome_opt_df<-ldply(outcome_opt, data.frame)
    
    outcome_opt_df$threshold<-th_vec
    # best ES
    ES_best<-outcome_opt_df[(which.max(outcome_opt_df$mean_ES_cur_th)),]
    
    # best HL
    HL_best<-outcome_opt_df[(which.max(outcome_opt_df$mean_HL_cur_th)),]
    # melt them 
    #outcome_df_melt<-melt(outcome_opt_df, id.vars='threshold')
    
    # save the data 
    setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/MOD_1_1/Optimization")
    
    save(outcome_opt_df, file=paste0('outcome_opt_', modelType, 'd', days, 'N', N, '_', format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.Rda'))
    
    coeff<-(days*72)
    endsurvival_col<-rgb(0.2, 0.6, 0.9, 1)
    halflife_col<-"#69b3a2"
      
    # Make a plot 
    opt_plot<-ggplot(outcome_opt_df, aes(x=threshold))+
      geom_line(aes(y=mean_ES_cur_th), size=2, col=endsurvival_col)+
      geom_line(aes(y=mean_HL_cur_th/coeff), size=2, col=halflife_col) + 
      scale_y_continuous(
        
        # Features of the first axis
        name = "End survival %",
        
        # Add a second axis and specify its features
        sec.axis = sec_axis(~.*coeff, name=paste("Timestep of halflife with total TS=", coeff))
      )+ 
      geom_point(aes(x=ES_best$threshold, y=ES_best$mean_ES_cur_th),colour='red', size=5)+
      geom_point(aes(x=HL_best$threshold, y=(HL_best$mean_HL_cur_th/coeff)), colour='red', size=5)+
      
      #theme_ipsum() +
      
      theme(
        axis.title.y = element_text(color = endsurvival_col, size=13, face='bold'),
        axis.title.y.right = element_text(color = halflife_col, size=13, face='bold'),
        axis.title.x=element_text(size=13, face='bold')
      ) +
      ggtitle(paste('Endsurvival and Timestep of Halflife for each threshold value, N=',N, ' days=',days, 'num_th=',num_th))
    opt_plot
    
    
    # end of if modeltype = 1.1 
} else if (modelType==12){
    # Set the number of options for which each trheshold needs to be tested 
      num_th<-10
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
      
      # Now, its time to run the different combinations in parallel 
      outcome_opt<-foreach(i=1:(nrow(th1_th2_comb)), .packages = c("truncnorm", 'foreach', 'doParallel', 'purrr')) %dopar% {
        
        cur_th1<-th1_th2_comb[i,1]
        cur_th2<-th1_th2_comb[i,2]
        
        # but only do this in the case that th2 is actually larger than th 1 
        if (cur_th2>cur_th1){
        env_func_1_2(days = days, N= N, th_forage_sc1 = cur_th1, th_forage_sc2 = cur_th2, daylight_h = daylight_h, modelType=modelType)
        } else{
          # Fill the variables wiht 0
            # generate the average average end-survival for this threshold, across all the environments 
            mean_ES_cur_th<-0
            # and now for the average time till halflife 
            mean_HL_cur_th<-0
            # do the same for the 
            output_env_func<-cbind(mean_ES_cur_th, mean_HL_cur_th)
            return(output_env_func)
          }
        
      } # end of the foreach loop (individuals) 
      
      # clean up cluster 
      stopImplicitCluster()
      
      # put it in a dataframe 
      outcome_opt_df<-ldply(outcome_opt, data.frame)
      
      outcome_opt_df$threshold1<-th1_th2_comb[,1]
      outcome_opt_df$threshold2<-th1_th2_comb[,2]
      
      # best ES
      ES_best<-outcome_opt_df[(which.max(outcome_opt_df$mean_ES_cur_th)),]
      # best HL
      HL_best<-outcome_opt_df[(which.max(outcome_opt_df$mean_HL_cur_th)),]

      # save the data 
      setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/MOD_1_2/Optimization")
      save(outcome_opt_df, file=paste0('outcome_opt_', modelType, 'd', days, 'N', N, '_', format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.Rda'))
      
      # create a matrix with the values for ES 
      ES_matrix<-matrix(data=outcome_opt_df$mean_ES_cur_th, ncol=length(th2_vec))
      # create a matrix with the values for HL 
      HL_matrix<-matrix(data=outcome_opt_df$mean_HL_cur_th, ncol=length(th2_vec))
      
      # heatmap(ES_matrix, Colv=NA, Rowv=NA, scale='column')
      # dev.new()
  
      ES_plot<-persp3D(z=ES_matrix, xlab='th_sc1', ylab='th_sc2', zlab='survival', main='Optimal survival for th_sc1 and th_sc2 - End Survival') #, zlim= c(0, 1))
      
      HL_plot<-persp3D(z=HL_matrix, xlab='th_sc1', ylab='th_sc2', zlab='Timesteps at 50% alive', main='Optimal survival for th_sc1 and th_sc2 - Halflife') #, zlim= c(0, (days*72)))
      
      
} else if (modelType==131){
  
          # Set the number of options for which each trheshold needs to be tested 
          num_th<-10
          # set the minima 
          min_th_sc1<-0
          min_th_sc2<-0
          min_th_sc3<-0
          # set the maxima
          max_th_sc1<-0.4
          max_th_sc2<-0.4
          max_th_sc3<-0.4
          # create the vectors
          th1_vec<-runif(n=num_th, min=min_th_sc1, max=max_th_sc1)
          th2_vec<-runif(n=num_th, min=min_th_sc2, max=max_th_sc2)
          th3_vec<-runif(n=num_th, min=min_th_sc3, max=max_th_sc3)
          
          # create a matrix that contains all possible combinations 
          # var 1 = th 1
          # var 2 = th 2 
          # var 3 = th 3
          th1_th2_th3_comb<-as.matrix(expand.grid(th1_vec, th2_vec, th3_vec))
          
          # Now, its time to run the different combinations in parallel 
          outcome_opt<-foreach(i=1:(nrow(th1_th2_th3_comb)), .packages = c("truncnorm", 'foreach', 'doParallel', 'purrr')) %dopar% {
            
            cur_th1<-th1_th2_th3_comb[i,1]
            cur_th2<-th1_th2_th3_comb[i,2]
            cur_th3<-th1_th2_th3_comb[i,3]
            
            # but only do this in the case that th2 is actually larger than th 1 
            if ((cur_th1< cur_th2) && (cur_th1< cur_th3) && (cur_th2<cur_th3)){
              env_func_1_3_1(days = days, N= N, th_forage_sc1 = cur_th1, th_forage_sc2 = cur_th2, th_forage_sc3 = cur_th3, daylight_h = daylight_h, modelType=modelType)
            } else{
              # Fill the variables wiht 0
              # generate the average average end-survival for this threshold, across all the environments 
              mean_ES_cur_th<-0
              # and now for the average time till halflife 
              mean_HL_cur_th<-0
              # do the same for the 
              output_env_func<-cbind(mean_ES_cur_th, mean_HL_cur_th)
              return(output_env_func)
            }
            
          } # end of the foreach loop (individuals) 
          
          # clean up cluster 
          stopImplicitCluster()
          
          # put it in a dataframe 
          outcome_opt_df<-ldply(outcome_opt, data.frame)
          
          outcome_opt_df$threshold1<-th1_th2_th3_comb[,1]
          outcome_opt_df$threshold2<-th1_th2_th3_comb[,2]
          outcome_opt_df$threshold2<-th1_th2_th3_comb[,3]
          
          # best ES
          ES_best<-outcome_opt_df[(which.max(outcome_opt_df$mean_ES_cur_th)),]
          # best HL
          HL_best<-outcome_opt_df[(which.max(outcome_opt_df$mean_HL_cur_th)),]
          
          
          
          
          
          
          
          
          # save the data 
          setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/MOD_1_2/Optimization")
          save(outcome_opt_df, file=paste0('outcome_opt_', modelType, 'd', days, 'N', N, '_', format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.Rda'))
          
          # create a matrix with the values for ES 
          ES_matrix<-matrix(data=outcome_opt_df$mean_ES_cur_th, ncol=length(th2_vec))
          # create a matrix with the values for HL 
          HL_matrix<-matrix(data=outcome_opt_df$mean_HL_cur_th, ncol=length(th2_vec))
          
          # heatmap(ES_matrix, Colv=NA, Rowv=NA, scale='column')
          # dev.new()
          
          ES_plot<-persp3D(z=ES_matrix, xlab='th_sc1', ylab='th_sc2', zlab='survival', main='Optimal survival for th_sc1 and th_sc2 - End Survival') #, zlim= c(0, 1))
          
          HL_plot<-persp3D(z=HL_matrix, xlab='th_sc1', ylab='th_sc2', zlab='Timesteps at 50% alive', main='Optimal survival for th_sc1 and th_sc2 - Halflife') #, zlim= c(0, (days*72)))
          
          
          
          
          
  }else {
    print('help stop, something is wrong with the modeltype ')
  }
beep()
}) # end the time thingey 





    
    




    