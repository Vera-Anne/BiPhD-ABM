#################################
# Small bird in winter - ABM 
# Start date: 16/05/2023
# Vera Vinken 
# all models - for running them (singular)
#################################

##############################
#      load packages         #
##############################
# library(usethis)
# library(devtools)
library(truncnorm)
# library(pracma)
library(ggplot2)
# library(plotly) # for 3D surface plot 
# library(rgl)
# library(plot3D)
# library(htmlwidgets)
# library(webshot)
# library(withr)
# library('plyr')
# library('gridExtra')
# library(grid)
# library(lattice)
# library(dplyr)
library(data.table)
# library(tidyverse)
# library(viridis)
library(foreach)
library(doParallel)
library(purrr)             # for making lists into dataframes 
library(beepr)
# library(tidyr)
library(doParallel)         # For runing code parallel with dopar function 
library(foreach)            # For running code parallel 
library(ggpubr)             # To arrange plots 
library(gridExtra)          # for grid.arrange 

# link to the function file 
# This contains all the general, smaller funcitons needed for the models 
#setwd("C:/Local_R/BiPhD-ABM/May23") # for hp elitebook 
setwd("C:/Local_R/BiPhD-ABM/May23") # for hp elitebook 
source('MOD_1_FuncSource.R')
source('ModelSource.R')

#################################################################
##   Model 1.1: Non-hoarding bird, Access to Stomach Content   ##
#################################################################

# Run model 1.1 

system.time({
  
  # clear workspace
  rm(list=ls())
  # load everything 
  setwd("C:/Local_R/BiPhD-ABM/May23") # for hp elitebook 
  source('MOD_1_FuncSource.R')
  source('ModelSource.R')
  
  # Run the model 
  mod_1_1(days=30, N=1000, env_type = 13, th_forage_sc = 0.09795918 , daylight_h = 8)
  
  # First put together some relevant dataframes (we want both individual data and mean data)
  save_11_list<-list(total_vars_df11, output_df_list_raw11)
  #  save the data 
  beep()
  setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/MOD_1_1/")
  save(save_11_list, file=paste0(format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_mod_run11', 'd', days, 'N', N,'env_t', env_type, 'th_sc', th_forage_sc, 'dayh', daylight_h,   '.Rda'))
})

# create plots 
plots_12_func(inputdata=save_11_list[[1]], modelType='11')
# if needed 
plot_12_11

# and then parallel 
system.time({
  env_func_1_1_par(days = 30, N= 1000, th_forage_sc = 0.09795918, daylight_h = 8, modelType = 11)
})

# Now do an overview image 
plot_env_18_surv(output_env_func)


######################################################################
##   Model 1.2: Leftover-hoarding bird, Access to Stomach Content   ##
######################################################################

# Run model 1.2
system.time({
  # clear workspace
  rm(list=ls())
  # load everything 
  setwd("C:/Local_R/BiPhD-ABM/May23") # for hp elitebook 
  source('MOD_1_FuncSource.R')
  source('ModelSource.R')
  
  # run model 
  mod_1_2(days= 30, N= 1000, env_type=13, th_forage_sc1=0.05714286, th_forage_sc2=0.06530612 , daylight_h=8)
  
  # First put together some relevant dataframes (we want both individual data and mean data)
  save_12_list<-list(total_vars_df12, output_df_list_raw12)
  #  save the data 
  beep()
  setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/MOD_1_2/")
  save(save_12_list, file=paste0(format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_mod_run12', 'd', days, 'N', N,'env_t', env_type, 'th_sc1', th_forage_sc1, 'th_sc2', th_forage_sc2, 'dayh', daylight_h,   '.Rda'))
})

# create plots 
plots_12_func(inputdata=save_12_list[[1]], modelType='12')
# if needed 
plot_12_12

# RUN ENVIRONMENTS 1.2 
system.time({
  
  # clear workspace
  rm(list=ls())
  # load everything 
  setwd("C:/Local_R/BiPhD-ABM/May23") # for hp elitebook 
  source('MOD_1_FuncSource.R')
  source('ModelSource.R')
  
  env_func_1_2_par(days = 30, N= 1000, th_forage_sc1 = 0.05714286, th_forage_sc2 = 0.06530612 , daylight_h = 8, modelType = 12)
})

# Now do an overview image 
plot_env_18_surv(output_env_func, modelType=12)


#################################################################################
##   Model 1.3.1 : Direct hoarding bird, Access to Stomach Content -hoard top  ##
#################################################################################

# Run model 1.3.1
system.time({
  
  # clear workspace
  rm(list=ls())
  # load everything 
  setwd("C:/Local_R/BiPhD-ABM/May23") # for hp elitebook 
  source('MOD_1_FuncSource.R')
  source('ModelSource.R')
  
  # run the model 
  mod_1_3_1(days=30, N=1000, env_type=8, th_forage_sc1=0.1, th_forage_sc2=0.2, th_forage_sc3=0.3, daylight_h=8)
  
  # First put together some relevant dataframes (we want both individual data and mean data)
  save_131_list<-list(total_vars_df131, output_df_list_raw131)
  #  save the data 
  setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/MOD_1_3_1/")
  save(save_131_list, file=paste0(format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_mod_run131', 'd', days, 'N', N,'env_t', env_type, 'th_sc1', th_forage_sc1, 'th_sc2', th_forage_sc2, 'th_sc3', th_forage_sc3, 'dayh', daylight_h,   '.Rda'))
})

# create plots 
plots_12_func(inputdata=save_131_list[[1]], modelType='131')
# if needed 
plot_12_131

# rUN IT FOR THE 18 ENVIRONMENTS 
system.time({
  
  # clear workspace
  rm(list=ls())
  # load everything 
  setwd("C:/Local_R/BiPhD-ABM/May23") # for hp elitebook 
  source('MOD_1_FuncSource.R')
  source('ModelSource.R')
  # run the model 
  env_func_1_3_1_par(days = 30, N= 1000, th_forage_sc1 = 0.1, th_forage_sc2 = 0.2, th_forage_sc3 = 0.3, daylight_h = 8, modelType = 131)
})

# Now do an overview image 
plot_env_18_surv(output_env_func)

#################################################################################
##   Model 1.3.2 : Direct hoarding bird, Access to Stomach Content - rest top  ##
#################################################################################

# Run model 1.3.2 
system.time({
  
  # clear workspace
  rm(list=ls())
  # load everything 
  setwd("C:/Local_R/BiPhD-ABM/May23") 
  source('MOD_1_FuncSource.R')
  source('ModelSource.R')
  
  # run the model 
  mod_1_3_2(days=30, N=1000, env_type=8, th_forage_sc1=0.1, th_forage_sc2=0.2, th_forage_sc3=0.3, daylight_h=8)
  
  # First put together some relevant dataframes (we want both individual data and mean data)
  save_132_list<-list(total_vars_df132, output_df_list_raw132)
  # save the data 
  setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/MOD_1_3_2/")
  save(save_132_list, file=paste0(format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_mod_run132', 'd', days, 'N', N,'env_t', env_type, 'th_sc1', th_forage_sc1, 'th_sc2', th_forage_sc2, 'th_sc3', th_forage_sc3, 'dayh', daylight_h,   '.Rda'))
})

# now plot
plots_12_func(inputdata = save_132_list[[1]], modelType='132')
# output
plot_12_132

# RUN THE 18 ENVIRONMENTS PARALLEL
system.time({
  
  # clear workspace
  rm(list=ls())
  # load everything 
  setwd("C:/Local_R/BiPhD-ABM/May23") # for hp elitebook 
  source('MOD_1_FuncSource.R')
  source('ModelSource.R')
  
  # run the model 
  env_func_1_3_2_par(days = 30, N= 1000, th_forage_sc1 = 0.1, th_forage_sc2 = 0.2, th_forage_sc3 = 0.3, daylight_h = 8, modelType = 132)
})

# Now do an overview image 
plot_env_18_surv(output_env_func)


#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#################################################################
##   Model 2.1: Non-hoarding bird, Access to Fat Reserves      ##
#################################################################

# Run model 2.1 
system.time({
  
  # clear workspace
  rm(list=ls())
  # load everything 
  setwd("C:/Local_R/BiPhD-ABM/May23") # for hp elitebook 
  source('MOD_1_FuncSource.R')
  source('ModelSource.R')
  
  # Run the model 
  mod_2_1(days=30, N=1000, env_type = 13, th_forage_fr = 1.55102 , daylight_h = 8)
  # First put together some relevant dataframes (we want both individual data and mean data)
  save_21_list<-list(total_vars_df21, output_df_list_raw21)
  # save the results 
  setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/MOD_2_1/")
  save(save_21_list, file=paste0(format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_mod_run21', 'd', days, 'N', N,'env_t', env_type, 'th_fr', th_forage_fr, 'dayh', daylight_h,   '.Rda'))
})

# create plots 
plots_12_func(inputdata=save_21_list[[1]], modelType='21')
# if needed 
plot_12_21

# Run IT FOR THE 18 ENVIRONMENTS -  parallel 
system.time({
  # clear workspace
  rm(list=ls())
  # load everything 
  setwd("C:/Local_R/BiPhD-ABM/May23") # for hp elitebook 
  source('MOD_1_FuncSource.R')
  source('ModelSource.R')
  
  env_func_2_1_par(days = 30, N= 1000, th_forage_fr = 1.55102, daylight_h = 8, modelType = 21)
})

# Now do an overview image 
plot_env_18_surv(output_env_func, modelType='21')


######################################################################
##   Model 2.2: Leftover-hoarding bird, Access to Fat - Reserves    ##
######################################################################

# Run model 2.2
system.time({
  
  # clear workspace
  rm(list=ls())
  # load everything 
  setwd("C:/Local_R/BiPhD-ABM/May23") # for hp elitebook 
  source('MOD_1_FuncSource.R')
  source('ModelSource.R')
  # run the model 
  mod_2_2(days=30, N=1000, env_type=13, th_forage_fr1=0.6530612, th_forage_fr2= 1.061224, daylight_h = 8)
  # First put together some relevant dataframes (we want both individual data and mean data)
  save_22_list<-list(total_vars_df22, output_df_list_raw22)
  # save the results 
  setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/MOD_2_2/")
  save(save_22_list, file=paste0(format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_mod_run22', 'd', days, 'N', N,'env_t', env_type, 'th_fr1', th_forage_fr1, 'th_fr2', th_forage_fr2, 'dayh', daylight_h,   '.Rda'))
})

# create plots 
plots_12_func(inputdata=save_22_list[[1]], modelType='22')
# if needed 
plot_12_22

# rUN IT FOR THE 18 ENVIRONMENTS - parallel 
system.time({
  # clear workspace
  rm(list=ls())
  # load everything 
  setwd("C:/Local_R/BiPhD-ABM/May23") # for hp elitebook 
  source('MOD_1_FuncSource.R')
  source('ModelSource.R')
  # run the model 
  env_func_2_2_par(days = 30, N= 1000, th_forage_fr1 = 0.6530612, th_forage_fr2 = 1.0612242 , daylight_h = 8, modelType = 22)
  # mark that simulation is done
  print('18 env MOD 2.2 done')
  beep()
})

# Now do an overview image 
plot_env_18_surv(output_env_func, modelType='22')


####################################################################################
##    Model 2.3.1: Direct hoarding bird, Access to Fat - Reserves - hoard top    ##
###################################################################################

# Run model 2.3.1
system.time({
  # clear workspace
  rm(list=ls())
  # load everything 
  setwd("C:/Local_R/BiPhD-ABM/May23") # for hp elitebook 
  source('MOD_1_FuncSource.R')
  source('ModelSource.R')
  # run the model 
  mod_2_3_1(days=30, N=1000, env_type=8, th_forage_fr1=1, th_forage_fr2=2, th_forage_fr3=3, daylight_h=8)
  # First put together some relevant dataframes (we want both individual data and mean data)
  save_231_list<-list(total_vars_df231, output_df_list_raw231)
  # save the results 
  setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/MOD_2_3_1/")
  save( save_231_list, file=paste0(format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_mod_run231', 'd', days, 'N', N,'env_t', env_type, 'th_fr1', th_forage_fr1, 'th_fr2', th_forage_fr2, 'th_fr3', th_forage_fr3, 'dayh', daylight_h,   '.Rda'))
  print('run single 2.3.1 done ')
  }) 

# create plots 
plots_12_func(inputdata= save_231_list[[1]], modelType='231')
# if needed 
plot_12_231

# rUN IT FOR THE 18 ENVIRONMENTS 
system.time({
  # clear workspace
  rm(list=ls())
  # load everything 
  setwd("C:/Local_R/BiPhD-ABM/May23") # for hp elitebook 
  source('MOD_1_FuncSource.R')
  source('ModelSource.R')
  # run the model 
  env_func_2_3_1_par(days = 30, N= 1000, th_forage_fr1 = 1, th_forage_fr2 = 2, th_forage_fr3 = 3, daylight_h = 8, modelType = 231)
  print('run 18 environments 2.3.1 done ')
  beep()
  })

# Now do an overview image 
plot_env_18_surv(output_env_func)

####################################################################################
##    Model 2.3.2: Direct hoarding bird, Access to Fat - Reserves - rest  top    ##
###################################################################################

# Run model 2.3.2 
system.time({
  # clear workspace
  rm(list=ls())
  # load everything 
  setwd("C:/Local_R/BiPhD-ABM/May23") # for hp elitebook 
  source('MOD_1_FuncSource.R')
  source('ModelSource.R')
  # run the model 
  mod_2_3_2(days=30, N=1000, env_type=8, th_forage_fr1=1, th_forage_fr2=2, th_forage_fr3=3, daylight_h=8)
  # First put together some relevant dataframes (we want both individual data and mean data)
  save_232_list<-list(total_vars_df232, output_df_list_raw232)
  # save the results 
  setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/MOD_2_3_2/")
  save(save_232_list, file=paste0(format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_mod_run232', 'd', days, 'N', N,'env_t', env_type, 'th_fr1', th_forage_fr1, 'th_fr2', th_forage_fr2, 'th_fr3', th_forage_fr3, 'dayh', daylight_h,   '.Rda'))
  beep()
  print('run 2.3.2 single done')
  })

# now plot
plots_12_func(inputdata = save_232_list[[1]], modelType='232')
# output
plot_12_232

# RUN IT FOR THE 18 ENVIRONMENTS 
system.time({
  # clear workspace
  rm(list=ls())
  # load everything 
  setwd("C:/Local_R/BiPhD-ABM/May23") # for hp elitebook 
  source('MOD_1_FuncSource.R')
  source('ModelSource.R')
  # run the model 
  env_func_2_3_2_par(days = 30, N= 1000, th_forage_fr1 = 1, th_forage_fr2 = 2, th_forage_fr3 = 3, daylight_h = 8, modelType = 232)
  beep()
  print('18 env 2.3.2 done')
  })

# Now do an overview image 
plot_env_18_surv(output_env_func)


# -----------------------------------------------------------------------------------------------------------------------------

#################################################################
##   Model 3.1: Non-hoarding bird, Access to Fat Loss Rate     ##
#################################################################

# Run model 3.1 
    system.time({
      
      # clear workspace
      rm(list=ls())
      # load everything 
      setwd("C:/Local_R/BiPhD-ABM/May23") # for hp elitebook 
      source('MOD_1_FuncSource.R')
      source('ModelSource.R')
      
      # Run the model 
      # Range of FLR: -0.6 to 0.6
      mod_3_1(days=30, N=1000, env_type = 13, th_forage_flr = 0.2326531 , daylight_h = 8)
      # First put together some relevant dataframes (we want both individual data and mean data)
      save_31_list<-list(total_vars_df31, output_df_list_raw31)
      # save the results 
      setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/MOD_3_1/")
      save(save_31_list, file=paste0(format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_mod_run31', 'd', days, 'N', N,'env_t', env_type, 'th_flr', th_forage_flr, 'dayh', daylight_h,   '.Rda'))
      # Mark the end of the simulation
      beep()
      print('single run 3.1 done')
      })
    # create plots 
    plots_12_func(inputdata=save_31_list[[1]], modelType='31')
    # if needed 
    plot_12_31

# Run 3.1 for the 18 ENVIRONMENTS -  parallel 
    system.time({
      # clear workspace
      rm(list=ls())
      # load everything 
      setwd("C:/Local_R/BiPhD-ABM/May23") # for hp elitebook 
      source('MOD_1_FuncSource.R')
      source('ModelSource.R')
      
      env_func_3_1_par(days = 30, N= 1000, th_forage_flr = 0.2326531, daylight_h = 8, modelType = 31)
      # mark end of simulation
      beep()
      print('Run 18 env 3.1 done')
    })
    
    # Now do an overview image 
    plot_env_18_surv(output_env_func, modelType = 31)
    
    
    
######################################################################
##   Model 3.2: Leftover-hoarding bird, Access to Fat-loss-rate     ##
######################################################################
    
    # Run model 3.2
    system.time({
      # clear workspace
      rm(list=ls())
      # load everything 
      setwd("C:/Local_R/BiPhD-ABM/May23") # for hp elitebook 
      source('MOD_1_FuncSource.R')
      source('ModelSource.R')
      # run the model 
      mod_3_2(days=30, N=1000, env_type=13, th_forage_flr1= -0.06122449, th_forage_flr2= 0.1102041, daylight_h = 8)
      # First put together some relevant dataframes (we want both individual data and mean data)
      save_32_list<-list(total_vars_df32, output_df_list_raw32)
      # save the results 
      setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/MOD_3_2/")
      save(save_32_list, file=paste0(format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_mod_run32', 'd', days, 'N', N,'env_t', env_type, 'th_flr1', th_forage_flr1, 'th_flr2', th_forage_flr2, 'dayh', daylight_h,   '.Rda'))
      # mark end of simulation 
      beep()
      print('single run 3.2 done')
      })
    
    # create plots 
    plots_12_func(inputdata=save_32_list[[1]], modelType='32')
    # if needed 
    plot_12_32
    
    # rUN IT FOR THE 18 ENVIRONMENTS - parallel 
    system.time({
      # clear workspace
      rm(list=ls())
      # load everything 
      setwd("C:/Local_R/BiPhD-ABM/May23") # for hp elitebook 
      source('MOD_1_FuncSource.R')
      source('ModelSource.R')
      # run the model 
      env_func_3_2_par(days = 30, N= 1000, th_forage_flr1 = -0.06122449, th_forage_flr2 = 0.1102041, daylight_h = 8, modelType = 32)
      # mark that simulation is done
      print('18 env MOD 3.2 done')
      beep()
    })
    
    # Now do an overview image 
    plot_env_18_surv(output_env_func, modelType=32)
    
    
####################################################################################
##    Model 3.3.1: Direct hoarding bird, Access to Fat-loss-rate  - hoard top     ##
####################################################################################
    
  # Run model 3.3.1
    system.time({
      # clear workspace
      rm(list=ls())
      # load everything 
      setwd("C:/Local_R/BiPhD-ABM/May23") # for hp elitebook 
      source('MOD_1_FuncSource.R')
      source('ModelSource.R')
      # run the model 
      mod_3_3_1(days=30, N=1000, env_type=8, th_forage_flr1=-0.3, th_forage_flr2=0, th_forage_flr3=0.3, daylight_h=8)
      # First put together some relevant dataframes (we want both individual data and mean data)
      save_331_list<-list(total_vars_df331, output_df_list_raw331)
      # save the results 
      setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/MOD_3_3_1/")
      save( save_331_list, file=paste0(format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_mod_run331', 'd', days, 'N', N,'env_t', env_type, 'th_flr1', th_forage_flr1, 'th_flr2', th_forage_flr2, 'th_flr3', th_forage_flr3, 'dayh', daylight_h,   '.Rda'))
      # mark that the simulation is done
      beep()
      print('run single 3.3.1 done ')
    }) 
    
    # create plots 
    plots_12_func(inputdata= save_331_list[[1]], modelType='331')
    # if needed 
    plot_12_331
    
  # Run 3.3.1 IT FOR THE 18 ENVIRONMENTS 
    system.time({
      # clear workspace
      rm(list=ls())
      # load everything 
      setwd("C:/Local_R/BiPhD-ABM/May23") # for hp elitebook 
      source('MOD_1_FuncSource.R')
      source('ModelSource.R')
      # run the model 
      env_func_3_3_1_par(days = 30, N= 1000, th_forage_flr1 = -0.3, th_forage_flr2 = 0, th_forage_flr3 = 0.3, daylight_h = 8, modelType = 331)
      # mark the end of the simulation 
      beep()
      print('run 18 environments 3.3.1 done ')
    })
    
    # Now do an overview image 
    plot_env_18_surv(output_env_func)
    
####################################################################################
##    Model 3.3.2: Direct hoarding bird, Access to Fat-loss-rate  - rest  top    ##
###################################################################################
    
  # Run model 3.3.2 
    system.time({
      # clear workspace
      rm(list=ls())
      # load everything 
      setwd("C:/Local_R/BiPhD-ABM/May23") # for hp elitebook 
      source('MOD_1_FuncSource.R')
      source('ModelSource.R')
      # run the model 
      mod_3_3_2(days=30, N=1000, env_type=8, th_forage_flr1=-0.3, th_forage_flr2=0, th_forage_flr3=0.3, daylight_h=8)
      # First put together some relevant dataframes (we want both individual data and mean data)
      save_332_list<-list(total_vars_df332, output_df_list_raw332)
      # save the results 
      setwd("C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Model_output/MOD_3_3_2/")
      save(save_332_list, file=paste0(format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_mod_run332', 'd', days, 'N', N,'env_t', env_type, 'th_flr1', th_forage_flr1, 'th_flr2', th_forage_flr2, 'th_flr3', th_forage_flr3, 'dayh', daylight_h,   '.Rda'))
      # mark the end of the simulation 
      beep()
      print('run 3.3.2 single done')
    })
    
    # now plot
    plots_12_func(inputdata = save_332_list[[1]], modelType='332')
    # output
    plot_12_332
    
    # RUN IT FOR THE 18 ENVIRONMENTS 
    system.time({
      # clear workspace
      rm(list=ls())
      # load everything 
      setwd("C:/Local_R/BiPhD-ABM/May23") # for hp elitebook 
      source('MOD_1_FuncSource.R')
      source('ModelSource.R')
      # run the model 
      env_func_3_3_2_par(days = 30, N= 1000, th_forage_flr1 = -0.3, th_forage_flr2 = 0, th_forage_flr3 = 0.3, daylight_h = 8, modelType = 332)
      # mark the end of the simulation 
      beep()
      print('18 env 3.3.2 done')
    })
    
    # Now do an overview image 
    plot_env_18_surv(output_env_func)
    
    
# -----------------------------------------------------------------------------------------------------------------------------
    
    
