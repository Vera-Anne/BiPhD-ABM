

###############
#   MODEL 1   # 
###############


# The environment loop 
# OLD:  
env_func_1_1_old<-function(days, N, th_forage_sc, daylight_h, modelType){
  
  # Loop through the environments 
  for (env in 1:18){
    if (env==1){
      list_means_envs<-list()
    }
    
    mod_1_1(days = days, N = N, env_type = env, th_forage_sc = th_forage_sc, daylight_h = daylight_h)
    
    
    list_means_envs[[env]]<-mean_dfs
    
  }
  
  # print(environment())
  #list_means_envs<<-mget(ls(pattern = "output_means_list11env"))
  
  # now select only the information about survival
  list_means_envs<-lapply(list_means_envs, function(x){x$alive})
  # now find the row with the closest value of survival to 0.5 (halflife)
  halflife_per_env<-lapply(list_means_envs, function(x){x$timestep[which.min(abs(0.5-x$value))]})
  # Same for the end survival
  end_survival_per_env<-lapply(list_means_envs, function(x){x$value[x$timestep==TS]})
  
  # now put relevant data in the global environment
  # assign(paste0('HL_pEnv_th_sc', th_forage_sc), halflife_per_env, envir=.GlobalEnv)
  # assign(paste0('ES_pEnv_th_sc', th_forage_sc), end_survival_per_env, envir=.GlobalEnv)
  # assign(paste0('list_means_per_env_thsc', th_forage_sc), list_means_envs, envir=.GlobalEnv)
  
  # generate the average average end-survival for this threshold, across all the environments 
  mean_ES_cur_th<-mean(unlist(end_survival_per_env))
  # and now for the average time till halflife 
  mean_HL_cur_th<-mean(unlist(halflife_per_env))
  # do the same for the 
  
  
  output_env_func<-cbind(mean_ES_cur_th, mean_HL_cur_th)
  
  assign(paste0('output_env_function_th', th_forage_sc), output_env_func, envir=.GlobalEnv)
  
  return(output_env_func)
  
  # Set the coefficient for plotting the optimisations 
  #assign(total_timesteps, (days*72), envir = .GlobalEnv)
  
} # end environment function loop 

# environment loop for the HPC 
env_func_1_1_hpc<-function(days, N, th_forage_sc, daylight_h, modelType){
  
  num_env<-18 
  
  outcome_env_1_1<- foreach(j=1:num_env, .packages = c( "truncnorm", "purrr")) %do% {
    
    mod_1_1(days = days, N = N, env_type = j, th_forage_sc = th_forage_sc, daylight_h = daylight_h)
    
  }
  
  # Create the variable called halflife_input
  halflife_input<-outcome_env_1_1
  rm(outcome_env_1_1)
  
  # run the t_halflife function 
  t_halflife_func(halflife_input)
  # put the output into a dataframe 
  t_HL_df<-map_dfr(t_HL_list, ~as.data.frame(t(.x)))
  t_HL_df$env<-1:18
  
  # Calculate mean 
  t_HL_mean<-mean(t_HL_df$V1)
  t_HL_SD<-sd(t_HL_df$V1)
  
  
  performance<<-cbind(t_HL_mean, t_HL_SD)
  colnames(performance)<-c('mean', 'SD')
  # New version where I no longer save the specific runs, jus tthe mean and SD to get to optimal results 
  output_env_func<<-list(performance)
  
  # save 
  #setwd(out_dir)
  #save(output_env_func, file=paste0(format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),'_env_func_out_', modelType, 'd', days, 'N', N,'th_sc', th_forage_sc, 'dayh', daylight_h,   '.Rda'))
  # 
  return(output_env_func)
  
  
} # end environment function loop 


# The environment loop 
env_func_1_2_old<-function(days, N, th_forage_sc1, th_forage_sc2, daylight_h, modelType){
  
  # Loop through the environments 
  for (env in 1:18){
    if (env==1){
      list_means_envs<-list()
    }
    
    mod_1_2(days = days, N = N, env_type = env, th_forage_sc1 = th_forage_sc1, th_forage_sc2 = th_forage_sc2, daylight_h = daylight_h)
    
    
    list_means_envs[[env]]<-mean_dfs
    
  }
  
  # print(environment())
  #list_means_envs<<-mget(ls(pattern = "output_means_list11env"))
  
  # now select only the information about survival
  list_means_envs<-lapply(list_means_envs, function(x){x$alive})
  # now find the row with the closest value of survival to 0.5 (halflife)
  halflife_per_env<-lapply(list_means_envs, function(x){x$timestep[which.min(abs(0.5-x$value))]})
  # Same for the end survival
  end_survival_per_env<-lapply(list_means_envs, function(x){x$value[x$timestep==TS]})
  
  # now put relevant data in the global environment
  # assign(paste0('HL_pEnv_th_sc', th_forage_sc), halflife_per_env, envir=.GlobalEnv)
  # assign(paste0('ES_pEnv_th_sc', th_forage_sc), end_survival_per_env, envir=.GlobalEnv)
  # assign(paste0('list_means_per_env_thsc', th_forage_sc), list_means_envs, envir=.GlobalEnv)
  
  # generate the average average end-survival for this threshold, across all the environments 
  mean_ES_cur_th<-mean(unlist(end_survival_per_env))
  # and now for the average time till halflife 
  mean_HL_cur_th<-mean(unlist(halflife_per_env))
  # do the same for the 
  
  
  output_env_func<-cbind(mean_ES_cur_th, mean_HL_cur_th)
  
  assign(paste0('output_env_function_th1=', th_forage_sc1, ' th2=', th_forage_sc2), output_env_func, envir=.GlobalEnv)
  
  return(output_env_func)
  
} # end environment function loop 

# environment loop for the HPC 
env_func_1_2_hpc<-function(days, N, th_forage_sc1, th_forage_sc2, daylight_h, modelType){
  
  num_env<-18 
  
  outcome_env_1_2<- foreach(j=1:num_env, .packages = c( "truncnorm", "purrr")) %do% {
    
    mod_1_2(days = days, N = N, env_type = j, th_forage_sc1 = th_forage_sc1, th_forage_sc2 = th_forage_sc2, daylight_h = daylight_h)
    
  }
  
  # Create the variable called halflife_input
  halflife_input<-outcome_env_1_2
  
  # run the t_halflife function 
  t_halflife_func(halflife_input)
  # put the output into a dataframe 
  t_HL_df<-map_dfr(t_HL_list, ~as.data.frame(t(.x)))
  t_HL_df$env<-1:18
  
  # Calculate mean 
  t_HL_mean<-mean(t_HL_df$V1)
  t_HL_SD<-sd(t_HL_df$V1)
  
  
  performance<<-cbind(t_HL_mean, t_HL_SD)
  colnames(performance)<-c('mean', 'SD')
  output_env_func<<-list(performance, outcome_env_1_2)
  
  return(output_env_func)
  
  
} # end environment function loop for the hpc 


# The environment loop 
env_func_1_3_1_old<-function(days, N, th_forage_sc1, th_forage_sc2, th_forage_sc3, daylight_h, modelType){
  
  # Loop through the environments 
  for (env in 1:18){
    if (env==1){
      list_means_envs<-list()
    }
    
    mod_1_3_1(days = days, N = N, env_type = env, th_forage_sc1 = th_forage_sc1, th_forage_sc2 = th_forage_sc2, th_forage_sc3 = th_forage_sc3, daylight_h = daylight_h)
    
    
    list_means_envs[[env]]<-mean_dfs
    
  }
  
  # print(environment())
  #list_means_envs<<-mget(ls(pattern = "output_means_list11env"))
  
  # now select only the information about survival
  list_means_envs<-lapply(list_means_envs, function(x){x$alive})
  # now find the row with the closest value of survival to 0.5 (halflife)
  halflife_per_env<-lapply(list_means_envs, function(x){x$timestep[which.min(abs(0.5-x$value))]})
  # Same for the end survival
  end_survival_per_env<-lapply(list_means_envs, function(x){x$value[x$timestep==TS]})
  
  # now put relevant data in the global environment
  # assign(paste0('HL_pEnv_th_sc', th_forage_sc), halflife_per_env, envir=.GlobalEnv)
  # assign(paste0('ES_pEnv_th_sc', th_forage_sc), end_survival_per_env, envir=.GlobalEnv)
  # assign(paste0('list_means_per_env_thsc', th_forage_sc), list_means_envs, envir=.GlobalEnv)
  
  # generate the average average end-survival for this threshold, across all the environments 
  mean_ES_cur_th<-mean(unlist(end_survival_per_env))
  # and now for the average time till halflife 
  mean_HL_cur_th<-mean(unlist(halflife_per_env))
  # do the same for the 
  
  
  output_env_func<-cbind(mean_ES_cur_th, mean_HL_cur_th)
  
  assign(paste0('output_env_function_th1=', th_forage_sc1, 'th2=', th_forage_sc2, 'th3=', th_forage_sc3), output_env_func, envir=.GlobalEnv)
  
  return(output_env_func)
  
} # end environment function loop 

# environment loop for the HPC 
env_func_1_3_1_hpc<-function(days, N, th_forage_sc1, th_forage_sc2, th_forage_sc3, daylight_h, modelType){
  
  num_env<-18 
  
  outcome_env_1_3_1<- foreach(j=1:num_env, .packages = c( "truncnorm", "purrr")) %do% {
    
    mod_1_3_1(days = days, N = N, env_type = j, th_forage_sc1 = th_forage_sc1, th_forage_sc2 = th_forage_sc2, th_forage_sc3 = th_forage_sc3, daylight_h = daylight_h)
    
  }
  
  # Create the variable called halflife_input
  halflife_input<-outcome_env_1_3_1
  
  # run the t_halflife function 
  t_halflife_func(halflife_input)
  # put the output into a dataframe 
  t_HL_df<-map_dfr(t_HL_list, ~as.data.frame(t(.x)))
  t_HL_df$env<-1:18
  
  # Calculate mean 
  t_HL_mean<-mean(t_HL_df$V1)
  t_HL_SD<-sd(t_HL_df$V1)
  
  
  performance<<-cbind(t_HL_mean, t_HL_SD)
  colnames(performance)<-c('mean', 'SD')
  output_env_func<<-list(performance, outcome_env_1_3_1)
  
  return(output_env_func)
  
  
} # end environment function loop for the hpc 


#  the enviornment loop OLD 
env_func_1_3_2<-function(days, N, th_forage_sc1, th_forage_sc2, th_forage_sc3, daylight_h, modelType){
  
  # Loop through the environments 
  for (env in 1:18){
    if (env==1){
      list_means_envs<-list()
    }
    
    mod_1_3_2(days = days, N = N, env_type = env, th_forage_sc1 = th_forage_sc1, th_forage_sc2 = th_forage_sc2, th_forage_sc3 = th_forage_sc3, daylight_h = daylight_h)
    
    
    list_means_envs[[env]]<-mean_dfs
    
  }
  
  # print(environment())
  #list_means_envs<<-mget(ls(pattern = "output_means_list11env"))
  
  # now select only the information about survival
  list_means_envs<-lapply(list_means_envs, function(x){x$alive})
  # now find the row with the closest value of survival to 0.5 (halflife)
  halflife_per_env<-lapply(list_means_envs, function(x){x$timestep[which.min(abs(0.5-x$value))]})
  # Same for the end survival
  end_survival_per_env<-lapply(list_means_envs, function(x){x$value[x$timestep==TS]})
  
  # now put relevant data in the global environment
  # assign(paste0('HL_pEnv_th_sc', th_forage_sc), halflife_per_env, envir=.GlobalEnv)
  # assign(paste0('ES_pEnv_th_sc', th_forage_sc), end_survival_per_env, envir=.GlobalEnv)
  # assign(paste0('list_means_per_env_thsc', th_forage_sc), list_means_envs, envir=.GlobalEnv)
  
  # generate the average average end-survival for this threshold, across all the environments 
  mean_ES_cur_th<-mean(unlist(end_survival_per_env))
  # and now for the average time till halflife 
  mean_HL_cur_th<-mean(unlist(halflife_per_env))
  # do the same for the 
  
  
  output_env_func<-cbind(mean_ES_cur_th, mean_HL_cur_th)
  
  assign(paste0('output_env_function_th1=', th_forage_sc1, 'th2=', th_forage_sc2, 'th3=', th_forage_sc3), output_env_func, envir=.GlobalEnv)
  
  return(output_env_func)
  
} # end environment function loop 

# environment loop for the HPC 
env_func_1_3_2_hpc<-function(days, N, th_forage_sc1, th_forage_sc2, th_forage_sc3, daylight_h, modelType){
  
  num_env<-18 
  
  outcome_env_1_3_2<- foreach(j=1:num_env, .packages = c( "truncnorm", "purrr")) %do% {
    
    mod_1_3_2(days = days, N = N, env_type = j, th_forage_sc1 = th_forage_sc1, th_forage_sc2 = th_forage_sc2, th_forage_sc3 = th_forage_sc3, daylight_h = daylight_h)
    
  }
  
  # Create the variable called halflife_input
  halflife_input<-outcome_env_1_3_2
  
  # run the t_halflife function 
  t_halflife_func(halflife_input)
  # put the output into a dataframe 
  t_HL_df<-map_dfr(t_HL_list, ~as.data.frame(t(.x)))
  t_HL_df$env<-1:18
  
  # Calculate mean 
  t_HL_mean<-mean(t_HL_df$V1)
  t_HL_SD<-sd(t_HL_df$V1)
  
  
  performance<<-cbind(t_HL_mean, t_HL_SD)
  colnames(performance)<-c('mean', 'SD')
  output_env_func<<-list(performance, outcome_env_1_3_2)
  
  return(output_env_func)
  
  
} # end environment function loop for the hpc 


###############
#   MODEL 2   # 
###############

# The environment loop 
env_func_2_1<-function(days, N, th_forage_fr, daylight_h, modelType){
  
  # Loop through the environments 
  for (env in 1:18){
    if (env==1){
      list_means_envs<-list()
    }
    
    mod_2_1(days = days, N = N, env_type = env, th_forage_fr = th_forage_fr, daylight_h = daylight_h)
    
    
    list_means_envs[[env]]<-mean_dfs
    
  }
  
  # print(environment())
  #list_means_envs<<-mget(ls(pattern = "output_means_list11env"))
  
  # now select only the information about survival
  list_means_envs<-lapply(list_means_envs, function(x){x$alive})
  # now find the row with the closest value of survival to 0.5 (halflife)
  halflife_per_env<-lapply(list_means_envs, function(x){x$timestep[which.min(abs(0.5-x$value))]})
  # Same for the end survival
  end_survival_per_env<-lapply(list_means_envs, function(x){x$value[x$timestep==TS]})
  
  # now put relevant data in the global environment
  # assign(paste0('HL_pEnv_th_sc', th_forage_sc), halflife_per_env, envir=.GlobalEnv)
  # assign(paste0('ES_pEnv_th_sc', th_forage_sc), end_survival_per_env, envir=.GlobalEnv)
  # assign(paste0('list_means_per_env_thsc', th_forage_sc), list_means_envs, envir=.GlobalEnv)
  
  # generate the average average end-survival for this threshold, across all the environments 
  mean_ES_cur_th<-mean(unlist(end_survival_per_env))
  # and now for the average time till halflife 
  mean_HL_cur_th<-mean(unlist(halflife_per_env))
  # do the same for the 
  
  
  output_env_func<-cbind(mean_ES_cur_th, mean_HL_cur_th)
  
  assign(paste0('output_env_function_th', th_forage_sc), output_env_func, envir=.GlobalEnv)
  
  return(output_env_func)
  
  # Set the coefficient for plotting the optimisations 
  #assign(total_timesteps, (days*72), envir = .GlobalEnv)
  
} # end environment function loop 


###############
#   MODEL 3   # 
###############

