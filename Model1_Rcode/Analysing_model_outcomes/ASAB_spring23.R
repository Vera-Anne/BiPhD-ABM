###########################################
# producing graphs/results for ASAB spring 
# Start date: 13/03/2023
# Vera Vinken 
###########################################

###########################################
#          preparing the files            # 
###########################################
# Set main directory 
mainDir<-'C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Figures/5-combi_model'

# get survival - behaviour and fr/sc from model 1.1 (non hoarding - sc )
setwd(paste0(mainDir, '/MOD_1_1/5-beh_loop//')) 
survival_1_1<-read.csv('beh_loop_surv_1_1_df_2023-03-10_14_43_08.csv') # survival data 
beh_1_1<-read.csv('beh_loop_beh_1_1_df_2023-03-10_14_43_09.csv')
sc_fr_1_1<-read.csv('beh_loop_sc_fr_1_1_df_2023-03-10_14_43_09.csv')

# get survival - behaviour and fr/sc from model 1.2 (non hoarding - fr )
setwd(paste0(mainDir, '/MOD_1_2/5-beh_loop//')) 
survival_1_2<-read.csv('beh_loop_surv_1_2_df_2023-03-12_12_25_31.csv') # survival data 
beh_1_2<-read.csv('beh_loop_beh_1_2_df_2023-03-12_12_25_32.csv')
sc_fr_1_2<-read.csv('beh_loop_sc_fr_1_2_df_2023-03-12_12_25_32.csv')

# get survival - behaviour and fr/sc from model 1.3 (hoarding - sc )
setwd(paste0(mainDir, '/MOD_1_3/5-beh_loop//')) 
survival_1_3<-read.csv('beh_loop_surv_1_3_df_2023-03-13_08_58_36.csv') # survival data 
beh_1_3<-read.csv('beh_loop_beh_1_3_df_2023-03-13_17_24_25.csv')
sc_fr_1_3<-read.csv('beh_loop_sc_fr_1_3_df_2023-03-13_15_09_01.csv')


# MODEL 1.4 IS TBC --> RUN FIRST 



#####################################################
#   graph comparing survival trajectories 1-2-3-4   #
#####################################################

# Add columns with model marker 
survival_1_1$model<-rep('11', times=length(survival_1_1))
survival_1_2$model<-rep('12', times=length(survival_1_2))
survival_1_3$model<-rep('13', times=length(survival_1_3))

# Bind together
survival_all<-rbind(survival_1_1, survival_1_2, survival_1_3)

# koop through environments and make a plot 
for (i in 1:18){
  if (i==1){
    plot_list_survival<<-list()
  }
    # subset an environment 
    cur_subset<<-survival_all[ which (survival_all$env==(paste(i))),]
    # plot
    surv_plot<<-ggplot(cur_subset, aes(x=timestep, y=perc_survival))+
      geom_line(aes(color=model), lwd=1.5)+
      ggtitle(paste('Survival for environment ', i))+
      ylim(0,100)
    
    # add plot to list 
    plot_list_survival<<-append(plot_list_survival, list(surv_plot))
    
} # end of loop for environments 

#ggarrange(plot_list_survival)
do.call('grid.arrange', c(plot_list_survival, ncol=3))




#####################################################
#   graph comparing fr and sc trajectories 1-2-3-4   #
#####################################################

# Add columns with model marker 
sc_fr_1_1$model<-rep('11', times=nrow(sc_fr_1_1))
sc_fr_1_2$model<-rep('12', times=nrow(sc_fr_1_2))
sc_fr_1_3$model<-rep('13', times=nrow(sc_fr_1_3))

# Bind together
sc_fr_all<-rbind(sc_fr_1_1, sc_fr_1_2, sc_fr_1_3)

# koop through environments and make a plot 
for (i in 1:18){
  if (i==1){
    plot_list_sc_fr<<-list()
  }
  # subset an environment 
  cur_subset<<-sc_fr_all[ which (sc_fr_all$env==(paste(i)) & sc_fr_all$type=='fr'),]
  # plot
  fat_plot<<-ggplot(cur_subset, aes(x=timesteps_dayscale, y=m))+
    geom_line(aes(color=model), lwd=1)+
    ggtitle(paste('FR for environment ', i))+
    ylim(0,5)+
    xlim(0, 24) # only daylight hours (assuming 8 hour days)
  
  # add plot to list 
  plot_list_sc_fr<<-append(plot_list_sc_fr, list(fat_plot))
  
  } # end of loop for environments 

#ggarrange(plot_list_survival)
dev.new()
do.call('grid.arrange', c(plot_list_sc_fr, ncol=3))



# same but for stomach content 
# koop through environments and make a plot 
for (i in 1:18){
  if (i==1){
    plot_list_sc<<-list()
  }
  # subset an environment 
  cur_subset<<-sc_fr_all[ which (sc_fr_all$env==(paste(i)) & sc_fr_all$type=='sc'),]
  # plot
  sc_plot<<-ggplot(cur_subset, aes(x=timesteps_dayscale, y=m))+
    geom_line(aes(color=model), lwd=1)+
    ggtitle(paste('SC for environment ', i))+
    ylim(0,0.5)+
    xlim(0,24)# only daylight hours (assuming 8 hour days)
  
  # add plot to list 
  plot_list_sc<<-append(plot_list_sc, list(sc_plot))
  
} # end of loop for environments 

#ggarrange(plot_list_survival)
dev.new()
do.call('grid.arrange', c(plot_list_sc, ncol=3))



#####################################################
#   graph comparing behaviour trajectories 1-2-3-4   #
#####################################################

# To start with, I need a graph that shows the proportion of birds foraging 
# For hoarding birds this includes eat-hoard and retrieving 
# for non-hoarders this includes just eating 

# Add columns with model marker 
beh_1_1$model<-rep('11', times=nrow(beh_1_1))
beh_1_2$model<-rep('12', times=nrow(beh_1_2))
beh_1_3$model<-rep('13', times=nrow(beh_1_3))


# Bind together
beh_all<-rbind(beh_1_1, beh_1_2, beh_1_3)


# loop through environments and make a plot 
for (i in 1:18){
  if (i==1){
    plot_list_beh<<-list()
  }
  # subset an environment 
  cur_subset<<-beh_all[ which (beh_all$env==(paste(i)) & beh_all$beh=='forage'),]
  # plot
  beh_plot<<-ggplot(cur_subset, aes(x=timesteps_dayscale, y=m))+
    geom_line(aes(color=model), lwd=1.25)+
    ggtitle(paste('BEH for environment ', i))+
    ylim(-1,101)+
    xlim(0, 24) # only daylight hours (assuming 8 hour days)
  
  # add plot to list 
  plot_list_beh<<-append(plot_list_beh, list(beh_plot))
  
} # end of loop for environments 

#ggarrange(plot_list_survival)
dev.new()
do.call('grid.arrange', c(plot_list_beh, ncol=3))


# Next there needs to be a split between the hoarding and retrieving
# this needs to be corrected in the bheaviour-loop first




# loop through environments and make a plot 
for (i in 1:18){
  if (i==1){
    plot_list_behH<<-list()
  }
  # subset an environment 
  cur_subset<<-beh_all[ which (beh_all$env==(paste(i)) & beh_all$beh=='forage'),]
  # plot
  beh_plot<<-ggplot(cur_subset, aes(x=timesteps_dayscale, y=m))+
    geom_line(aes(color=model), lwd=1.25)+
    ggtitle(paste('BEH for environment ', i))+
    ylim(-1,101)+
    xlim(0, 24) # only daylight hours (assuming 8 hour days)
  
  # add plot to list 
  plot_list_beh<<-append(plot_list_beh, list(beh_plot))
  
} # end of loop for environments 

#ggarrange(plot_list_survival)
dev.new()
do.call('grid.arrange', c(plot_list_beh, ncol=3))

