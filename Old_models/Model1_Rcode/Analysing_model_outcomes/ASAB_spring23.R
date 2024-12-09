###########################################
# producing graphs/results for ASAB spring 
# Start date: 13/03/2023
# Vera Vinken 
###########################################

# packages t5656
library(gridExtra)
library(ggplot2)

###########################################
#          preparing the files            # 
###########################################
# Set main directory 
mainDir<-'C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Figures/5-combi_model'

# get survival - behaviour and fr/sc from model 1.1 (non hoarding - sc )
setwd(paste0(mainDir, '/MOD_1_1/5-beh_loop//')) 
survival_1_1<-read.csv('beh_loop_surv_1_1_df_2023-03-20_18_08_22.csv') # survival data 
beh_1_1<-read.csv('beh_loop_beh_1_1_df_2023-03-20_18_08_23.csv')
sc_fr_1_1<-read.csv('beh_loop_sc_fr_1_1_df_2023-03-20_18_08_23.csv')

# get survival - behaviour and fr/sc from model 1.2 (non hoarding - fr )
setwd(paste0(mainDir, '/MOD_1_2/5-beh_loop//')) 
survival_1_2<-read.csv('beh_loop_surv_1_2_df_2023-03-20_17_29_35.csv') # survival data 
beh_1_2<-read.csv('beh_loop_beh_1_2_df_2023-03-20_17_29_36.csv')
sc_fr_1_2<-read.csv('beh_loop_sc_fr_1_2_df_2023-03-20_17_29_36.csv')

# get survival - behaviour and fr/sc from model 1.3 (hoarding - sc )
setwd(paste0(mainDir, '/MOD_1_3/5-beh_loop//')) 
survival_1_3<-read.csv('beh_loop_surv_1_3_df_2023-03-20_17_39_54.csv') # survival data 6
beh_1_3<-read.csv('beh_loop_beh_1_3_df_2023-03-20_17_39_54.csv')
sc_fr_1_3<-read.csv('beh_loop_sc_fr_1_3_df_2023-03-20_17_39_54.csv')

# MODEL 1.4  - behaviour and fr/sc from model 1.4 (hoarding- FR)
setwd(paste0(mainDir, '/MOD_1_4/5-beh_loop//')) 
survival_1_4<-read.csv('beh_loop_surv_1_4_df_2023-03-20_17_49_11.csv') # survival data 
beh_1_4<-read.csv('beh_loop_beh_1_4_df_2023-03-20_17_49_11.csv')
sc_fr_1_4<-read.csv('beh_loop_sc_fr_1_4_df_2023-03-20_17_49_11.csv')


# Set the colours for each model
model_colours<-c('#168E7C', '#99CCFF', 'darkolivegreen', 'darkolivegreen3', 'tan3', 'tan4')


#####################################################
#   graph comparing survival trajectories 1-2-3-4   #
#####################################################

# Add columns with model marker 
survival_1_1$model<-rep('11', times=length(survival_1_1))
survival_1_2$model<-rep('12', times=length(survival_1_2))
survival_1_3$model<-rep('13', times=length(survival_1_3))
survival_1_4$model<-rep('14', times=length(survival_1_4))

# Bind together
survival_all<-rbind(survival_1_1, survival_1_2, survival_1_3, survival_1_4)

# koop through environments and make a plot 
for (i in 1:18){
  if (i==1){
    plot_list_survival<<-list()
  }
    # subset an environment 
    cur_subset<<-survival_all[ which (survival_all$env==(paste(i))),]
    # plot
    surv_plot<<-ggplot(cur_subset, aes(x=timestep, y=perc_survival))+
      geom_line(aes(color=model), lwd=1)+
      ggtitle(paste('Survival for environment ', i))+
      ylim(0,100)+ 
      #scale_color_manual(values=model_colours)
      scale_color_brewer(palette='BrBG')+
      # take away legend
      theme(legend.position = "none")+
      xlab('Timestep')+
      ylab('% Alive')
    
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
sc_fr_1_4$model<-rep('14', times=nrow(sc_fr_1_4))

# Bind together
sc_fr_all<-rbind(sc_fr_1_1, sc_fr_1_2, sc_fr_1_3, sc_fr_1_4)

# koop through environments and make a plot 
for (i in 1:18){
  if (i==1){
    plot_list_sc_fr<<-list()
  }
  # subset an environment 
  cur_subset<<-sc_fr_all[ which (sc_fr_all$env==(paste(i)) & sc_fr_all$type=='fr'),]
  # plot
  fat_plot<<-ggplot(cur_subset, aes(x=timesteps_dayscale, y=m))+
    geom_line(aes(color=model), lwd=0.75)+
    ggtitle(paste('FR for environment ', i))+
    ylim(0,5)+
    xlim(0, 24) + # only daylight hours (assuming 8 hour days)
    #scale_color_manual(values=model_colours)
    scale_color_brewer(palette='BrBG')+
    # take away legend
    theme(legend.position = "none")+
    xlab('Timestep')+
    ylab('FR (gram)')
  
  # add plot to list 
  plot_list_sc_fr<<-append(plot_list_sc_fr, list(fat_plot))
  
  } # end of loop for environments 

#ggarrange(plot_list_survival)
dev.new()
do.call('grid.arrange', c(plot_list_sc_fr, ncol=3))



# same but for stomach content 
# loop through environments and make a plot 
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
    ylim(0,0.35)+
    xlim(0,24) + # only daylight hours (assuming 8 hour days)
    #scale_color_manual(values=model_colours)
    scale_color_brewer(palette='BrBG')+
    # take away legend
    theme(legend.position = "none")+
    xlab('Timestep')+
    ylab('SC (gram)')
  
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
beh_1_4$model<-rep('14', times=nrow(beh_1_4))


# Bind together
beh_all<-rbind(beh_1_1, beh_1_2, beh_1_3, beh_1_4)


# loop through environments and make a plot 
for (i in 1:18){
  if (i==1){
    plot_list_beh<<-list()
  }
  # subset an environment 
  cur_subset<<-beh_all[ which (beh_all$env==(paste(i)) & beh_all$beh=='rest'),]
  # plot
  beh_plot<<-ggplot(cur_subset, aes(x=timesteps_dayscale, y=m))+
    geom_line(aes(color=model), lwd=0.75)+
    ggtitle(paste('% Resting environment ', i))+
    ylim(-1,101)+
    xlim(0, 24)+ # only daylight hours (assuming 8 hour days)
    #scale_color_manual(values=model_colours)
    scale_color_brewer(palette='BrBG')+
    # take away legend
    theme(legend.position = "none")+
    xlab('Timestep')+
    ylab('%')
  
  
  # add plot to list 
  plot_list_beh<<-append(plot_list_beh, list(beh_plot))
  
} # end of loop for environments 

#ggarrange(plot_list_survival)
dev.new()
do.call('grid.arrange', c(plot_list_beh, ncol=3))


# Next there needs to be a split between the hoarding and retrieving
# this needs to be corrected in the bheaviour-loop first
# 20/03: done this now (i think)

# subset for only the hoarding modles 



# loop through environments and make a plot 
for (i in 1:18){
  if (i==1){
    plot_list_beh<<-list()
  }
  # subset an environment 
  cur_subset<<-beh_all[ which ((beh_all$model=='13'|beh_all$model=='14') & beh_all$env==(paste(i)) & (beh_all$beh=='eat_hoard' | beh_all$beh=='retrieve')),] #|beh_all$model=='14'
  # plot
  beh_plot<<-ggplot(cur_subset, aes(x=timesteps_dayscale, y=m))+
    geom_line(aes(color=model, lty=beh), lwd=0.75)+
    ggtitle(paste('Hoarding beh. for environment ', i))+
    ylim(-1,101)+
    xlim(0, 24) +# only daylight hours (assuming 8 hour days)
    #scale_color_manual(values=model_colours)+
    scale_color_brewer(palette='Dark2')+
    # take away legend
    theme(legend.position = "none")+
    xlab('Timestep')+
    ylab('%')
  
  # add plot to list 
  plot_list_beh<<-append(plot_list_beh, list(beh_plot))
  
} # end of loop for environments 

#ggarrange(plot_list_survival)
dev.new()
do.call('grid.arrange', c(plot_list_beh, ncol=3))

###################################################################################################

# What do things look like if we only look at the first 3 days 
# this needs to be done in the model code 


# upload the 3-day data 

# Set main directory 
mainDir<-'C:/Users/c0070955/OneDrive - Newcastle University/1-PHD-project/Modelling/R/Figures/5-combi_model'

# get survival - behaviour and fr/sc from model 1.1 (non hoarding - sc )
setwd(paste0(mainDir, '/MOD_1_1/5-beh_loop//')) 
survival_1_1_3d<-read.csv('beh_loop_surv_1_1_df_N1000D3_2023-03-21_09_58_12.csv') # survival data 
beh_1_1_3d<-read.csv('beh_loop_beh_1_1_df_2023-03-21_09_58_12.csv')
sc_fr_1_1_3d<-read.csv('beh_loop_sc_fr_1_1_df_2023-03-21_09_58_12.csv')

# get survival - behaviour and fr/sc from model 1.2 (non hoarding - fr )
setwd(paste0(mainDir, '/MOD_1_2/5-beh_loop//')) 
survival_1_2_3d<-read.csv('beh_loop_surv_1_2_df_2023-03-21_10_02_01.csv') # survival data 
beh_1_2_3d<-read.csv('beh_loop_beh_1_2_df_2023-03-21_10_02_01.csv')
sc_fr_1_2_3d<-read.csv('beh_loop_sc_fr_1_2_df_2023-03-21_10_02_01.csv')

# get survival - behaviour and fr/sc from model 1.3 (hoarding - sc )
setwd(paste0(mainDir, '/MOD_1_3/5-beh_loop//')) 
beh_1_3_3d<-read.csv('beh_loop_beh_1_3_df_2023-03-21_10_10_55.csv')       # behaviour traj
sc_fr_1_3_3d<-read.csv('beh_loop_sc_fr_1_3_df_2023-03-21_10_10_55.csv')   # fr and sc 
survival_1_3_3d<-read.csv('beh_loop_surv_1_3_df_2023-03-21_10_10_55.csv') # survival data 


# MODEL 1.4  - behaviour and fr/sc from model 1.4 (hoarding- FR)
setwd(paste0(mainDir, '/MOD_1_4/5-beh_loop//')) 
beh_1_4_3d<-read.csv('beh_loop_beh_1_4_df_2023-03-21_10_13_07.csv')
sc_fr_1_4_3d<-read.csv('beh_loop_sc_fr_1_4_df_2023-03-21_10_13_07.csv')
survival_1_4_3d<-read.csv('beh_loop_surv_1_4_df_2023-03-21_10_13_07.csv') # survival data 



##################################################################
#   graph comparing survival trajectories 1-2-3-4   3 DAY CODE  #
#################################################################

# Add columns with model marker 
survival_1_1_3d$model<-rep('11', times=nrow(survival_1_1_3d))
survival_1_2_3d$model<-rep('12', times=nrow(survival_1_2_3d))
survival_1_3_3d$model<-rep('13', times=nrow(survival_1_3_3d))
survival_1_4_3d$model<-rep('14', times=nrow(survival_1_4_3d))

# Bind together
survival_all_3d<-rbind(survival_1_1_3d, survival_1_2_3d, survival_1_3_3d, survival_1_4_3d)

# koop through environments and make a plot 
for (i in 1:18){
  if (i==1){
    plot_list_survival<<-list()
  }
  # subset an environment 
  cur_subset<<-survival_all_3d[ which (survival_all_3d$env==(paste(i))),]
  # plot
  surv_plot<<-ggplot(cur_subset, aes(x=timestep, y=perc_survival))+
    geom_line(aes(color=model), lwd=1)+
    ggtitle(paste('Survival 3D for environment ', i))+
    ylim(0,100)+ 
    #scale_color_manual(values=model_colours)
    scale_color_brewer(palette='BrBG')+
    # take away legend
    #theme(legend.position = "none")+
    xlab('Timestep')+
    ylab('% Alive')
  
  # add plot to list 
  plot_list_survival<<-append(plot_list_survival, list(surv_plot))
  
} # end of loop for environments 

#ggarrange(plot_list_survival)
do.call('grid.arrange', c(plot_list_survival, ncol=3))




#####################################################
#   graph comparing fr and sc trajectories 1-2-3-4   #
#####################################################

# Add columns with model marker 
sc_fr_1_1_3d$model<-rep('11', times=nrow(sc_fr_1_1_3d))
sc_fr_1_2_3d$model<-rep('12', times=nrow(sc_fr_1_2_3d))
sc_fr_1_3_3d$model<-rep('13', times=nrow(sc_fr_1_3_3d))
sc_fr_1_4_3d$model<-rep('14', times=nrow(sc_fr_1_4_3d))

# Bind together
sc_fr_all_3d<-rbind(sc_fr_1_1_3d, sc_fr_1_2_3d, sc_fr_1_3_3d, sc_fr_1_4_3d)

# koop through environments and make a plot 
for (i in 1:18){
  if (i==1){
    plot_list_sc_fr<<-list()
  }
  # subset an environment 
  cur_subset<<-sc_fr_all_3d[ which (sc_fr_all_3d$env==(paste(i)) & sc_fr_all_3d$type=='fr'),]
  # plot
  fat_plot<<-ggplot(cur_subset, aes(x=timesteps_dayscale, y=m))+
    geom_line(aes(color=model), lwd=0.75)+
    ggtitle(paste('FR 3D for environment ', i))+
    ylim(0,5)+
    xlim(0, 24) + # only daylight hours (assuming 8 hour days)
    #scale_color_manual(values=model_colours)
    scale_color_brewer(palette='BrBG')+
    # take away legend
    theme(legend.position = "none")+
    xlab('Timestep')+
    ylab('FR (gram)')
  
  # add plot to list 
  plot_list_sc_fr<<-append(plot_list_sc_fr, list(fat_plot))
  
} # end of loop for environments 

#ggarrange(plot_list_survival)
dev.new()
do.call('grid.arrange', c(plot_list_sc_fr, ncol=3))



# same but for stomach content 
# loop through environments and make a plot 
for (i in 1:18){
  if (i==1){
    plot_list_sc<<-list()
  }
  # subset an environment 
  cur_subset<<-sc_fr_all_3d[ which (sc_fr_all_3d$env==(paste(i)) & sc_fr_all_3d$type=='sc'),]
  # plot
  sc_plot<<-ggplot(cur_subset, aes(x=timesteps_dayscale, y=m))+
    geom_line(aes(color=model), lwd=1)+
    ggtitle(paste('SC 3D for environment ', i))+
    ylim(0,0.35)+
    xlim(0,24) + # only daylight hours (assuming 8 hour days)
    #scale_color_manual(values=model_colours)
    scale_color_brewer(palette='BrBG')+
    # take away legend
    theme(legend.position = "none")+
    xlab('Timestep')+
    ylab('SC (gram)')
  
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
beh_1_1_3d$model<-rep('11', times=nrow(beh_1_1_3d))
beh_1_2_3d$model<-rep('12', times=nrow(beh_1_2_3d))
beh_1_3_3d$model<-rep('13', times=nrow(beh_1_3_3d))
beh_1_4_3d$model<-rep('14', times=nrow(beh_1_4_3d))


# Bind together
beh_all_3d<-rbind(beh_1_1_3d, beh_1_2_3d, beh_1_3_3d, beh_1_4_3d)


# loop through environments and make a plot 
for (i in 1:18){
  if (i==1){
    plot_list_beh<<-list()
  }
  # subset an environment 
  cur_subset<<-beh_all_3d[ which (beh_all_3d$env==(paste(i)) & beh_all_3d$beh=='forage'),]
  # plot
  beh_plot<<-ggplot(cur_subset, aes(x=timesteps_dayscale, y=m))+
    geom_line(aes(color=model), lwd=0.75)+
    ggtitle(paste('% 3D foraging environment ', i))+
    ylim(-1,101)+
    xlim(0, 24)+ # only daylight hours (assuming 8 hour days)
    #scale_color_manual(values=model_colours)
    scale_color_brewer(palette='BrBG')+
    # take away legend
    #theme(legend.position = "none")+
    xlab('Timestep')+
    ylab('%')
  
  
  # add plot to list 
  plot_list_beh<<-append(plot_list_beh, list(beh_plot))
  
} # end of loop for environments 

#ggarrange(plot_list_survival)
dev.new()
do.call('grid.arrange', c(plot_list_beh, ncol=3))


# Next there needs to be a split between the hoarding and retrieving
# this needs to be corrected in the bheaviour-loop first
# 20/03: done this now (i think)

# subset for only the hoarding modles 



# loop through environments and make a plot 
for (i in 1:18){
  if (i==1){
    plot_list_beh<<-list()
  }
  # subset an environment 
  cur_subset<<-beh_all_3d[ which ((beh_all_3d$model=='13'|beh_all_3d$model=='14') & beh_all_3d$env==(paste(i)) & (beh_all_3d$beh=='eat_hoard' | beh_all_3d$beh=='retrieve')),] #|beh_all$model=='14'
  # plot
  beh_plot<<-ggplot(cur_subset, aes(x=timesteps_dayscale, y=m))+
    geom_line(aes(color=model, lty=beh), lwd=0.75)+
    ggtitle(paste('BEH for environment ', i))+
    ylim(-1,101)+
    xlim(0, 24) +# only daylight hours (assuming 8 hour days)
    #scale_color_manual(values=model_colours)+
    scale_color_brewer(palette='Dark2')+
    # take away legend
    #theme(legend.position = "none")+
    xlab('Timestep')+
    ylab('%')
  
  # add plot to list 
  plot_list_beh<<-append(plot_list_beh, list(beh_plot))
  
} # end of loop for environments 

#ggarrange(plot_list_survival)
dev.new()
do.call('grid.arrange', c(plot_list_beh, ncol=3))
