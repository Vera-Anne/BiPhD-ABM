##########################################
# Small bird in winter - ABM 
# Start date: 15/05/2023
# Vera Vinken 
# Import normal model runs & visualize 
#########################################


# link to the function file 
setwd("C:/Local_R/BiPhD-ABM/Model1_Rcode/")
source('MOD_1_FuncSource.R')

# Retrieve the output you want to analyse 
# The filename can be copied from the folder manually 
retrieve_output_func(modelType = 'MOD_1_1')
# define the file you want to read 
fileName<-'MOD_1_1_out_D30_N1000_eType8_th_sc0.2_DL8_2023-05-15_15_50_39'
# Now load the correct file 
load(paste0(fileName))


#################################
#  CONCATENATE THE DATAFRAMES   # 
#################################

# For each of teh 12 variables that we want the matrices off 
create_df_func(outputFile = outcome_1_1, modelType = '11')
                     
                     
