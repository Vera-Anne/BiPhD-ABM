#!/bin/bash

#SBATCH -A tsabmfhb
#SBATCH -t 00:25:00
#SBATCH --output=/nobackup/proj/tsabmfhb/model_1/model_1_1/output_files/Rscript_%A.out
#SBATCH --error=/nobackup/proj/tsabmfhb/model_1/model_1_1/output_files/Rscript_%A.err
#SBATCH --mail-type=ALL


# This script should take all the matrices in a certain job folder 
# It should then concatenate the ones with similar names (e.g. mat_eat_*) 
# and concatenate them  into big matrices that can be used for analysis 

# SET THE WORKING DIRECTORY HERE 

current_run=$OUTPUT_TO_CONCAT



# step 1: set the working directory to the correct folder (THIS IS WHERE THE SCRIPT NEEDS CHANGING FOR EVERY USE) 
 cd /nobackup/proj/tsabmfhb/model_1/model_1_1/"$current_run"

# step 2: concatenate 
cat mat_eat*.csv > total_mat_eat.csv
cat mat_eat_hoard*.csv > total_mat_eat_hoard.csv
cat mat_forage*.csv > total_mat_forage.csv
cat mat_predated*.csv > total_mat_predated.csv
cat mat_rest*.csv > total_mat_rest.csv
cat mat_retrieved*.csv > total_mat_retrieved.csv
cat mat_sleep*.csv > total_mat_sleep.csv
cat mat_dir_hoard*.csv > total_mat_dir_hoard.csv
cat mat_fat_reserve*.csv > total_fat_reserve.csv
cat mat_stomach_content*.csv > total_stomach_content.csv
cat mat_mass*.csv > total_mass.csv
cat mat_caches*csv > total_caches.csv
cat mat_alive*csv > total_alive.csv
cat mat_predRisk*.csv > total_pred_risk.csv
cat mat_findFood*.csv > total_find_food.csv 

# make directory for the totals 
mkdir /nobackup/proj/tsabmfhb/model_1/model_1_1/"$current_run"/tot_mat

# move the totals to there 
mv total* /nobackup/proj/tsabmfhb/model_1/model_1_1/"$current_run"/tot_mat


