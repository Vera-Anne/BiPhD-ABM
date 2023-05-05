#!/bin/bash

#SBATCH -A tsabmfhb
#SBATCH -t 00:05:00
#SBATCH --output=/nobackup/proj/tsabmfhb/model_1/model_1_1/output_files/Rscript_%A_%a.out
#SBATCH --error=/nobackup/proj/tsabmfhb/model_1/model_1_1/output_files/Rscript_%A_%a.err
#SBATCH --mail-type=ALL
#SBATCH --array 1-100 

# load the R module 
module load R


# Tell the program where the R script is located 
R_SCRIPT=/nobackup/proj/tsabmfhb/model_1/model_1_1/MOD_1_1.R

# Now tell it where R can put the output matrices 
OUTPUT_DIR="/nobackup/proj/tsabmfhb/model_1/model_1_1/Rmatrices__$SLURM_ARRAY_JOB_ID"

# And make that directory if needed 
mkdir -p $OUTPUT_DIR

# run the Rscript 
srun Rscript $R_SCRIPT 30 1 8 0.2 2 8 $SLURM_ARRAY_TASK_ID $OUTPUT_DIR

	# argument 1 = number of days in simulation 	
	# argument 2 = number of agents in the simulation 
	# argument 3 = the environment type (1-18) 
	# argument 4 = the stomach content threshold. Forage below this. Default = 0.2
	# argument 5 = the fat reserve threshold. Forage below this (not relevant here). Default = 2
	# argument 6 = number of daylight hours 


