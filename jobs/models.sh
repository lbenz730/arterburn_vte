#!/usr/bin/bash

#SBATCH -c 4 ## number of cores
#SBATCH -t 0-08:00 ## amount of time in D-HH:MM
#SBATCH -p fasse_ultramem ## Partition to submit to
#SBATCH --mem=1400000 ## memory pool for all cores
#SBATCH -o logs/log_big.stdout_%a ## STDOUT
#SBATCH -e logs/log_big.stderr_%a ## STDERR
#SBATCH --account=haneuse_lab

module load R/4.2.2-fasrc01
export R_LIBS_USER=$HOME/apps/R_4.2.2:$R_LIBS_USER

cd $HOME/arterburn_vte

Rscript scripts/fit_models.R $SLURM_ARRAY_TASK_ID  $* full
