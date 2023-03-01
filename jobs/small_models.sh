#!/usr/bin/bash

#SBATCH -c 4 ## number of cores
#SBATCH -t 0-04:00 ## amount of time in D-HH:MM
#SBATCH -p fasse_bigmem ## Partition to submit to
#SBATCH --mem=490000 ## memory pool for all cores
#SBATCH -o logs/log.stdout_%a ## STDOUT
#SBATCH -e logs/log.stderr_%a ## STDERR
#SBATCH --account=haneuse_lab

module load R/4.1.0-fasrc01
export R_LIBS_USER=$HOME/apps/R_4.1.0:$R_LIBS_USER

cd $HOME/arterburn_vte

Rscript scripts/fit_models.R $SLURM_ARRAY_TASK_ID $* small
