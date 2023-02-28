sbatch --array=1,3 jobs/models.sh main
sbatch --array=1,3 jobs/models.sh sensitivity
sbatch --array=1,3 jobs/models.sh hte_bmi
sbatch --array=1,3 jobs/models.sh hte_age