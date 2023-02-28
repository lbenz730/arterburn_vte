sbatch --array=1,3 jobs/small_models.sh main
sbatch --array=1,3 jobs/small_models.sh sensitivity
sbatch --array=1,3 jobs/small_models.sh hte_bmi
sbatch --array=1,3 jobs/small_models.sh hte_age