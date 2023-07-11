# sbatch --array=1,3 jobs/small_models.sh main
# sbatch --array=1,3 jobs/small_models.sh sensitivity
# sbatch --array=1,3 jobs/small_models.sh hte_bmi
# sbatch --array=1,3 jobs/small_models.sh hte_age
# sbatch --array=1,3 jobs/small_models.sh hte_sex
# sbatch --array=1,3 jobs/small_models.sh hte_race
# sbatch --array=1,3 jobs/small_models.sh white_subgroup
sbatch --array=1,3 jobs/small_models.sh black_subgroup