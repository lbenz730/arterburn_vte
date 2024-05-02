# VTE Bariatric Surgery Project
Code for the paper:

Harrington, L., Benz, L., Haneuse, S., Johnson, E., Coleman, K., Courcolas, A., Li, R., Theis, MK., Liu, L., Cooper, J., Chin, P., Grinberg, G., Daigle, C., Chang, J., Um, S., Yenumula, P., Zelada, J., Smith, N., Arterburn, D. ”Bariatric Surgery and the Long-Term Risk of Venous Thromboembolism in a Population-Based Cohort of Severely Obese Adults.” _Obesity Surgery_, 2024. [Link](https://link.springer.com/article/10.1007/s11695-024-07236-y)


# R Scripts (`scripts/`)

* __load_data.R__: Load's in VTE dataset
* __fit_models.R__: Script that fits the various models
* __plot_helpers.R__: Helper functions for plotting, including extracting log hazard ratios and computing standard errors for time varying `surg_cont` variable
* __plots_basic.R__: KM Curves
* __plots_models.R__: Plots of $\log(HR(t))$ for `surg_cont`
* __final_figures.R__: Final plots for the paper
* __table_1.R__: Function to produce table_1 word document (Table 1.docx)
* __table_2.R__: Script to generate table_2 (table_2.png)
* __table_2_revised.R__: Script to generate table_2 (table_2.png)
* __vte_data_exploration.R__: Exploratory analysis
* __img_to_pdf.R__: Function to gather image files and combine them all as PDF
* __hte_likelihood_ratio_tests.R__: Conduct LRT to explore heterogeneous treatment effects
* __extra_numbers.R__: Scratch calculations for some one off numbers needed in our paper.

# Models (`models/`)

* __knots.rds__: .rds file containing knots for splines w/ `df` ranging from 2 to 8. These knots are the on the quantiles of event time scale.
* __main/__: Primary analysis
* __sensitivity/__: Sensitivy analysis (stricter outcome definition)
* __hte_bmi/__: Secondary analysis looking at treatment effect heterogeneity by categories of baseline BMI
* __hte_age/__: Secondary analysis looking at treatment effect heterogeneity by categories of baseline age

# Figures (`figures/`)

* __surv_plots/__: Basic KM Plots
* __main/__: Primary analysis
* __sensitivity/__: Sensitivy analysis (stricter outcome definition)
* __hte_bmi/__: Secondary analysis looking at treatment effect heterogeneity by categories of baseline BMI
* __hte_age/__: Secondary analysis looking at treatment effect heterogeneity by categories of baseline age

# Jobs (`jobs/`)

.sh files to submit batch jobs on the FASSE cluster

# Shell (`shell/`)

.sh files to transfer files to and from the FASSE cluster

# Archive (`archive/`)

Old model objects and figures
