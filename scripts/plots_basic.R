library(tidyverse)
library(haven)
library(survival)
library(survminer)
library(splines)
library(gt)
library(broom)

source('scripts/load_data.R')
source('scripts/plot_helpers.R')

### Load Data
df_vte <- load_vte_data(local = F)

### Define Matching Variables + Confounders
matching_vars <- 
  c('site', 'age', 'bmi', 'GENDER', 'diabetes', 'raceeth', 
    'insulin','elix_cat', 'util_count')

confounders <- 
  c('dysl', 'Smoking2', 'htn_dx', 'ht_index', 'oc_index')

### Define Time variables
time_vars <- 
  c('index_date', 'pregnancy_date', 'cancer_date', 'eos_date',
    'enr_end396', 'deathdt', 'dvt_date', 'pe_date', 'date_censor', 'date_censor_p')

df_1 <- 
  df_vte %>% 
  select(time_1, 
         status_1,
         surg_cont,
         all_of(matching_vars), 
         all_of(confounders))

df_3 <- 
  df_vte %>% 
  select(time_3, 
         status_3,
         surg_cont,
         all_of(matching_vars), 
         all_of(confounders))

### Cox PH Models
model_1_ph <- coxph(Surv(time_1, status_1) ~ ., data = df_1)
model_3_ph <- coxph(Surv(time_3, status_3) ~ ., data = df_3)

### Plots ###
### For each model I fit 4 plots
### 1. KM Curve: All Patients
### 2. KM Curve: Stratified by Surgery Status
### 3. Survival Curve from Cox Model w/ PH for Surgery
### 4. Log[HR(t)] for each model (Plots in plots_models.R)

### (1) Any VTE
ggsurvplot(survfit(Surv(time_1, status_1) ~ 1, data = df_1),
           data = df_1,
           ylim = c(0.925, 1),
           xlab = 'Time since Index Date (Days)',
           title = 'Kaplan-Meier Survival Probability: All Subjects',
           ylab = 'Survival Probability',
           subtitle = 'Time to First VTE Event',
           ggtheme = theme_survminer() + 
             theme(plot.title = element_text(hjust = 0.5),
                   plot.subtitle = element_text(hjust = 0.5)))
ggsave('figures/surv_plots/01_km_all_1.png', height = 9/1.2, width = 16/1.2)

ggsurvplot(survfit(Surv(time_1, status_1) ~ surg_cont, data = df_1),
           data = df_1,
           pval = T,
           pval.coord = c(3000, 0.99),
           conf.int = T,
           ylim = c(0.925, 1),
           legend.title = "",
           legend.labs = c("Control", "Surgey"),
           xlab = 'Time since Index Date (Days)',
           ylab = 'Survival Probability',
           title = 'Kaplan-Meier Survival Probability: Stratifed by Surgery Status',
           subtitle = 'Time to First VTE Event',
           ggtheme = theme_survminer() + 
             theme(plot.title = element_text(hjust = 0.5),
                   plot.subtitle = element_text(hjust = 0.5)))
ggsave('figures/surv_plots/02_km_by_surgery_1.png', height = 9/1.2, width = 16/1.2)

ggadjustedcurves(fit = model_1_ph,
                 method = 'average',
                 variable = 'surg_cont',
                 data = as.data.frame(df_1),
                 ylim = c(0.925, 1),
                 xlab = 'Time since Index Date (Days)',
                 ylab = 'Survival Probability',
                 title = 'Cox-PH Survival Probability: Stratifed by Surgery Status\n(Proportional Hazards for Surgery)',
                 subtitle = 'Time to First VTE Event',
                 ggtheme = theme_survminer() + 
                   theme(plot.title = element_text(hjust = 0.5),
                         plot.subtitle = element_text(hjust = 0.5))) +
  scale_color_discrete(labels = c('Control', 'Surgery'))  +
  labs(color = '')
ggsave('figures/surv_plots/03_survival_ph_by_surgery_1.png', height = 9/1.2, width = 16/1.2)

### (3) PE (w/ or w/out DVT)
ggsurvplot(survfit(Surv(time_3, status_3) ~ 1, data = df_3),
           data = df_3,
           ylim = c(0.925, 1),
           xlab = 'Time since Index Date (Days)',
           title = 'Kaplan-Meier Survival Probability: All Subjects',
           subtitle = 'Time to PE',
           ggtheme = theme_survminer() + 
             theme(plot.title = element_text(hjust = 0.5),
                   plot.subtitle = element_text(hjust = 0.5)))
ggsave('figures/surv_plots/04_km_all_3.png', height = 9/1.2, width = 16/1.2)

ggsurvplot(survfit(Surv(time_3, status_3) ~ surg_cont, data = df_3),
           data = df_3,
           pval = T,
           pval.coord = c(3000, 0.99),
           conf.int = T,
           ylim = c(0.925, 1),
           legend.title = "",
           legend.labs = c("Control", "Surgey"),
           xlab = 'Time since Index Date (Days)',
           title = 'Kaplan-Meier Survival Probability: Stratifed by Surgery Status',
           subtitle = 'Time to PE',
           ggtheme = theme_survminer() + 
             theme(plot.title = element_text(hjust = 0.5),
                   plot.subtitle = element_text(hjust = 0.5)))
ggsave('figures/surv_plots/05_km_by_surgery_3.png', height = 9/1.2, width = 16/1.2)

ggadjustedcurves(fit = model_3_ph,
                 method = 'average',
                 variable = 'surg_cont',
                 data = as.data.frame(df_3),
                 ylim = c(0.925, 1),
                 xlab = 'Time since Index Date (Days)',
                 ylab = 'Survival Probability',
                 title = 'Cox-PH Survival Probability: Stratifed by Surgery Status\n(Proportional Hazards for Surgery)',
                 subtitle = 'Time to PE',
                 ggtheme = theme_survminer() + 
                   theme(plot.title = element_text(hjust = 0.5),
                         plot.subtitle = element_text(hjust = 0.5))) + 
  scale_color_discrete(labels = c('Control', 'Surgery'))  +
  labs(color = '')
ggsave('figures/surv_plots/06_survival_ph_by_surgery_3.png', height = 9/1.2, width = 16/1.2)

