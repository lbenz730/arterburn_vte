library(tidyverse)
library(haven)
library(survival)
library(survminer)
library(splines)
library(gt)
library(broom)

source('scripts/load_data.R')
source('scripts/plot_helpers.R')

size <- 'small'

### Load Data
df_vte <- load_vte_data()

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

df_2 <- 
  df_vte %>% 
  select(time_2, 
         status_2,
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
model_2_ph <- coxph(Surv(time_2, status_2) ~ ., data = df_2)
model_3_ph <- coxph(Surv(time_3, status_3) ~ ., data = df_3)

### Model Summaryies
if(size == 'small') {
  files <- dir('models/', full.names = T)
  model_files <- files[grepl('outcome.*_small.csv', files)]
  hr_files <- files[grepl('hr.*_small.csv', files)]
}

df_summary <- 
  map_dfr(model_files, read_csv) %>% 
  group_by(outcome) 

df_hr <- 
  map_dfr(hr_files, read_csv) %>% 
  group_by(outcome) %>% 
  group_split()

### Get Log[HR(t)]
df_log_hr <- 
  map2(df_hr, list(model_1_ph, model_2_ph, model_3_ph), extract_hr)

### Model Fit Summary
best_by_group <- 
  mutate(df_summary, 'best' = aic == min(aic)) %>% 
  pull(best) %>% 
  which(.)

gt_aic <- 
  df_summary %>% 
  gt() %>% 
  cols_align(align = "center", columns = everything()) %>%
  fmt_number(columns = c('log_lik', 'aic'), decimals = 1, sep_mark = '') %>% 
  fmt_number(columns = c('n_param'), decimals = 0, sep_mark = '') %>% 
  tab_style(style = list(cell_borders(sides = "right", color = "black", weight = px(2))),
            locations = list(cells_body(columns = c('model')))) %>% 
  tab_options(column_labels.font.size = 16,
              heading.title.font.size = 40,
              heading.subtitle.font.size = 20,
              column_labels.font.weight = 'bold',
              row_group.font.weight = 'bold',
              row_group.font.size  = 22) %>% 
  tab_style(style = list(cell_fill(color = "lightblue")), 
            locations = cells_body(rows = best_by_group)) %>% 
  cols_label('model' = '',
             'n_param' = '# of Parameters',
             'log_lik' = 'Log-Likelihood',
             'aic' = 'AIC') 

gtsave(gt_aic, 'figures/00_aic_table.png')

### Plots ###
### For each model I fit 4 plots
### 1. KM Curve: All Patients
### 2. KM Curve: Stratified by Surgery Status
### 3. Survival Curve from Cox Model w/ PH for Surgery
### 4. Log[HR(t)] for each model

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
ggsave('figures/01_km_all_1.png', height = 9/1.2, width = 16/1.2)

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
ggsave('figures/02_km_by_surgery_1.png', height = 9/1.2, width = 16/1.2)

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
ggsave('figures/03_survival_ph_by_surgery_1.png', height = 9/1.2, width = 16/1.2)

ggplot(df_log_hr[[1]], aes(x = time, y = log_hr)) + 
  facet_wrap(~model) + 
  geom_hline(yintercept = 0, lty = 2, col = 'black') + 
  geom_line(aes(col = model)) + 
  labs(x = 'Time since Index Date (Days)',
       y = 'Surgery log[HR(t)]',
       title = 'Log Hazard Ratio for Surgery Over Time',
       subtitle = 'First VTE Event') + 
  theme(legend.position = 'none')
ggsave('figures/04_surgery_log_hr(t)_1.png', height = 9/1.2, width = 16/1.2)
         

### (2) DVT (No PE) 
ggsurvplot(survfit(Surv(time_2, status_2) ~ 1, data = df_2),
           data = df_2,
           ylim = c(0.925, 1),
           xlab = 'Time since Index Date (Days)',
           title = 'Kaplan-Meier Survival Probability: All Subjects',
           subtitle = 'Time to DVT',
           ggtheme = theme_survminer() + 
             theme(plot.title = element_text(hjust = 0.5),
                   plot.subtitle = element_text(hjust = 0.5)))
ggsave('figures/05_km_all_2.png', height = 9/1.2, width = 16/1.2)

ggsurvplot(survfit(Surv(time_2, status_2) ~ surg_cont, data = df_2),
           data = df_2,
           pval = T,
           pval.coord = c(3000, 0.99),
           conf.int = T,
           ylim = c(0.925, 1),
           legend.title = "",
           legend.labs = c("Control", "Surgey"),
           xlab = 'Time since Index Date (Days)',
           title = 'Kaplan-Meier Survival Probability: Stratifed by Surgery Status',
           subtitle = 'Time to DVT',
           ggtheme = theme_survminer() + 
             theme(plot.title = element_text(hjust = 0.5),
                   plot.subtitle = element_text(hjust = 0.5)))
ggsave('figures/06_km_by_surgery_2.png', height = 9/1.2, width = 16/1.2)

ggadjustedcurves(fit = model_2_ph,
                 method = 'average',
                 variable = 'surg_cont',
                 data = as.data.frame(df_2),
                 ylim = c(0.925, 1),
                 xlab = 'Time since Index Date (Days)',
                 ylab = 'Survival Probability',
                 title = 'Cox-PH Survival Probability: Stratifed by Surgery Status\n(Proportional Hazards for Surgery)',
                 subtitle = 'Time to First DVT',
                 ggtheme = theme_survminer() + 
                   theme(plot.title = element_text(hjust = 0.5),
                         plot.subtitle = element_text(hjust = 0.5))) + 
  scale_color_discrete(labels = c('Control', 'Surgery'))  +
  labs(color = '')
ggsave('figures/07_survival_ph_by_surgery_2.png', height = 9/1.2, width = 16/1.2)

ggplot(df_log_hr[[2]], aes(x = time, y = log_hr)) + 
  facet_wrap(~model) + 
  geom_hline(yintercept = 0, lty = 2, col = 'black') + 
  geom_line(aes(col = model)) + 
  labs(x = 'Time since Index Date (Days)',
       y = 'Surgery log[HR(t)]',
       title = 'Log Hazard Ratio for Surgery Over Time',
       subtitle = 'DVT (w/out PE)') + 
  theme(legend.position = 'none')
ggsave('figures/08_surgery_log_hr(t)_2.png', height = 9/1.2, width = 16/1.2)

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
ggsave('figures/09_km_all_3.png', height = 9/1.2, width = 16/1.2)

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
ggsave('figures/10_km_by_surgery_3.png', height = 9/1.2, width = 16/1.2)

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
ggsave('figures/11_survival_ph_by_surgery_3.png', height = 9/1.2, width = 16/1.2)

ggplot(df_log_hr[[3]], aes(x = time, y = log_hr)) + 
  facet_wrap(~model) + 
  geom_hline(yintercept = 0, lty = 2, col = 'black') + 
  geom_line(aes(col = model)) + 
  labs(x = 'Time since Index Date (Days)',
       y = 'Surgery log[HR(t)]',
       title = 'Log Hazard Ratio for Surgery Over Time',
       subtitle = 'PE (w/ or w/out DVT)') + 
  theme(legend.position = 'none')
ggsave('figures/12_surgery_log_hr(t)_3.png', height = 9/1.2, width = 16/1.2)


ggplot(bind_rows(df_log_hr), aes(x = time, y = log_hr)) + 
  facet_wrap(~model) + 
  geom_hline(yintercept = 0, lty = 2, col = 'black') + 
  geom_line(aes(col = outcome)) + 
  labs(x = 'Time since Index Date (Days)',
       y = 'Surgery log[HR(t)]',
       title = 'Log Hazard Ratio for Surgery Over Time',
       subtitle = 'Comparison by Outcome',
       color = 'Outcome')
ggsave('figures/13_surgery_log_hr(t)_all.png', height = 9/1.2, width = 16/1.2)

