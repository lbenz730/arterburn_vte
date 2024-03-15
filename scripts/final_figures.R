library(tidyverse)
library(haven)
library(glue)
library(survival)
library(patchwork)
library(survminer)
source('scripts/load_data.R')
source('scripts/plot_helpers.R')

### Load data
df_vte <- load_vte_data(local = !file.exists('data/vte_data.sas7bdat'))

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

### Main Figure ####
### (1) VTE KM Curves
p1 <- 
  ggsurvplot(survfit(Surv(time_1/365.25, status_1) ~ surg_cont, data = df_1),
             data = df_1,
             pval = T,
             pval.coord = c(3000/365.25, 0.01),
             conf.int = F, 
             fun = 'event',
             palette = c('orange', 'skyblue'),
             ylim = c(0, 0.06),
             legend.title = "",
             legend = 'bottom',
             legend.labs = c("Control", "Surgey"),
             xlab = 'Time since Index Date (Years)',
             ylab = 'Cumulative Incidence',
             title = 'Kaplan-Meier Cumulative Incidence Curves',
             subtitle = 'Time to First VTE Event',
             ggtheme = 
               theme(plot.title = element_text(hjust = 0.5, size = 24),
                     plot.subtitle = element_text(hjust = 0.5, size = 18),
                     axis.title = element_text(size = 20),
                     axis.text = element_text(size = 12),
                     strip.text = element_text(size = 12),
                     strip.text.y = element_text(size = 8),
                     plot.caption = element_text(size = 10),
                     legend.text = element_text(size = 12))) %>% 
  pluck('plot') +
  scale_x_continuous(breaks = 0:10) + 
  scale_y_continuous(breaks = seq(0, 0.06, 0.01), labels = scales::percent)


p2 <- 
  ggsurvplot(survfit(Surv(time_3/365.25, status_3) ~ surg_cont, data = df_3),
             data = df_3,
             pval = T,
             pval.coord = c(3000/365.25, 0.005),
             conf.int = F,
             fun = 'event',
             ylim = c(0, 0.03),
             palette = c('orange', 'skyblue'),
             legend.title = "",
             legend = 'bottom',
             legend.labs = c("Control", "Surgey"),
             title = 'Kaplan-Meier Cumulative Incidence Curves',
             xlab = 'Time since Index Date (Years)',
             ylab = 'Cumulative Incidence',
             subtitle = 'Time to PE (w/ or w/out DVT)',
             ggtheme =             
               theme(plot.title = element_text(hjust = 0.5, size = 24),
                     plot.subtitle = element_text(hjust = 0.5, size = 18),
                     axis.title = element_text(size = 20),
                     axis.text = element_text(size = 12),
                     strip.text = element_text(size = 12),
                     strip.text.y = element_text(size = 8),
                     plot.caption = element_text(size = 10),
                     legend.text = element_text(size = 12))) %>% 
  pluck('plot') +
  scale_x_continuous(breaks = 0:10) + 
  scale_y_continuous(breaks = seq(0, 0.03, 0.01), labels = scales::percent)


hr_plots <- 
  model_plots_final(size = 'full',
                    analysis = 'main')



(p1 + hr_plots$vte)/(p2 + hr_plots$pe)
ggsave('figures/main_paper_figure.png', height = 9, width = 16)
ggsave('figures/main_paper_figure.tiff', height = 9, width = 16)



### Supplementary Figure ####
df_1 <- 
  df_vte %>% 
  select(time_1_sens, 
         status_1_sens,
         surg_cont,
         all_of(matching_vars), 
         all_of(confounders))

df_3 <- 
  df_vte %>% 
  select(time_3_sens, 
         status_3_sens,
         surg_cont,
         all_of(matching_vars), 
         all_of(confounders))
### (1) VTE KM Curves
p1 <- 
  ggsurvplot(survfit(Surv(time_1_sens/365.25, status_1_sens) ~ surg_cont, data = df_1),
             data = df_1,
             pval = T,
             pval.coord = c(3000/365.25, 0.01),
             conf.int = F, 
             fun = 'event',
             palette = c('orange', 'skyblue'),
             ylim = c(0, 0.04),
             legend.title = "",
             legend = 'bottom',
             legend.labs = c("Control", "Surgey"),
             xlab = 'Time since Index Date (Years)',
             ylab = 'Cumulative Incidence',
             title = 'Kaplan-Meier Cumulative Incidence Curves',
             subtitle = 'Time to First VTE Event',
             ggtheme = 
               theme(plot.title = element_text(hjust = 0.5, size = 24),
                     plot.subtitle = element_text(hjust = 0.5, size = 18),
                     axis.title = element_text(size = 20),
                     axis.text = element_text(size = 12),
                     strip.text = element_text(size = 12),
                     strip.text.y = element_text(size = 8),
                     plot.caption = element_text(size = 10),
                     legend.text = element_text(size = 12))) %>% 
  pluck('plot') +
  scale_x_continuous(breaks = 0:10) + 
  scale_y_continuous(breaks = seq(0, 0.04, 0.01), labels = scales::percent)


p2 <- 
  ggsurvplot(survfit(Surv(time_3_sens/365.25, status_3_sens) ~ surg_cont, data = df_3),
             data = df_3,
             pval = T,
             pval.coord = c(3000/365.25, 0.005),
             conf.int = F,
             fun = 'event',
             ylim = c(0, 0.03),
             palette = c('orange', 'skyblue'),
             legend.title = "",
             legend = 'bottom',
             legend.labs = c("Control", "Surgey"),
             title = 'Kaplan-Meier Cumulative Incidence Curves',
             xlab = 'Time since Index Date (Years)',
             ylab = 'Cumulative Incidence',
             subtitle = 'Time to PE (w/ or w/out DVT)',
             ggtheme =             
               theme(plot.title = element_text(hjust = 0.5, size = 24),
                     plot.subtitle = element_text(hjust = 0.5, size = 18),
                     axis.title = element_text(size = 20),
                     axis.text = element_text(size = 12),
                     strip.text = element_text(size = 12),
                     strip.text.y = element_text(size = 8),
                     plot.caption = element_text(size = 10),
                     legend.text = element_text(size = 12))) %>% 
  pluck('plot') +
  scale_x_continuous(breaks = 0:10) + 
  scale_y_continuous(breaks = seq(0, 0.03, 0.01), labels = scales::percent)


hr_plots <- 
  model_plots_final(size = 'full',
                    analysis = 'sensitivity')



(p1 + hr_plots$vte)/(p2 + hr_plots$pe)
ggsave('figures/supplementary_figure.png', height = 9, width = 16)
ggsave('figures/supplementary_figure.tiff', height = 9, width = 16)
