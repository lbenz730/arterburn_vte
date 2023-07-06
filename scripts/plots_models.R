library(tidyverse)
library(haven)
library(survival)
library(splines)
library(gt)
library(broom)
library(glue)

source('scripts/load_data.R')
source('scripts/plot_helpers.R')

model_plots <- function(size, analysis, 
                        interaction = F, 
                        interaction_labs = NULL,
                        sub_title = '') {
  if(!dir.exists(glue('figures/models/{analysis}'))) {
    dir.create(glue('figures/models/{analysis}'))
  }
  
  ### Load Data
  df_vte <- load_vte_data(local = !file.exists('data/vte_data.sas7bdat'))
  if(analysis != 'sensitivity') {
    knots <- read_rds('models/knots.rds')
  } else {
    knots <- read_rds('models/knots_sens.rds')
  }
  
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
  
  
  ### Model Summaries
  if(size == 'small') {
    files <- dir(glue('models/{analysis}'), full.names = T)
    model_files <- files[grepl('outcome.*_small.csv', files)]
    hr_files <- files[grepl('hr.*_small.csv', files)]
    cov_files <- files[grepl('cov.*_small.rds', files)]
  } else {
    files <- files <- dir(glue('models/{analysis}'), full.names = T)
    model_files <- files[grepl('outcome.*\\d+_summary.csv', files)]
    hr_files <- files[grepl('hr.*\\d+_summary.csv', files)]
    cov_files <- files[grepl('cov.*\\d+.rds', files)]
  }
  
  df_summary <- 
    map_dfr(model_files, read_csv) %>% 
    group_by(outcome) 
  
  df_hr <- 
    map_dfr(hr_files, read_csv) %>% 
    group_by(outcome) %>% 
    group_split()
  
  cov_mats <- 
    map(cov_files, read_rds)
  
  ### Get Log[HR(t)]
  df_log_hr <- 
    map2(df_hr, cov_mats, 
         ~extract_hr(hr_summary = .x,
                     cov_mat = .y,
                     knots = knots,
                     interaction = interaction, 
                     n_levels = length(interaction_labs)))
  
  ### Model Fit Summary
  best_by_group_aic <- 
    mutate(df_summary, 'best' = aic == min(aic)) %>% 
    pull(best) %>% 
    which(.)
  
  best_by_group_bic <- 
    mutate(df_summary, 'best' = bic == min(bic)) %>% 
    pull(best) %>% 
    which(.)
  
  gt_summary <- 
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
                heading.title.font.weight = 'bold',
                heading.subtitle.font.weight = 'bold',
                column_labels.font.weight = 'bold',
                row_group.font.weight = 'bold',
                row_group.font.size  = 22) %>% 
    tab_style(style = list(cell_fill(color = "lightblue")), 
              locations = cells_body(rows = setdiff(best_by_group_aic, best_by_group_bic))) %>% 
    tab_style(style = list(cell_fill(color = "red")), 
              locations = cells_body(rows = setdiff(best_by_group_bic, best_by_group_aic))) %>% 
    tab_style(style = list(cell_fill(color = "purple")), 
              locations = cells_body(rows = intersect(best_by_group_bic, best_by_group_aic))) %>% 
    cols_label('model' = '',
               'n_param' = '# of Parameters',
               'log_lik' = 'Log-Likelihood',
               'bic' = 'BIC',
               'aic' = 'AIC') %>% 
    tab_header(title = 'Model Comparison',
               subtitle = sub_title) %>% 
    tab_source_note('Red: Best Model by BIC | Blue: Best Model by AIC | Purple: Best Model by BOTH metrics')
  
  
  gtsave(gt_summary, glue('figures/models/{analysis}/00_model_summary_table.png'))
  
  ### Plots ###
  ### For each model I fit 4 plots (1-3 in plots_basic.R)
  ### 1. KM Curve: All Patients
  ### 2. KM Curve: Stratified by Surgery Status
  ### 3. Survival Curve from Cox Model w/ PH for Surgery
  ### 4. Log[HR(t)] for each model (This script)
  
  ### (1) Any VTE
  ggplot(df_log_hr[[1]], aes(x = time, y = log_hr)) + 
    facet_wrap(~model) + 
    geom_hline(yintercept = 0, lty = 2, col = 'black') + 
    geom_ribbon(aes(ymin = log_hr - 1.96 * std_err, ymax = log_hr + 1.96 * std_err, fill = as.factor(interaction)), alpha = 0.1) +
    geom_line(aes(col = as.factor(interaction))) +
    scale_x_continuous(limits = c(0, 10) * 365.25, 
                       labels = function(x) {round(x/365.25)},
                       breaks = c(0:10) * 365.25) + 
    scale_y_continuous(breaks = seq(-2, 4, 1), labels = function(x) { sprintf('%0.1f', exp(x)) }) +
    scale_color_discrete(labels = interaction_labs) +
    scale_fill_discrete(labels = interaction_labs) +
    labs(x = 'Time since Index Date (Years)',
         y = 'Surgery Hazard Ratio',
         title = 'Hazard Ratio for Surgery Over Time',
         subtitle = sub_title,
         col = '',
         fill = '') +
    theme(legend.position = ifelse(interaction, 'bottom', 'none'))
  ggsave(glue('figures/models/{analysis}/01_surgery_log_hr(t)_1.png'), height = 9/1.2, width = 16/1.2)
  
  
  ### (3) PE (w/ or w/out DVT)
  ggplot(df_log_hr[[2]], aes(x = time, y = log_hr)) + 
    facet_wrap(~model) + 
    geom_hline(yintercept = 0, lty = 2, col = 'black') + 
    geom_ribbon(aes(ymin = log_hr - 1.96 * std_err, ymax = log_hr + 1.96 * std_err, fill = as.factor(interaction)), alpha = 0.1) +
    geom_line(aes(col = as.factor(interaction))) + 
    scale_x_continuous(limits = c(0, 10) * 365.25, 
                       labels = function(x) {round(x/365.25)},
                       breaks = c(0:10) * 365.25) + 
    scale_y_continuous(breaks = seq(-2, 4, 1), labels = function(x) { sprintf('%0.1f', exp(x)) }) +
    scale_color_discrete(labels = interaction_labs) +
    scale_fill_discrete(labels = interaction_labs) +
    labs(x = 'Time since Index Date (Years)',
         y = 'Surgery Hazard Ratio',
         title = 'Hazard Ratio for Surgery Over Time',
         subtitle = sub_title,
         col = '',
         fill = '') +
    theme(legend.position = ifelse(interaction, 'bottom', 'none'))
  ggsave(glue('figures/models/{analysis}/02_surgery_log_hr(t)_3.png'), height = 9/1.2, width = 16/1.2)
  
  ### Composite Best Models
  df_best <- 
    bind_rows(df_log_hr) %>% 
    inner_join(df_summary,  by = c("model", "outcome")) %>% 
    group_by(outcome) %>% 
    filter(bic == min(bic)) %>% 
    ungroup() %>% 
    mutate('outcome' = gsub('^\\(\\d+\\)\\s+', '', outcome))
  
  if(!interaction) {
    ggplot(bind_rows(df_log_hr) %>% mutate('outcome' = gsub('^\\(\\d+\\)\\s+', '', outcome)), 
           aes(x = time, y = log_hr)) + 
      facet_wrap(~model) + 
      geom_hline(yintercept = 0, lty = 2, col = 'black') + 
      geom_ribbon(aes(ymin = log_hr - 1.96 * std_err, ymax = log_hr + 1.96 * std_err, fill = outcome), alpha = 0.1) +
      geom_line(aes(col = outcome)) + 
      scale_x_continuous(limits = c(0, 10) * 365.25, 
                         labels = function(x) {round(x/365.25)},
                         breaks = c(0:10) * 365.25) + 
      scale_y_continuous(breaks = seq(-2, 4, 1), labels = function(x) { sprintf('%0.1f', exp(x)) }) +
      labs(x = 'Time since Index Date (Years)',
           y = 'Surgery Hazard Ratio',
           title = 'Hazard Ratio for Surgery Over Time',
           subtitle = sub_title,
           fill = 'Outcome',
           color = 'Outcome')
    ggsave(glue('figures/models/{analysis}/03_surgery_log_hr(t)_all.png'), height = 9/1.2, width = 16/1.2)
    
    ggplot(df_best, aes(x = time, y = log_hr)) + 
      geom_hline(yintercept = 0, lty = 2, col = 'black') + 
      geom_ribbon(aes(ymin = log_hr - 1.96 * std_err, ymax = log_hr + 1.96 * std_err, fill = outcome), alpha = 0.1) +
      geom_line(aes(col = outcome)) + 
      scale_x_continuous(limits = c(0, 10) * 365.25, 
                         labels = function(x) {round(x/365.25)},
                         breaks = c(0:10) * 365.25) + 
      scale_y_continuous(breaks = seq(-2, 4, 1), labels = function(x) { sprintf('%0.1f', exp(x)) }) +
      labs(x = 'Time since Index Date (Years)',
           y = 'Surgery Hazard Ratio',
           title = 'Hazard Ratio for Surgery Over Time',
           subtitle = sub_title,
           fill = 'Outcome',
           color = 'Outcome')
    ggsave(glue('figures/models/{analysis}/04_surgery_log_hr(t)_best.png'), height = 9/1.2, width = 16/1.2)
    
  } else {
    ggplot(df_best , aes(x = time, y = log_hr)) + 
      facet_wrap(~outcome) + 
      geom_hline(yintercept = 0, lty = 2, col = 'black') + 
      geom_ribbon(aes(ymin = log_hr - 1.96 * std_err, ymax = log_hr + 1.96 * std_err, fill = as.factor(interaction)), alpha = 0.1) +
      geom_line(aes(col = as.factor(interaction))) + 
      scale_x_continuous(limits = c(0, 10) * 365.25, 
                         labels = function(x) {round(x/365.25)},
                         breaks = c(0:10) * 365.25) + 
      scale_y_continuous(breaks = seq(-2, 4, 1), labels = function(x) { sprintf('%0.1f', exp(x)) }) +
      scale_color_discrete(labels = interaction_labs) +
      scale_fill_discrete(labels = interaction_labs) +
      labs(x = 'Time since Index Date (Years)',
           y = 'Surgery Hazard Ratio',
           title = paste('Hazard Ratio for Surgery Over Time',
                         'Comparison by Outcome (Best Models)', sep = '\n'),
           subtitle = sub_title,
           fill = '',
           color = '')
    ggsave(glue('figures/models/{analysis}/03_surgery_log_hr(t)_best.png'), height = 9/1.2, width = 16/1.2)
  }
}

### Main Analysis
model_plots(size = 'full', 
            analysis = 'main', 
            interaction = F,
            sub_title = NULL)

### Sensitivity Analysis
model_plots(size = 'full', 
            analysis = 'sensitivity',
            interaction = F,
            sub_title = 'Sensitivy Analysis: VTE Defined Using ICD-9 Codes and Anticoagulation Prescription Fill')

### Secondary Analyses 
### (BMI)
model_plots(size = 'full',
            analysis = 'hte_bmi',
            interaction = T, 
            interaction_labs = c('BMI < 50', 'BMI >= 50'),
            sub_title = 'Secondary Analyis: Treatment Effect Heterogenity by Baseline BMI')

### (Age) 
model_plots(size = 'full',
            analysis = 'hte_age',
            interaction = T, 
            interaction_labs = c('Age < 65', 'Age >= 65'),
            sub_title = 'Secondary Analyis: Treatment Effect Heterogenity by Baseline Age')

### (Sex) 
model_plots(size = 'full',
            analysis = 'hte_sex',
            interaction = T, 
            interaction_labs = c('Female', 'Male'),
            sub_title = 'Secondary Analyis: Treatment Effect Heterogenity by Sex')

### (Raceeth) 
model_plots(size = 'full',
            analysis = 'hte_race',
            interaction = T, 
            interaction_labs = c('Non-Hispanic Black', 'Hispanic', 'Other', 'Unknown', 'Non-Hispanic White'),
            sub_title = 'Secondary Analyis: Treatment Effect Heterogenity by Race/Ethnicity')

model_plots(size = 'full',
            analysis = 'white_subgroup',
            interaction = F, 
            sub_title = 'Subgroup Analyis: Non-Hispanic White Subpopulation')
