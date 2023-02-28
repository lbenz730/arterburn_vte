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
                        interaction_labs = NULL) {
  if(!dir.exists(glue('figures/models/{analysis}'))) {
    dir.create(glue('figures/models/{analysis}'))
  }
  
  ### Load Data
  df_vte <- load_vte_data(local = F)
  knots <- read_rds('models/knots.rds')
  
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
                     interaction = interaction))
  
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
                column_labels.font.weight = 'bold',
                row_group.font.weight = 'bold',
                row_group.font.size  = 22) %>% 
    tab_style(style = list(cell_fill(color = "lightblue")), 
              locations = cells_body(rows = best_by_group_aic)) %>% 
    tab_style(style = list(cell_fill(color = "orange")), 
              locations = cells_body(rows = best_by_group_bic)) %>% 
    cols_label('model' = '',
               'n_param' = '# of Parameters',
               'log_lik' = 'Log-Likelihood',
               'bic' = 'BIC',
               'aic' = 'AIC') 
  
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
    scale_y_continuous(breaks = seq(-2, 4, 1), labels = function(x) { sprintf('%0.1f', exp(x)) }) +
    labs(x = 'Time since Index Date (Days)',
         y = 'Surgery HR(t)\n(Log Scale)',
         title = 'Hazard Ratio for Surgery Over Time',
         subtitle = 'First VTE Event',
         col = '',
         fill = '') + 
    scale_color_discrete(labels = interaction_labs) + 
    scale_fill_discrete(labels = interaction_labs) + 
    guides(color = guide_legend(show = interaction),
           fill = guide_legend(show = interaction))
  ggsave(glue('figures/models/{analysis}/01_surgery_log_hr(t)_1.png'), height = 9/1.2, width = 16/1.2)
  
  
  ### (3) PE (w/ or w/out DVT)
  ggplot(df_log_hr[[2]], aes(x = time, y = log_hr)) + 
    facet_wrap(~model) + 
    geom_hline(yintercept = 0, lty = 2, col = 'black') + 
    geom_ribbon(aes(ymin = log_hr - 1.96 * std_err, ymax = log_hr + 1.96 * std_err, fill = as.factor(interaction)), alpha = 0.1) +
    geom_line(aes(col = as.factor(interaction))) + 
    scale_y_continuous(limits = c(-2, 4), breaks = seq(-2, 4, 1), labels = function(x) { sprintf('%0.1f', exp(x)) }) +
    labs(x = 'Time since Index Date (Days)',
         y = 'Surgery HR(t)\n(Log Scale)',
         title = 'Hazard Ratio for Surgery Over Time',
         subtitle = 'PE (w/ or w/out DVT)',   
         col = '',
         fill = '') + 
    scale_color_discrete(labels = interaction_labs) + 
    scale_fill_discrete(labels = interaction_labs) + 
    guides(color = guide_legend(show = interaction),
           fill = guide_legend(show = interaction))
  ggsave(glue('figures/models/{analysis}/02_surgery_log_hr(t)_3.png'), height = 9/1.2, width = 16/1.2)
  
  ### Composite Best Models
  df_best <- 
    bind_rows(df_log_hr) %>% 
    inner_join(df_summary,  by = c("model", "outcome")) %>% 
    group_by(outcome) %>% 
    filter(aic == min(aic)) %>% 
    ungroup()
  
  if(!interaction) {
    ggplot(bind_rows(df_log_hr), aes(x = time, y = log_hr)) + 
      facet_wrap(~model) + 
      geom_hline(yintercept = 0, lty = 2, col = 'black') + 
      geom_ribbon(aes(ymin = log_hr - 1.96 * std_err, ymax = log_hr + 1.96 * std_err, fill = outcome), alpha = 0.1) +
      geom_line(aes(col = outcome)) + 
      scale_y_continuous(breaks = seq(-2, 4, 1), labels = function(x) { sprintf('%0.1f', exp(x)) }) +
      labs(x = 'Time since Index Date (Days)',
           y = 'Surgery HR(t)\n(Log Scale)',
           title = 'Hazard Ratio for Surgery Over Time',
           subtitle = 'Comparison by Outcome',
           fill = 'Outcome',
           color = 'Outcome')
    ggsave(glue('figures/models/{analysis}/03_surgery_log_hr(t)_all.png'), height = 9/1.2, width = 16/1.2)
    
    ggplot(df_best, aes(x = time, y = log_hr)) + 
      geom_hline(yintercept = 0, lty = 2, col = 'black') + 
      geom_ribbon(aes(ymin = log_hr - 1.96 * std_err, ymax = log_hr + 1.96 * std_err, fill = outcome), alpha = 0.1) +
      geom_line(aes(col = outcome)) + 
      scale_y_continuous(breaks = seq(-2, 4, 1), labels = function(x) { sprintf('%0.1f', exp(x)) }) +
      labs(x = 'Time since Index Date (Days)',
           y = 'Surgery HR(t)\n(Log Scale)',
           title = 'Hazard Ratio for Surgery Over Time',
           subtitle = 'Comparison by Outcome',
           fill = 'Outcome',
           color = 'Outcome')
    ggsave(glue('figures/models/{analysis}/04_surgery_log_hr(t)_all.png'), height = 9/1.2, width = 16/1.2)
    
  } else {
    ggplot(df_best , aes(x = time, y = log_hr)) + 
      facet_wrap(~interaction_labs[interaction + 1]) + 
      geom_hline(yintercept = 0, lty = 2, col = 'black') + 
      geom_ribbon(aes(ymin = log_hr - 1.96 * std_err, ymax = log_hr + 1.96 * std_err, fill = outcome), alpha = 0.1) +
      geom_line(aes(col = outcome)) + 
      scale_y_continuous(breaks = seq(-2, 4, 1), labels = function(x) { sprintf('%0.1f', exp(x)) }) +
      labs(x = 'Time since Index Date (Days)',
           y = 'Surgery HR(t)\n(Log Scale)',
           title = 'Hazard Ratio for Surgery Over Time',
           subtitle = 'Comparison by Outcome',
           fill = 'Outcome',
           color = 'Outcome')
    ggsave(glue('figures/models/{analysis}/03_surgery_log_hr(t)_all.png'), height = 9/1.2, width = 16/1.2)
  }
}


model_plots(size = 'small', 
            analysis = 'main')

model_plots(size = 'small',
            analysis = 'hte_bmi',
            interaction = T, 
            interaction_labs = c('BMI < 50', 'BMI >= 50'))
