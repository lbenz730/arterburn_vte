library(tidyverse)
library(glue)
library(gt)

lik_ratio_test <- function(outcome, hte_var) {
  ### Read in Saved Out Model Fit Statistics
  df_main <- read_csv(glue('models/main/outcome_{outcome}_summary.csv'), col_types = cols())
  df_hte <- read_csv(glue('models/hte_{hte_var}/outcome_{outcome}_summary.csv'), col_types = cols())
  
  ### Best Version of Main Model (BIC)
  df_main <- 
    df_main %>% 
    filter(bic == min(bic))
  
  ### Version of This Model for HTE Analysis 
  df_hte <- 
    df_hte %>% 
    filter(model == df_main$model)
  
  ### LRT
  x <- -2 * (df_main$log_lik - df_hte$log_lik)
  p <- df_hte$n_param - df_main$n_param
  
  p_value <- 1 - pchisq(q = x, df = p)
  
  ### Package Results
  df_lrt <- 
    tibble('outcome' = ifelse(outcome == 1, 'Any VTE', 'PE'),
           'variable' = hte_var,
           'n_param' = p,
           'test_stat' = x,
           'p_value' = p_value)
  
  return(df_lrt)
}


df <- 
  crossing('outcome' = c(1,3),
           'analysis' = c('age', 'bmi', 'sex', 'race'))

df_hte <- map_dfr(1:nrow(df), ~lik_ratio_test(df$outcome[.x], df$analysis[.x]))

table_4 <-
  df_hte %>% 
  group_by(outcome) %>% 
  gt() %>% 
  cols_align(align = "center", columns = everything()) %>% 
  fmt_number(columns = c('p_value'), decimals = 3, sep_mark = '') %>% 
  fmt_number(columns = c('test_stat'), decimals = 1, sep_mark = '') %>% 
  tab_options(column_labels.font.size = 16,
              heading.title.font.size = 20,
              heading.title.font.weight = 'bold',
              heading.subtitle.font.weight = 'bold',
              column_labels.font.weight = 'bold',
              row_group.font.weight = 'bold',
              row_group.font.size  = 12) %>% 
  cols_label('variable' = 'Variable',
             'n_param' = '# of Additional Parameters in Interaction Model',
             'test_stat' = 'LR Statistic',
             'p_value' = 'p-value') %>% 
  tab_header(title = 'Table 4: P-Values for Treatment Effect Heterogeneity')
gtsave(table_4, 'figures/tables/table_4.png')  