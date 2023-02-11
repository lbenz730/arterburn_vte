library(tidyverse)
library(haven)
library(survival)
library(splines)
library(broom)
source('scripts/load_data.R')

### Which Model to Fit
args <- commandArgs(trailingOnly = T)
fit_id <- as.numeric(args[1])
size <- args[2]

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

### Primary Analysis
### Formula's for interactions w/ time
formula_1 <- 
  as.formula(Surv(time_1, status_1) ~ 
               surg_cont + tt(surg_cont) + 
               site + age + bmi + GENDER + diabetes + raceeth + insulin + 
               elix_cat + util_count + dysl + Smoking2 + htn_dx + ht_index + oc_index)

formula_2 <- 
  as.formula(Surv(time_2, status_2) ~ 
               surg_cont + tt(surg_cont) + 
               site + age + bmi + GENDER + diabetes + raceeth + insulin + 
               elix_cat + util_count + dysl + Smoking2 + htn_dx + ht_index + oc_index)

formula_3 <- 
  as.formula(Surv(time_3, status_3) ~ 
               surg_cont + tt(surg_cont) + 
               site + age + bmi + GENDER + diabetes + raceeth + insulin + 
               elix_cat + util_count + dysl + Smoking2 + htn_dx + ht_index + oc_index)

### Model Descriptions
desc <- 
  c('Proportional Hazards', 'Interaction with log(t)', 
    'NCS with df = 1', 'NCS with df = 2', 
    'NCS with df = 3', 'NCS with df = 6', 
    'NCS with df = 8', 'NCS with df = 10')

### For Development Use a sub-sample of the data
if(size == 'small') {
  set.seed(056232)
  df_vte <-
    df_vte %>% 
    group_by(status_1, status_2, status_3, surg_cont) %>% 
    slice(sample(1:n(), 0.25 * n())) %>% 
    ungroup()
}

### (1) Any VTE
if(fit_id == 1) {
  df_1 <- 
    df_vte %>% 
    select(time_1, 
           status_1,
           surg_cont,
           all_of(matching_vars), 
           all_of(confounders))
  
  cat('PH Model\n')
  model_1_ph <- coxph(Surv(time_1, status_1) ~ ., data = df_1)
  cat('Log(t) Interaction\n')
  model_1_logt <- coxph(formula_1, data = df_1, tt = function(x, t, ...) { x * log(t) })
  gc()
  cat('NS 1\n')
  model_1_ns1 <- coxph(formula_1, data = df_1, tt = function(x, t, ...) { x * ns(t, df = 1) })
  gc()
  cat('NS 2\n')
  model_1_ns2 <- coxph(formula_1, data = df_1, tt = function(x, t, ...) { x * ns(t, df = 2) })
  gc()
  cat('NS 3\n')
  model_1_ns3 <- coxph(formula_1, data = df_1, tt = function(x, t, ...) { x * ns(t, df = 3) })
  gc()
  cat('NS 6\n')
  model_1_ns6 <- coxph(formula_1, data = df_1, tt = function(x, t, ...) { x * ns(t, df = 6) })
  gc()
  cat('NS 8\n')
  model_1_ns8 <- coxph(formula_1, data = df_1, tt = function(x, t, ...) { x * ns(t, df = 8) })
  gc()
  cat('NS 10\n')
  model_1_ns10 <- coxph(formula_1, data = df_1, tt = function(x, t, ...) { x * ns(t, df = 10) })
  gc()
  
  models_1 <- 
    list(model_1_ph, model_1_logt, model_1_ns1, 
         model_1_ns2, model_1_ns3, model_1_ns6,
         model_1_ns8, model_1_ns10)
  
  df_summary_1 <- 
    map2_dfr(models_1, desc, ~{
      tibble('model' = .y,
             'n_param' = length(.x$coefficients),
             'log_lik' = .x$loglik[2],
             'aic' = AIC(.x)) 
    }) %>% 
    mutate('outcome' = '(1) Any VTE')
  
  hr_summary_1 <- 
    map2_dfr(models_1, desc, ~{tidy(.x) %>% mutate('model' = .y)}) %>% 
    mutate('outcome' = '(1) Any VTE')
  
  if(size == 'small') {
    write_csv(df_summary_1, 'models/outcome_1_summary_small.csv')
    write_csv(hr_summary_1, 'models/hr_1_summary_small.csv')
  } else {
    write_csv(df_summary_1, 'models/outcome_1_summary.csv')
    write_csv(hr_summary_1, 'models/hr_1_summary.csv')
  }
  
} else if(fit_id == 2) {
  ### (2) DVT (no PE) 
  df_2 <- 
    df_vte %>% 
    select(time_2, 
           status_2,
           surg_cont,
           all_of(matching_vars), 
           all_of(confounders)) 
  
  cat('PH Model\n')
  model_2_ph <- coxph(Surv(time_2, status_2) ~ ., data = df_2)
  cat('Log(t) Interaction\n')
  model_2_logt <- coxph(formula_2, data = df_2, tt = function(x, t, ...) { x * log(t) })
  gc()
  cat('NS 1\n')
  model_2_ns1 <- coxph(formula_2, data = df_2, tt = function(x, t, ...) { x * ns(t, df = 1) })
  gc()
  cat('NS 2\n')
  model_2_ns2 <- coxph(formula_2, data = df_2, tt = function(x, t, ...) { x * ns(t, df = 2) })
  gc()
  cat('NS 3\n')
  model_2_ns3 <- coxph(formula_2, data = df_2, tt = function(x, t, ...) { x * ns(t, df = 3) })
  gc()
  cat('NS 6\n')
  model_2_ns6 <- coxph(formula_2, data = df_2, tt = function(x, t, ...) { x * ns(t, df = 6) })
  gc()
  cat('NS 8\n')
  model_2_ns8 <- coxph(formula_2, data = df_2, tt = function(x, t, ...) { x * ns(t, df = 8) })
  gc()
  cat('NS 10\n')
  model_2_ns10 <- coxph(formula_2, data = df_2, tt = function(x, t, ...) { x * ns(t, df = 10) })
  gc()
  
  models_2 <- 
    list(model_2_ph, model_2_logt, model_2_ns1, 
         model_2_ns2, model_2_ns3, model_2_ns6, 
         model_2_ns8, model_2_ns10)
  
  df_summary_2 <- 
    map2_dfr(models_2, desc, ~{
      tibble('model' = .y,
             'n_param' = length(.x$coefficients),
             'log_lik' = .x$loglik[2],
             'aic' = AIC(.x)) 
    }) %>% 
    mutate('outcome' = '(2) DVT (no PE)')
  
  hr_summary_2 <- 
    map2_dfr(models_2, desc, ~{tidy(.x) %>% mutate('model' = .y)}) %>% 
    mutate('outcome' = '(2) DVT (no PE)')
  
  if(size == 'small') {
    write_csv(df_summary_2, 'models/outcome_2_summary_small.csv')
    write_csv(hr_summary_2, 'models/hr_2_summary_small.csv')
  } else {
    write_csv(df_summary_2, 'models/outcome_2_summary.csv')
    write_csv(hr_summary_2, 'models/hr_2_summary.csv')
  }
  
} else if(fit_id == 3) {
  ### (3) PE (w/ or w/out DVT)
  df_3 <- 
    df_vte %>% 
    select(time_3, 
           status_3,
           surg_cont,
           all_of(matching_vars), 
           all_of(confounders)) 
  
  cat('PH Model\n')
  model_3_ph <- coxph(Surv(time_3, status_3) ~ ., data = df_3)
  gc()
  cat('Log(t) Interaction\n')
  model_3_logt <- coxph(formula_3, data = df_3, tt = function(x, t, ...) { x * log(t) })
  gc()
  cat('NS 1\n')
  model_3_ns1 <- coxph(formula_3, data = df_3, tt = function(x, t, ...) { x * ns(t, df = 1) })
  gc()
  cat('NS 2\n')
  model_3_ns2 <- coxph(formula_3, data = df_3, tt = function(x, t, ...) { x * ns(t, df = 2) })
  gc()
  cat('NS 3\n')
  model_3_ns3 <- coxph(formula_3, data = df_3, tt = function(x, t, ...) { x * ns(t, df = 3) })
  gc()
  cat('NS 6\n')
  model_3_ns6 <- coxph(formula_3, data = df_3, tt = function(x, t, ...) { x * ns(t, df = 6) })
  gc()
  cat('NS 8\n')
  model_3_ns8 <- coxph(formula_3, data = df_3, tt = function(x, t, ...) { x * ns(t, df = 8) })
  gc()
  cat('NS 10\n')
  model_3_ns10 <- coxph(formula_3, data = df_3, tt = function(x, t, ...) { x * ns(t, df = 10) })
  gc()
  
  models_3 <- 
    list(model_3_ph, model_3_logt, model_3_ns1, 
         model_3_ns2, model_3_ns3, model_3_ns6,
         model_3_ns8, model_3_ns10)
  
  df_summary_3 <- 
    map2_dfr(models_3, desc, ~{
      tibble('model' = .y,
             'n_param' = length(.x$coefficients),
             'log_lik' = .x$loglik[2],
             'aic' = AIC(.x)) 
    }) %>% 
    mutate('outcome' = '(3) PE (w/ or w/out DVT)')
  
  hr_summary_3 <- 
    map2_dfr(models_3, desc, ~{tidy(.x) %>% mutate('model' = .y)}) %>% 
    mutate('outcome' = '(3) PE (w/ or w/out DVT)')
  
  
  if(size == 'small') {
    write_csv(df_summary_3, 'models/outcome_3_summary_small.csv')
    write_csv(hr_summary_3, 'models/hr_3_summary_small.csv')
  } else {
    write_csv(df_summary_3, 'models/outcome_3_summary.csv')
    write_csv(hr_summary_3, 'models/hr_3_summary.csv')
  }
  
}