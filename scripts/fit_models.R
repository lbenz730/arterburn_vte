library(tidyverse)
library(haven)
library(survival)
library(splines)
library(broom)
library(glue)
source('scripts/load_data.R')

### Which Model to Fit
args <- commandArgs(trailingOnly = T)
fit_id <- as.numeric(args[1])
analysis <- args[2]
size <- args[3]
pct_small <- 0.50 ### Fraction of the data to work with when size == 'small'

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

formula_vars <- c('time_1', 'time_1_sens', 'status_1', 'status_1_sens',
                  'time_3', 'time_3_sens', 'status_3', 'status_3_sens')

if(analysis != 'sensitivity') {
  outcome_1 <- c('time_1', 'status_1') 
  outcome_3 <- c('time_3', 'status_3') 
} else {
  outcome_1 <- c('time_1_sens', 'status_1_sens') 
  outcome_3 <- c('time_3_sens', 'status_3_sens') 
}


if(!dir.exists(glue('models/{analysis}'))) {
  dir.create(glue('models/{analysis}'))
}


### Formula's for interactions w/ time
### Primary Analysis
if(analysis == 'main') {
  cat('Main Analysis\n')
  formula_1 <- 
    as.formula(Surv(time_1, status_1) ~ 
                 surg_cont + tt(surg_cont) + 
                 site + age + bmi + GENDER + diabetes + raceeth + insulin + 
                 elix_cat + util_count + dysl + Smoking2 + htn_dx + ht_index + oc_index)
  
  cox_formula_1 <- 
    as.formula(Surv(time_1, status_1) ~ .)
  
  formula_3 <- 
    as.formula(Surv(time_3, status_3) ~ 
                 surg_cont + tt(surg_cont) + 
                 site + age + bmi + GENDER + diabetes + raceeth + insulin + 
                 elix_cat + util_count + dysl + Smoking2 + htn_dx + ht_index + oc_index)
  
  cox_formula_3 <- 
    as.formula(Surv(time_3, status_3) ~ .)
  
} else if(analysis == 'sensitivity') {
  cat('Sensitiviy\n')
  formula_1 <- 
    as.formula(Surv(time_1_sens, status_1_sens) ~ 
                 surg_cont + tt(surg_cont) + 
                 site + age + bmi + GENDER + diabetes + raceeth + insulin + 
                 elix_cat + util_count + dysl + Smoking2 + htn_dx + ht_index + oc_index)
  
  cox_formula_1 <- 
    as.formula(Surv(time_1_sens, status_1_sens) ~ .)
  
  formula_3 <- 
    as.formula(Surv(time_3_sens, status_3_sens) ~ 
                 surg_cont + tt(surg_cont) + 
                 site + age + bmi + GENDER + diabetes + raceeth + insulin + 
                 elix_cat + util_count + dysl + Smoking2 + htn_dx + ht_index + oc_index)
  
  cox_formula_3 <- 
    as.formula(Surv(time_3_sens, status_3_sens) ~ .)
} else if(analysis == 'hte_bmi') {
  cat('HTE BMI Secondary\n')
  confounders <- c(confounders, 'bmi_over_50')
  
  formula_1 <- 
    as.formula(Surv(time_1, status_1) ~ 
                 surg_cont + tt(surg_cont) + 
                 bmi_over_50:surg_cont + bmi_over_50:tt(surg_cont) + 
                 site + age + bmi + GENDER + diabetes + raceeth + insulin + 
                 elix_cat + util_count + dysl + Smoking2 + htn_dx + ht_index + oc_index)
  
  cox_formula_1 <- 
    as.formula(Surv(time_1, status_1) ~ surg_cont + bmi_over_50:surg_cont + .)
  
  formula_3 <- 
    as.formula(Surv(time_3, status_3) ~ 
                 surg_cont + tt(surg_cont) + 
                 bmi_over_50:surg_cont + bmi_over_50:tt(surg_cont) + 
                 site + age + bmi + GENDER + diabetes + raceeth + insulin + 
                 elix_cat + util_count + dysl + Smoking2 + htn_dx + ht_index + oc_index)
  
  cox_formula_3 <- 
    as.formula(Surv(time_3, status_3) ~ surg_cont + bmi_over_50:surg_cont + .)
  
} else if(analysis == 'hte_age') {
  cat('HTE Age Secondary\n')
  confounders <- c(confounders, 'age_over_65')
  
  formula_1 <- 
    as.formula(Surv(time_1, status_1) ~ 
                 surg_cont + tt(surg_cont) + 
                 age_over_65:surg_cont + age_over_65:tt(surg_cont) + 
                 site + age + bmi + GENDER + diabetes + raceeth + insulin + 
                 elix_cat + util_count + dysl + Smoking2 + htn_dx + ht_index + oc_index)
  
  cox_formula_1 <- 
    as.formula(Surv(time_1, status_1) ~ surg_cont + age_over_65:surg_cont + .)
  
  formula_3 <- 
    as.formula(Surv(time_3, status_3) ~ 
                 surg_cont + tt(surg_cont) + 
                 age_over_65:surg_cont + age_over_65:tt(surg_cont) + 
                 site + age + bmi + GENDER + diabetes + raceeth + insulin + 
                 elix_cat + util_count + dysl + Smoking2 + htn_dx + ht_index + oc_index)
  
  cox_formula_3 <- 
    as.formula(Surv(time_3, status_3) ~ surg_cont + age_over_65:surg_cont + .)
  
} else if(analysis == 'hte_sex') {
  cat('HTE Sex Secondary\n')
  formula_1 <- 
    as.formula(Surv(time_1, status_1) ~ 
                 surg_cont + tt(surg_cont) + 
                 GENDER:surg_cont + GENDER:tt(surg_cont) + 
                 site + age + bmi + GENDER + diabetes + raceeth + insulin + 
                 elix_cat + util_count + dysl + Smoking2 + htn_dx + ht_index + oc_index)
  
  cox_formula_1 <- 
    as.formula(Surv(time_1, status_1) ~ surg_cont + GENDER:surg_cont + .)
  
  formula_3 <- 
    as.formula(Surv(time_3, status_3) ~ 
                 surg_cont + tt(surg_cont) + 
                 GENDER:surg_cont + GENDER:tt(surg_cont) + 
                 site + age + bmi + GENDER + diabetes + raceeth + insulin + 
                 elix_cat + util_count + dysl + Smoking2 + htn_dx + ht_index + oc_index)
  
  cox_formula_3 <- 
    as.formula(Surv(time_3, status_3) ~ surg_cont + GENDER:surg_cont + .)
} else if(analysis == 'hte_race') {
  cat('HTE Race Secondary\n')
  formula_1 <- 
    as.formula(Surv(time_1, status_1) ~ 
                 surg_cont + tt(surg_cont) + 
                 raceeth:surg_cont + raceeth:tt(surg_cont) + 
                 site + age + bmi + GENDER + diabetes + insulin + raceeth +
                 elix_cat + util_count + dysl + Smoking2 + htn_dx + ht_index + oc_index)
  
  cox_formula_1 <- 
    as.formula(Surv(time_1, status_1) ~ surg_cont + raceeth:surg_cont + .)
  
  formula_3 <- 
    as.formula(Surv(time_3, status_3) ~ 
                 surg_cont + tt(surg_cont) + 
                 raceeth:surg_cont + raceeth:tt(surg_cont) + 
                 site + age + bmi + GENDER + diabetes + insulin + raceeth + 
                 elix_cat + util_count + dysl + Smoking2 + htn_dx + ht_index + oc_index)
  
  cox_formula_3 <- 
    as.formula(Surv(time_3, status_3) ~ surg_cont + raceeth:surg_cont + .)
} else if(analysis == 'white_subgroup') {
  cat('White Subgroup Analysis\n')
  matching_vars <- setdiff(matching_vars, 'raceeth')
  
  formula_1 <- 
    as.formula(Surv(time_1, status_1) ~ 
                 surg_cont + tt(surg_cont) + 
                 site + age + bmi + GENDER + diabetes + insulin + 
                 elix_cat + util_count + dysl + Smoking2 + htn_dx + ht_index + oc_index)
  
  cox_formula_1 <- 
    as.formula(Surv(time_1, status_1) ~ .)
  
  formula_3 <- 
    as.formula(Surv(time_3, status_3) ~ 
                 surg_cont + tt(surg_cont) + 
                 site + age + bmi + GENDER + diabetes + insulin + 
                 elix_cat + util_count + dysl + Smoking2 + htn_dx + ht_index + oc_index)
  
  cox_formula_3 <- 
    as.formula(Surv(time_3, status_3) ~ .) 
} else if(analysis == 'black_subgroup') {
  cat('Black Subgroup Analysis\n')
  matching_vars <- setdiff(matching_vars, 'raceeth')
  
  formula_1 <- 
    as.formula(Surv(time_1, status_1) ~ 
                 surg_cont + tt(surg_cont) + 
                 site + age + bmi + GENDER + diabetes + insulin + 
                 elix_cat + util_count + dysl + Smoking2 + htn_dx + ht_index + oc_index)
  
  cox_formula_1 <- 
    as.formula(Surv(time_1, status_1) ~ .)
  
  formula_3 <- 
    as.formula(Surv(time_3, status_3) ~ 
                 surg_cont + tt(surg_cont) + 
                 site + age + bmi + GENDER + diabetes + insulin + 
                 elix_cat + util_count + dysl + Smoking2 + htn_dx + ht_index + oc_index)
  
  cox_formula_3 <- 
    as.formula(Surv(time_3, status_3) ~ .) 
}

### Model Descriptions
desc <- 
  c('Proportional Hazards', 'Interaction with log(t)', 
    'NCS with df = 1', 'NCS with df = 2', 
    'NCS with df = 3', 'NCS with df = 4',
    'NCS with df = 5', 'NCS with df = 6')

### Knot Definitions
if(analysis != 'sensitivity') {
  times <- 
    df_vte %>% 
    filter(status_1 == 1, surg_cont == 1) %>% 
    pull(time_1)
  
  ### Quantiles on the time scale of when events are happening
  knots <- 
    map(c(2:8), ~{
      probs <- seq(0, 1, length.out = .x + 1)
      probs <- probs[probs > 0 & probs < 1]
      quantile(times, probs)
    })
  names(knots) <- as.character(2:8)
  
  write_rds(knots, 'models/knots.rds')
} else {
  times <- 
    df_vte %>% 
    filter(status_1_sens == 1, surg_cont == 1) %>% 
    pull(time_1)
  
  ### Quantiles on the time scale of when events are happening
  knots <- 
    map(c(2:8), ~{
      probs <- seq(0, 1, length.out = .x + 1)
      probs <- probs[probs > 0 & probs < 1]
      quantile(times, probs)
    })
  names(knots) <- as.character(2:8)
  
  write_rds(knots, 'models/knots_sens.rds') 
}

### For Development Use a sub-sample of the data
if(size == 'small') {
  set.seed(056232)
  df_vte <-
    df_vte %>% 
    group_by(status_1, status_3, surg_cont) %>% 
    slice(sample(1:n(), pct_small * n())) %>% 
    ungroup()
}

### White Subgroup Analysis
if(analysis == 'white_subgroup') {
 df_vte <- 
   df_vte %>% 
   filter(raceeth == 'White')
}

### Black Subgroup Analysis
if(analysis == 'black_subgroup') {
  df_vte <- 
    df_vte %>% 
    filter(raceeth == 'Black')
}

### (1) Any VTE
if(fit_id == 1) {
  df_1 <- 
    df_vte %>% 
    select(all_of(outcome_1),
           surg_cont,
           all_of(matching_vars), 
           all_of(confounders))
  
  cat('PH Model\n')
  model_1_ph <- coxph(cox_formula_1, data = df_1)
  cat('Log(t) Interaction\n')
  model_1_logt <- coxph(formula_1, data = df_1, tt = function(x, t, ...) { x * log(t) })
  gc()
  cat('NS 1\n')
  model_1_ns1 <- coxph(formula_1, data = df_1, tt = function(x, t, ...) { x * ns(t, df = 1) })
  gc()
  cat('NS 2\n')
  model_1_ns2 <- coxph(formula_1, data = df_1, tt = function(x, t, ...) { x * ns(t, df = 2, knots = knots[['2']]) })
  gc()
  cat('NS 3\n')
  model_1_ns3 <- coxph(formula_1, data = df_1, tt = function(x, t, ...) { x * ns(t, df = 3, knots = knots[['3']]) })
  gc()
  cat('NS 4\n')
  model_1_ns4 <- coxph(formula_1, data = df_1, tt = function(x, t, ...) { x * ns(t, df = 4, knots = knots[['4']]) })
  gc()
  cat('NS 5\n')
  model_1_ns5 <- coxph(formula_1, data = df_1, tt = function(x, t, ...) { x * ns(t, df = 5, knots = knots[['5']]) })
  gc()
  cat('NS 6\n')
  model_1_ns6 <- coxph(formula_1, data = df_1, tt = function(x, t, ...) { x * ns(t, df = 6, knots = knots[['6']]) })
  gc()
  
  
  models_1 <- 
    list(model_1_ph, model_1_logt, model_1_ns1, 
         model_1_ns2, model_1_ns3, model_1_ns4,
         model_1_ns5, model_1_ns6)
  
  
  ### Covariance matrices
  cov_1 <- 
    map(models_1, ~{
      Sigma <- .x$var
      colnames(Sigma) <- names(.x$coefficients)
      rownames(Sigma) <- names(.x$coefficients)
      Sigma
    })
  names(cov_1) <- desc
  
  ### Model Summaries
  df_summary_1 <- 
    map2_dfr(models_1, desc, ~{
      tibble('model' = .y,
             'n_param' = length(.x$coefficients),
             'log_lik' = .x$loglik[2],
             'aic' = AIC(.x),
             'bic' = BIC(.x)) 
    }) %>% 
    mutate('outcome' = '(1) Any VTE')
  
  ### Coefficients
  hr_summary_1 <- 
    map2_dfr(models_1, desc, ~{tidy(.x) %>% mutate('model' = .y)}) %>% 
    mutate('outcome' = '(1) Any VTE')
  
  if(size == 'small') {
    write_csv(df_summary_1, glue('models/{analysis}/outcome_1_summary_small.csv'))
    write_csv(hr_summary_1, glue('models/{analysis}/hr_1_summary_small.csv'))
    write_rds(cov_1, glue('models/{analysis}/covariance_1_small.rds'))
  } else {
    write_csv(df_summary_1, glue('models/{analysis}/outcome_1_summary.csv'))
    write_csv(hr_summary_1, glue('models/{analysis}/hr_1_summary.csv'))
    write_rds(cov_1, glue('models/{analysis}/covariance_1.rds'))
  }
} else if(fit_id == 3) {
  ### (3) PE (w/ or w/out DVT)
  df_3 <- 
    df_vte %>% 
    select(all_of(outcome_3),
           surg_cont,
           all_of(matching_vars), 
           all_of(confounders)) 
  
  cat('PH Model\n')
  model_3_ph <- coxph(cox_formula_3, data = df_3)
  gc()
  cat('Log(t) Interaction\n')
  model_3_logt <- coxph(formula_3, data = df_3, tt = function(x, t, ...) { x * log(t) })
  gc()
  cat('NS 1\n')
  model_3_ns1 <- coxph(formula_3, data = df_3, tt = function(x, t, ...) { x * ns(t, df = 1) })
  gc()
  cat('NS 2\n')
  model_3_ns2 <- coxph(formula_3, data = df_3, tt = function(x, t, ...) { x * ns(t, df = 2, knots = knots[['2']]) })
  gc()
  cat('NS 3\n')
  model_3_ns3 <- coxph(formula_3, data = df_3, tt = function(x, t, ...) { x * ns(t, df = 3, knots = knots[['3']]) })
  gc()
  cat('NS 4\n')
  model_3_ns4 <- coxph(formula_3, data = df_3, tt = function(x, t, ...) { x * ns(t, df = 4, knots = knots[['4']]) })
  gc()
  cat('NS 5\n')
  model_3_ns5 <- coxph(formula_3, data = df_3, tt = function(x, t, ...) { x * ns(t, df = 5, knots = knots[['5']]) })
  gc()
  cat('NS 6\n')
  model_3_ns6 <- coxph(formula_3, data = df_3, tt = function(x, t, ...) { x * ns(t, df = 6, knots = knots[['6']]) })
  gc()
  
  models_3 <- 
    list(model_3_ph, model_3_logt, model_3_ns1, 
         model_3_ns2, model_3_ns3, model_3_ns4,
         model_3_ns5, model_3_ns6)
  
  
  ### Covariance matrices
  cov_3 <- 
    map(models_3, ~{
      Sigma <- .x$var
      colnames(Sigma) <- names(.x$coefficients)
      rownames(Sigma) <- names(.x$coefficients)
      Sigma
    })
  names(cov_3) <- desc
  
  ### Model Summaries
  df_summary_3 <- 
    map2_dfr(models_3, desc, ~{
      tibble('model' = .y,
             'n_param' = length(.x$coefficients),
             'log_lik' = .x$loglik[2],
             'aic' = AIC(.x),
             'bic' = BIC(.x)) 
    }) %>% 
    mutate('outcome' = '(3) PE (w/ or w/out DVT)')
  
  ### Coefficients
  hr_summary_3 <- 
    map2_dfr(models_3, desc, ~{tidy(.x) %>% mutate('model' = .y)}) %>% 
    mutate('outcome' = '(3) PE (w/ or w/out DVT)')
  
  
  if(size == 'small') {
    write_csv(df_summary_3, glue('models/{analysis}/outcome_3_summary_small.csv'))
    write_csv(hr_summary_3, glue('models/{analysis}/hr_3_summary_small.csv'))
    write_rds(cov_3, glue('models/{analysis}/covariance_3_small.rds'))
  } else {
    write_csv(df_summary_3, glue('models/{analysis}/outcome_3_summary.csv'))
    write_csv(hr_summary_3, glue('models/{analysis}/hr_3_summary.csv'))
    write_rds(cov_3, glue('models/{analysis}/covariance_3.rds'))
  }
}
