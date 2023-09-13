library(tidyverse)
library(haven)
library(glue)
library(survival)
source('scripts/load_data.R')

df_vte <- load_vte_data(local = !file.exists('data/vte_data.sas7bdat'))

### Follow Up Times
df_vte <- 
  df_vte %>% 
  mutate('followup_event' = as.numeric(censor_reason != 'Study End')) 


km_model <- survfit(Surv(obstime_censor/365.25, followup_event) ~ 1, data = df_vte)
km_model
summary(km_model, times = 5)

km_model_bs <- survfit(Surv(obstime_censor/365.25, followup_event) ~ bs_type, data = df_vte)
summary(km_model_bs, times = 5)$surv

