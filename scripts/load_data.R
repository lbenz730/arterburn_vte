library(tidyverse)
library(haven)

load_vte_data  <- function(local = F) {
  ### Read in SAS data file
  if(local) {
    df_vte <- read_sas('~/Dropbox (Harvard University)/Haneuse/DURABLE/vte_data.sas7bdat')
  } else {
    df_vte <- read_sas('data/vte_data.sas7bdat')
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
  
  ### Code Status and time
  ### confirm this is correct
  ### Censor at PE for DVT Only?
  df_vte <- 
    df_vte %>% 
    mutate('surg_cont' = as.numeric(surg_cont == 'Surgery')) %>% 
    mutate('raceeth' = case_when(raceeth == 'Multiple' ~ 'Other',
                                 T ~ raceeth)) %>% 
    mutate('date_censor_v1' = pmin(pregnancy_date, cancer_date, eos_date, enr_end396, deathdt, na.rm = T)) %>% ### 4052 events
    mutate('date_censor_v2' = pmin(pregnancy_date, eos_date, enr_end396, deathdt, na.rm = T)) %>% ### 4636 events 
    mutate('date_censor_v3' = pmin(eos_date, enr_end396, deathdt, na.rm = T)) %>% ### 4692 events
    mutate('status_1' = case_when(!is.na(dvt_date) & dvt_date <= date_censor_v1 ~ 1,
                                  !is.na(pe_date) & pe_date <= date_censor_v1 ~ 1,
                                  is.na(dvt_date) & is.na(pe_date) ~ 0,
                                  !is.na(dvt_date) & dvt_date > date_censor_v1 ~ 0,
                                  !is.na(pe_date) & pe_date > date_censor_v1 ~ 0),
           'status_2' = case_when(!is.na(dvt_date) & dvt_date <= date_censor_v1 & is.na(pe_date) ~ 1,
                                  !is.na(dvt_date) & dvt_date <= date_censor_v1 & !is.na(pe_date) & pe_date > date_censor_v1 ~ 1,
                                  is.na(dvt_date) & is.na(pe_date) ~ 0,
                                  is.na(dvt_date) ~ 0,
                                  !is.na(dvt_date) & dvt_date > date_censor_v1 ~ 0,
                                  !is.na(pe_date) ~ 0),
           'status_3' = case_when(is.na(pe_date) ~ 0,
                                  !is.na(pe_date) & pe_date > date_censor_v1 ~ 0,
                                  !is.na(pe_date) & pe_date <= date_censor_v1 ~ 1)) %>% 
    mutate('time_1' = case_when(status_1 == 0 ~ as.numeric(date_censor_v1 - index_date),
                                status_1 == 1 ~ pmin(as.numeric(dvt_date - index_date),
                                                     as.numeric(pe_date - index_date), na.rm = T)),
           'time_2' = case_when(status_2 == 0 ~ as.numeric(date_censor_v1 - index_date),
                                status_2 == 1 ~ as.numeric(dvt_date - index_date)),
           'time_3' = case_when(status_3 == 0 ~ as.numeric(date_censor_v1 - index_date),
                                status_3 == 1 ~ as.numeric(pe_date - index_date))) 
  
  return(df_vte)
}