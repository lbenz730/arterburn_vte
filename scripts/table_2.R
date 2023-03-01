library(tidyverse)
library(gt)
library(glue)
library(splines)
source('scripts/load_data.R')
source('scripts/plot_helpers.R')


### Event Rates 
df_vte <- load_vte_data()
knots <- read_rds('models/knots.rds')

df_event_rate <- 
  bind_rows(select(df_vte, 'time' = time_1, 'event' = status_1) %>% mutate(outcome = 'Any VTE'),
            select(df_vte, 'time' = time_3, 'event' = status_3) %>% mutate(outcome = 'PE')) %>% 
  mutate('time' = time/365.25)

df_summary <-          
  df_event_rate %>% 
  group_by(outcome) %>% 
  summarise('person_years' = sum(time),
            'n_events' = sum(event),
            'rate' = n_events/person_years * 100) %>% 
  ungroup()

df_event_rate_sens <- 
  bind_rows(select(df_vte, 'time' = time_1_sens, 'event' = status_1_sens) %>% mutate(outcome = 'Any VTE'),
            select(df_vte, 'time' = time_3_sens, 'event' = status_3_sens) %>% mutate(outcome = 'PE')) %>% 
  mutate('time' = time/365.25)

df_summary_sens <-          
  df_event_rate_sens %>% 
  group_by(outcome) %>% 
  summarise('person_years' = sum(time),
            'n_events' = sum(event),
            'rate' = n_events/person_years * 100) %>% 
  ungroup()

### Hazard Ratio @ 1 and 5 Years
hr_best <- 
  get_hr_best(size = 'small',
              analysis = 'main',
              eval_times = round(365.25 * c(1, 5)),
              knots = knots) %>% 
  mutate('hr' = exp(log_hr), 
         'lower' = exp(log_hr - 1.96 * std_err),
         'upper' = exp(log_hr + 1.96 * std_err)) %>% 
  mutate('ci_95' = paste0('(', sprintf('%0.2f', lower), ', ',  sprintf('%0.2f', upper), ')')) %>% 
  select(outcome, time, hr, ci_95) %>% 
  pivot_wider(names_from = 'time',
              values_from = c('hr', 'ci_95')) %>% 
  select(outcome, ends_with('1'), ends_with('5')) %>% 
  mutate('outcome' = c('Any VTE', 'PE'))

df_summary <- 
  df_summary %>% 
  inner_join(hr_best, by = 'outcome')

hr_best_sens <- 
  get_hr_best(size = 'small',
              analysis = 'sensitivity',
              eval_times = round(365.25 * c(1, 5)),
              knots = knots) %>% 
  mutate('hr' = exp(log_hr), 
         'lower' = exp(log_hr - 1.96 * std_err),
         'upper' = exp(log_hr + 1.96 * std_err)) %>% 
  mutate('ci_95' = paste0('(', sprintf('%0.2f', lower), ', ',  sprintf('%0.2f', upper), ')')) %>% 
  select(outcome, time, hr, ci_95) %>% 
  pivot_wider(names_from = 'time',
              values_from = c('hr', 'ci_95')) %>% 
  select(outcome, ends_with('1'), ends_with('5')) %>% 
  mutate('outcome' = c('Any VTE', 'PE'))

df_summary_sens <- 
  df_summary_sens %>% 
  inner_join(hr_best, by = 'outcome')


df_table <- 
  bind_rows(
    df_summary %>% mutate('defn' = 'Primary Analysis: VTE Defined Using ICD-9 Codes'),
    df_summary_sens %>% mutate('defn' = 'Sensitivy Analysis: VTE Defined Using ICD-9 Codes and Anticoagulation Prescription Fill')
  )

df_table %>% 
  group_by(defn) %>% 
  gt()
