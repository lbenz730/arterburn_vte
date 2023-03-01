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
         'upper' = exp(log_hr + 1.96 * std_err),
         'p_value' = 2 * dnorm(-abs(log_hr)/std_err, mean = 0, sd = 1)) %>% 
  mutate('ci_95' = paste0('(', sprintf('%0.2f', lower), ', ',  sprintf('%0.2f', upper), ')')) %>% 
  select(outcome, time, hr, ci_95, p_value) %>% 
  pivot_wider(names_from = 'time',
              values_from = c('hr', 'ci_95', 'p_value')) %>% 
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
         'upper' = exp(log_hr + 1.96 * std_err),
         'p_value' = 2 * dnorm(-abs(log_hr)/std_err, mean = 0, sd = 1)) %>% 
  mutate('ci_95' = paste0('(', sprintf('%0.2f', lower), ', ',  sprintf('%0.2f', upper), ')')) %>% 
  select(outcome, time, hr, ci_95, p_value) %>% 
  pivot_wider(names_from = 'time',
              values_from = c('hr', 'ci_95', 'p_value')) %>% 
  select(outcome, ends_with('1'), ends_with('5')) %>% 
  mutate('outcome' = c('Any VTE', 'PE'))


df_summary_sens <- 
  df_summary_sens %>% 
  inner_join(hr_best_sens, by = 'outcome')


df_table <- 
  bind_rows(
    df_summary %>% mutate('defn' = 'Primary Analysis: VTE Defined Using ICD-9 Codes'),
    df_summary_sens %>% mutate('defn' = 'Sensitivy Analysis: VTE Defined Using ICD-9 Codes and Anticoagulation Prescription Fill')
  )

### Make Table
table_2 <- 
  df_table %>% 
  group_by(defn) %>% 
  gt() %>% 
  cols_align(align = "center", columns = everything()) %>% 
  tab_spanner(columns = ends_with('_1'), label = '1 Year Post Index Date') %>% 
  tab_spanner(columns = ends_with('_5'), label = '5 Years Post Index Date') %>% 
  fmt_number(columns = c('rate'), decimals = 3, sep_mark = '') %>% 
  fmt_number(columns = c('hr_1', 'hr_5'), decimals = 2, sep_mark = '') %>% 
  fmt_number(columns = c('p_value_1', 'p_value_5'), sep_mark = '', n_sigfig = 2) %>% 
  fmt_number(columns = c('person_years', 'n_events'), sep_mark = ',', decimals = 0, drop_trailing_zeros = T) %>% 
  tab_options(column_labels.font.size = 16,
              heading.title.font.size = 20,
              heading.title.font.weight = 'bold',
              heading.subtitle.font.weight = 'bold',
              column_labels.font.weight = 'bold',
              row_group.font.weight = 'bold',
              row_group.font.size  = 12) %>% 
  cols_label('outcome' = '',
             'person_years' = 'Person-Years',
             'n_events' = '# of Events',
             'rate' = 'Rate/100-Pys',
             'hr_1' = 'Adjusted HR',
             'ci_95_1' = '95% CI',
             'p_value_1' = 'p-value',
             'hr_5' = 'Adjusted HR',
             'ci_95_5' = '95% CI',
             'p_value_5' = 'p-value') %>% 
  tab_header(title = 'Table 2: Long-term risk of incident VTE among bariatric surgery patients in relation to severely obese patients who did not undergo bariatric surgery') 
gtsave(table_2, 'figures/tables/table_2.png') 

### Table 3 Event Rates by Surgery Type 
df_event_rate_bs <- 
  bind_rows(select(df_vte, 'time' = time_1, 'event' = status_1, bs_type) %>% mutate(outcome = 'Any VTE'),
            select(df_vte, 'time' = time_3, 'event' = status_3, bs_type) %>% mutate(outcome = 'PE')) %>% 
  mutate('time' = time/365.25)

df_summary_bs <- 
  df_event_rate_bs %>% 
  group_by(outcome, bs_type) %>% 
  summarise('person_years' = sum(time),
            'n_events' = sum(event),
            'rate' = n_events/person_years * 100) %>% 
  ungroup()

table_3 <- 
  df_summary_bs %>% 
  group_by(outcome) %>% 
  gt() %>% 
  cols_align(align = "center", columns = everything()) %>% 
  fmt_number(columns = c('rate'), decimals = 3, sep_mark = '') %>% 
  fmt_number(columns = c('person_years', 'n_events'), sep_mark = ',', decimals = 0, drop_trailing_zeros = T) %>% 
  tab_options(column_labels.font.size = 16,
              heading.title.font.size = 20,
              heading.title.font.weight = 'bold',
              heading.subtitle.font.weight = 'bold',
              column_labels.font.weight = 'bold',
              row_group.font.weight = 'bold',
              row_group.font.size  = 12) %>% 
  cols_label('bs_type' = 'Surgery Group',
             'person_years' = 'Person-Years',
             'n_events' = '# of Events',
             'rate' = 'Rate/100-Pys') %>% 
  tab_header(title = 'Table 3: Event Rates of VTE by Surgery Type vs. Control')
gtsave(table_3, 'figures/tables/table_3.png') 
