library(tidyverse)
library(gt)
library(glue)
library(splines)
library(EValue)
source('scripts/load_data.R')
source('scripts/plot_helpers.R')


### Event Rates 
df_vte <- load_vte_data(local = !file.exists('data/vte_data.sas7bdat'))
knots <- read_rds('models/knots.rds')
knots_sens <- read_rds('models/knots_sens.rds')

df_event_rate <- 
  bind_rows(select(df_vte, 'time' = time_1, 'event' = status_1, surg_cont) %>% mutate(outcome = 'Any VTE'),
            select(df_vte, 'time' = time_3, 'event' = status_3, surg_cont) %>% mutate(outcome = 'PE')) %>% 
  mutate('time' = time/365.25)

df_summary <-
  bind_rows(
    
    ### 30 Days Post Index
    df_event_rate %>% 
      mutate('event' = ifelse(time > 30/365.25, 0, event)) %>% 
      mutate('time' = pmin(time, 30/365.25)) %>% 
      group_by(outcome) %>% 
      summarise('time_point' = '30 Days Post Index Date', 
                'person_years_cont' = sum(time[surg_cont == 0]),
                'n_events_cont' = sum(event[surg_cont == 0]),
                'rate_cont' = n_events_cont/person_years_cont * 1000,
                'person_years_surg' = sum(time[surg_cont == 1]),
                'n_events_surg' = sum(event[surg_cont == 1]),
                'rate_surg' = n_events_surg/person_years_surg * 1000
      ),
    
    ### 1 Year Post Index
    df_event_rate %>% 
      mutate('event' = ifelse(time > 1, 0, event)) %>% 
      mutate('time' = pmin(time, 1)) %>% 
      group_by(outcome) %>% 
      summarise('time_point' = '1 Year Post Index Date', 
                'person_years_cont' = sum(time[surg_cont == 0]),
                'n_events_cont' = sum(event[surg_cont == 0]),
                'rate_cont' = n_events_cont/person_years_cont * 1000,
                'person_years_surg' = sum(time[surg_cont == 1]),
                'n_events_surg' = sum(event[surg_cont == 1]),
                'rate_surg' = n_events_surg/person_years_surg * 1000
      ),
    
    ### 5 Year Post Index
    df_event_rate %>% 
      mutate('event' = ifelse(time > 5, 0, event)) %>% 
      mutate('time' = pmin(time, 5)) %>% 
      group_by(outcome) %>% 
      summarise('time_point' = '5 Years Post Index Date', 
                'person_years_cont' = sum(time[surg_cont == 0]),
                'n_events_cont' = sum(event[surg_cont == 0]),
                'rate_cont' = n_events_cont/person_years_cont * 1000,
                'person_years_surg' = sum(time[surg_cont == 1]),
                'n_events_surg' = sum(event[surg_cont == 1]),
                'rate_surg' = n_events_surg/person_years_surg * 1000
      )
  ) %>% 
  arrange(outcome)

### Hazard Ratio @ 1 and 5 Years and 30
hr_best <-
  get_hr_best(size = 'full',
              analysis = 'main',
              eval_times = round(c(30, 365.25 * c(1, 5))),
              knots = knots) %>% 
  mutate('hr' = exp(log_hr), 
         'lower' = exp(log_hr - 1.96 * std_err),
         'upper' = exp(log_hr + 1.96 * std_err),
         'p_value' = 2 * dnorm(-abs(log_hr)/std_err, mean = 0, sd = 1)) %>% 
  mutate('ci_95' = paste0('(', sprintf('%0.2f', lower), ', ',  sprintf('%0.2f', upper), ')')) %>% 
  select(outcome, time, hr, ci_95, p_value) %>% 
  mutate('outcome' = rep(c('Any VTE', 'PE'), each = 3),
         'time_point' = rep(c('30 Days Post Index Date', '1 Year Post Index Date', '5 Years Post Index Date'), 2)) %>% 
  select(outcome, time_point, hr, ci_95, p_value) 

df_summary <- 
  df_summary %>% 
  inner_join(hr_best, by = c('outcome', 'time_point'))

### Make Table
table_2 <-
  df_summary %>% 
  group_by(outcome) %>% 
  gt() %>% 
  cols_align(align = "center", columns = everything()) %>% 
  tab_spanner(columns = ends_with('_cont'), label = 'Non-Surgical Controls') %>% 
  tab_spanner(columns = ends_with('_surg'), label = 'Surgical Patients') %>% 
  fmt_number(columns = contains('rate'), decimals = 1, sep_mark = '') %>% 
  fmt_number(columns = c('hr'), decimals = 2, sep_mark = '') %>% 
  fmt_number(columns = c('p_value'), sep_mark = '', decimals = 3) %>%  
  fmt_number(columns = contains(c('person_years', 'n_events')), sep_mark = ',', decimals = 0, drop_trailing_zeros = T) %>% 
  sub_small_vals(columns = 'p_value', threshold = 0.001) %>% 
  tab_options(column_labels.font.size = 16,
              heading.title.font.size = 20,
              heading.title.font.weight = 'bold',
              heading.subtitle.font.weight = 'bold',
              column_labels.font.weight = 'bold',
              row_group.font.weight = 'bold',
              row_group.font.size  = 12) %>% 
  cols_label('outcome' = '',
             'time_point' = '',
             'person_years_cont' = 'Cumulative Person-Years',
             'n_events_cont' = 'Cumulative # of Events',
             'rate_cont' = 'Rate/1000-Pys',
             'person_years_surg' = 'Cumulative Person-Years',
             'n_events_surg' = 'Cumulative # of Events',
             'rate_surg' = 'Rate/1000-Pys',
             'hr' = 'Adjusted HR',
             'ci_95' = '95% CI',
             'p_value' = 'p-value') %>% 
  tab_header(title = 'Table 2: Long-term risk of incident VTE among bariatric surgery patients in relation to severely obese patients who did not undergo bariatric surgery')  %>% 
  tab_source_note('HR: Hazard Ratio, CI: Confidence Interval') %>% 
  tab_footnote( locations = cells_column_labels(columns = hr),
                footnote = 
                  paste('Adjusted for site of surgery, age, most recent BMI pre-index, sex,',
                        'diabetes status, race/ethnicity, Charlson/Elixhauser comordbity score,', 
                        'insulin use in the prior year, health care utilization in the 7-12 months prior to index date,',
                        'dyslipidemia, hypertension diagnoses, hormone therapy usage at index, use of oral contraceptives at index, and smoking status'))
gtsave(table_2, 'figures/tables/table_2_revised.png', vwidth = 1500, vheight = 800)
gtsave(table_2, 'figures/tables/table_2_revised.docx', vwidth = 1500, vheight = 800)


df_event_rate_sens <- 
  bind_rows(select(df_vte, 'time' = time_1_sens, 'event' = status_1_sens, surg_cont) %>% mutate(outcome = 'Any VTE'),
            select(df_vte, 'time' = time_3_sens, 'event' = status_3_sens, surg_cont) %>% mutate(outcome = 'PE')) %>% 
  mutate('time' = time/365.25)

df_summary_sens <-
  bind_rows(
    
    ### 30 Days Post Index
    df_event_rate_sens %>% 
      mutate('event' = ifelse(time > 30/365.25, 0, event)) %>% 
      mutate('time' = pmin(time, 30/365.25)) %>% 
      group_by(outcome) %>% 
      summarise('time_point' = '30 Days Post Index Date', 
                'person_years_cont' = sum(time[surg_cont == 0]),
                'n_events_cont' = sum(event[surg_cont == 0]),
                'rate_cont' = n_events_cont/person_years_cont * 1000,
                'person_years_surg' = sum(time[surg_cont == 1]),
                'n_events_surg' = sum(event[surg_cont == 1]),
                'rate_surg' = n_events_surg/person_years_surg * 1000
      ),
    
    ### 1 Year Post Index
    df_event_rate_sens %>% 
      mutate('event' = ifelse(time > 1, 0, event)) %>% 
      mutate('time' = pmin(time, 1)) %>% 
      group_by(outcome) %>% 
      summarise('time_point' = '1 Year Post Index Date', 
                'person_years_cont' = sum(time[surg_cont == 0]),
                'n_events_cont' = sum(event[surg_cont == 0]),
                'rate_cont' = n_events_cont/person_years_cont * 1000,
                'person_years_surg' = sum(time[surg_cont == 1]),
                'n_events_surg' = sum(event[surg_cont == 1]),
                'rate_surg' = n_events_surg/person_years_surg * 1000
      ),
    
    ### 5 Year Post Index
    df_event_rate_sens %>% 
      mutate('event' = ifelse(time > 5, 0, event)) %>% 
      mutate('time' = pmin(time, 5)) %>% 
      group_by(outcome) %>% 
      summarise('time_point' = '5 Years Post Index Date', 
                'person_years_cont' = sum(time[surg_cont == 0]),
                'n_events_cont' = sum(event[surg_cont == 0]),
                'rate_cont' = n_events_cont/person_years_cont * 1000,
                'person_years_surg' = sum(time[surg_cont == 1]),
                'n_events_surg' = sum(event[surg_cont == 1]),
                'rate_surg' = n_events_surg/person_years_surg * 1000
      )
  ) %>% 
  arrange(outcome)

### Hazard Ratio @ 1 and 5 Years and 30
hr_best_sens <-
  get_hr_best(size = 'full',
              analysis = 'sensitivity',
              eval_times = round(c(30, 365.25 * c(1, 5))),
              knots = knots) %>% 
  mutate('hr' = exp(log_hr), 
         'lower' = exp(log_hr - 1.96 * std_err),
         'upper' = exp(log_hr + 1.96 * std_err),
         'p_value' = 2 * dnorm(-abs(log_hr)/std_err, mean = 0, sd = 1)) %>% 
  mutate('ci_95' = paste0('(', sprintf('%0.2f', lower), ', ',  sprintf('%0.2f', upper), ')')) %>% 
  select(outcome, time, hr, ci_95, p_value) %>% 
  mutate('outcome' = rep(c('Any VTE', 'PE'), each = 3),
         'time_point' = rep(c('30 Days Post Index Date', '1 Year Post Index Date', '5 Years Post Index Date'), 2)) %>% 
  select(outcome, time_point, hr, ci_95, p_value) 

df_summary_sens <- 
  df_summary_sens %>% 
  inner_join(hr_best_sens, by = c('outcome', 'time_point'))

### Make Table
table_supp_1 <-
  df_summary_sens %>% 
  group_by(outcome) %>% 
  gt() %>% 
  cols_align(align = "center", columns = everything()) %>% 
  tab_spanner(columns = ends_with('_cont'), label = 'Non-Surgical Controls') %>% 
  tab_spanner(columns = ends_with('_surg'), label = 'Surgical Patients') %>% 
  fmt_number(columns = contains('rate'), decimals = 1, sep_mark = '') %>% 
  fmt_number(columns = c('hr'), decimals = 2, sep_mark = '') %>% 
  fmt_number(columns = c('p_value'), sep_mark = '', decimals = 3) %>%  
  fmt_number(columns = contains(c('person_years', 'n_events')), sep_mark = ',', decimals = 0, drop_trailing_zeros = T) %>% 
  sub_small_vals(columns = 'p_value', threshold = 0.001) %>% 
  tab_options(column_labels.font.size = 16,
              heading.title.font.size = 20,
              heading.subtitle.font.size = 18,
              heading.title.font.weight = 'bold',
              heading.subtitle.font.weight = 'bold',
              column_labels.font.weight = 'bold',
              row_group.font.weight = 'bold',
              row_group.font.size  = 12) %>% 
  cols_label('outcome' = '',
             'time_point' = '',
             'person_years_cont' = 'Cumulative Person-Years',
             'n_events_cont' = 'Cumulative # of Events',
             'rate_cont' = 'Rate/1000-Pys',
             'person_years_surg' = 'Cumulative Person-Years',
             'n_events_surg' = 'Cumulative # of Events',
             'rate_surg' = 'Rate/1000-Pys',
             'hr' = 'Adjusted HR',
             'ci_95' = '95% CI',
             'p_value' = 'p-value') %>% 
  tab_header(title = 'Supplementary Table 1: Long-term risk of incident VTE among bariatric surgery patients in relation to severely obese patients who did not undergo bariatric surgery',
             subtitle = 'VTE definied using ICD-9 codes and anticoagulation perscription fill')  %>% 
  tab_source_note('HR: Hazard Ratio, CI: Confidence Interval') %>% 
  tab_footnote( locations = cells_column_labels(columns = hr),
                footnote = 
                  paste('Adjusted for site of surgery, age, most recent BMI pre-index, sex,',
                        'diabetes status, race/ethnicity, Charlson/Elixhauser comordbity score,', 
                        'insulin use in the prior year, health care utilization in the 7-12 months prior to index date,',
                        'dyslipidemia, hypertension diagnoses, hormone therapy usage at index, use of oral contraceptives at index, and smoking status'))
gtsave(table_supp_1, 'figures/tables/table_supp_1.png', vwidth = 1500, vheight = 800)
gtsave(table_supp_1, 'figures/tables/table_supp_1.docx', vwidth = 1500, vheight = 800)


### Evalues
df_hr <- 
  get_hr_best(size = 'full',
              analysis = 'main',
              eval_times = round(c(30, 365.25 * c(1, 5))),
              knots = knots) %>% 
  mutate('hr' = exp(log_hr), 
         'lower' = exp(log_hr - 1.96 * std_err),
         'upper' = exp(log_hr + 1.96 * std_err)) %>% 
  mutate('ci_95' = paste0('(', sprintf('%0.2f', lower), ', ',  sprintf('%0.2f', upper), ')')) %>% 
  mutate('outcome' = rep(c('Any VTE', 'PE'), each = 3),
         'time' = rep(c('30 Days Post Index Date', '1 Year Post Index Date', '5 Years Post Index Date'), 2)) 


df_hr$e_value <- NA
df_hr$e_value_ci <- NA

for(i in 1:nrow(df_hr)) {
  df_evalue <- 
    evalues.HR(est = df_hr$hr[i],
               lo = df_hr$lower[i],
               hi = df_hr$upper[i],
               rare = 1)
  
  df_hr$e_value[i] <- df_evalue['E-values', 'point']
  df_hr$e_value_ci[i] <- min(df_evalue['E-values', -1], na.rm = T)
  
}

table_evalues <-            
  df_hr %>% 
  select(-log_hr, -std_err, -lower, -upper) %>% 
  group_by(outcome) %>% 
  gt() %>% 
  cols_align(align = "center", columns = everything()) %>% 
  fmt_number(columns = c('hr'), decimals = 2, sep_mark = '') %>% 
  fmt_number(columns = contains('e_value'), sep_mark = '', decimals = 2) %>%  
  tab_spanner(columns = contains('e_value'), label = 'E-Values') %>% 
  tab_options(column_labels.font.size = 16,
              heading.title.font.size = 20,
              heading.title.font.weight = 'bold',
              heading.subtitle.font.weight = 'bold',
              column_labels.font.weight = 'bold',
              row_group.font.weight = 'bold',
              row_group.font.size  = 12) %>% 
  cols_label('outcome' = '',
             'time' = '',
             'hr' = 'Adjusted HR',
             'ci_95' = '95% CI',
             'e_value' = 'Point Estimate',
             'e_value_ci' = 'Conf. Interval') %>% 
  tab_footnote( locations = cells_column_labels(columns = hr),
                footnote = 
                  paste('Adjusted for site of surgery, age, most recent BMI pre-index, sex,',
                        'diabetes status, race/ethnicity, Charlson/Elixhauser comordbity score,', 
                        'insulin use in the prior year, health care utilization in the 7-12 months prior to index date,',
                        'dyslipidemia, hypertension diagnoses, hormone therapy usage at index, use of oral contraceptives at index, and smoking status'))

gtsave(table_evalues, 'figures/tables/evalues.png', vwidth = 1500, vheight = 800)
gtsave(table_evalues, 'figures/tables/evalues.docx', vwidth = 1500, vheight = 800)

