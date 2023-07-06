library(tidyverse)
library(haven)
library(survival)
library(splines)
library(gt)
library(broom)
library(glue)

source('scripts/load_data.R')
source('scripts/plot_helpers.R')


size = 'full'
analysis = 'hte_race'
interaction = T
interaction_labs = c('Non-Hispanic Black', 'Hispanic', 'Other', 'Unknown', 'Non-Hispanic White')
sub_title = 'Secondary Analyis: Treatment Effect Heterogenity by Race/Ethnicity'

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


### Composite Best Models
df_summary <- 
  tibble('model' = c('NCS with df = 5', 'NCS with df = 4'),
         'outcome' = c('(1) Any VTE', '(3) PE (w/ or w/out DVT)'))
df_best <- 
  bind_rows(df_log_hr) %>% 
  inner_join(df_summary,  by = c("model", "outcome")) %>% 
  mutate('outcome' = gsub('^\\(\\d+\\)\\s+', '', outcome))

ggplot(df_best , aes(x = time, y = log_hr)) + 
  facet_wrap(~outcome, scales = 'free_y') + 
  geom_hline(yintercept = 0, lty = 2, col = 'black') + 
  # geom_ribbon(aes(ymin = log_hr - 1.96 * std_err, ymax = log_hr + 1.96 * std_err, fill = as.factor(interaction)), alpha = 0.1) +
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
                     'Comparison by Outcome (Best Models from Main Analysis)', sep = '\n'),
       subtitle = sub_title,
       fill = '',
       color = '')
ggsave('figures/models/hte_race/race_hr_no_CI.png', height = 9/1.2, width = 16/1.2)
