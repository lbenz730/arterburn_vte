library(tidyverse)
library(haven)
library(glue)
source('scripts/load_data.R')

df_vte <- load_vte_data(local = !file.exists('data/vte_data.sas7bdat'))

### Follow Up Times
df_vte %>%
  group_by(bs_type) %>% 
  summarise('median_follow_up' = median(time_1)/365.25,
            'follow_up_year_5' = mean(time_1 >= 365.25 * 5))
  
