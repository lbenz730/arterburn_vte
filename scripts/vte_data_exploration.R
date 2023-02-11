library(tidyverse)
library(haven)

### Read in SAS data file
df_vte <- read_sas('~/Dropbox (Harvard University)/Haneuse/DURABLE/vte_data.sas7bdat')

### Number of matches per surgery
df_vte %>% 
  group_by(case_psid) %>% 
  summarise('n' = n()) %>% 
  View()

### Example of 1 case w/ 3 matches
df_vte %>% 
  filter(case_psid == 10000001) %>% 
  View()


### Example of 1 case w/ 10 matches
df_vte %>% 
  filter(case_psid == 10000050) %>% 
  View()


### Outcomes: 
table(!is.na(df_vte$dvt_date), !is.na(df_vte$pe_date)) ### concurrent disease
table(df_vte$dvt_date > df_vte$pe_date) ### timing of DVT vs. PE
table(!is.na(df_vte$dvt_date), df_vte$anticoag_dvt) ### medications
table(df_vte$dvt_date > df_vte$date_censor_p)

### Missing Data
na_pct <- apply(df_vte, 2, function(x) {mean(is.na(x))})

na_pct[na_pct > 0]


### Censorship for 
table(df_vte$pe_date < df_vte$date_censor)

filter(df_vte, pe_date > date_censor) %>% 
  filter(censor_reason == 'Death') %>% 
  View()
  table()

table(df_vte$censor_reason)


### Data Checks
table(df_vte$pregnancy_date > df_vte$index_date)
table(df_vte$cancer_date > df_vte$index_date)
table(df_vte$eos_date > df_vte$index_date) ### 148 fail (all on index date exactly)
table(df_vte$deathdt > df_vte$index_date)
table(df_vte$enr_end > df_vte$index_date) ### 70 fail (all on index date exactly)
table(df_vte$pe_date > df_vte$index_date)
table(df_vte$dvt_date > df_vte$index_date)
