library(tidyverse)
library(haven)
library(table1)
library(lubridate)

### Custom table 1 functions to get , after the thousands
render.continuous <- function(x, ...) {
  with(stats.default(x, ...), c("",
                                "Mean (SD)"         = sprintf("%s (%s)",
                                                              signif_pad(MEAN,   3, big.mark=","),
                                                              signif_pad(SD,     3, big.mark=","))))
}

render.categorical <- function(x, ...) {
  c("", sapply(stats.apply.rounding(stats.default(x)), function(y) with(y,
                                                                        sprintf("%s (%s%%)", prettyNum(FREQ, big.mark=","), PCT))))
}

render.strat <- function (label, n, ...) {
  sprintf("<span class='stratlabel'>%s<br><span class='stratn'>(N=%s)</span></span>", 
          label, prettyNum(n, big.mark=","))
}

### Read in SAS data file
df_vte <- read_sas('~/Dropbox (Harvard University)/Haneuse/DURABLE/vte_data.sas7bdat')

### use table1 to make table
df_vte <- 
  df_vte %>% 
  mutate('raceeth' = case_when(raceeth == 'Multiple' ~ 'Other',
                               T ~ raceeth),
         'surg_status' = factor(ifelse(surg_cont == 'Surgery', 'Bariatric Patients', 'Matched Non-Surgical Patients')),
         'elix_cat' = as.factor(elix_cat),
         'diabetes' = as.factor(diabetes),
         'dysl' = as.factor(dysl),
         'insulin' = as.factor(insulin),
         'statins'  = as.factor(statins),
         'nonstatins' = as.factor(nonstatins),
         'missing_bp' = as.factor(missing_bp),
         'htn_dx' = as.factor(htn_dx),
         'htn_arb' = as.factor(htn_arb),
         'htn_ace' = as.factor(htn_ace),
         'htn_non_acearb' = as.factor(htn_non_acearb),
         'bp_elevated' = case_when(is.na(systolic) ~ NA_character_,
                                   is.na(diastolic) ~ NA_character_,
                                   systolic > 140 | diastolic > 90 ~ '1',
                                   T ~ '0'), ### confirm
         'cardio' = as.factor(cardio),
         'cerebro' = as.factor(cerebro),
         'ht_index' = as.factor(ht_index),
         'oc_index' = as.factor(oc_index),
         'anticoag_index' = as.factor(anticoag_index),
         'anticoag_30' = as.factor(anticoag_30),
         'index_cat' = case_when(year(index_date) >= 2005 & year(index_date) <= 2007 ~ '2005-2007',
                                 year(index_date) >= 2008 & year(index_date) <= 2010 ~ '2008-2010',
                                 year(index_date) >= 2011 & year(index_date) <= 2013 ~ '2011-2013',
                                 year(index_date) >= 2014 & year(index_date) <= 2015 ~ '2014-2015'))

label(df_vte$bs_type) <- 'Surgery Type'
label(df_vte$age) <- 'Age'
label(df_vte$agecat) <- 'Age Categories'
label(df_vte$GENDER) <- 'Gender'
label(df_vte$raceeth) <- 'Race/Ethnicity'
label(df_vte$site) <- 'Health care Site'
label(df_vte$inscat) <- 'Insurance Type'
label(df_vte$index_cat) <- 'Year of surgery/index date'
label(df_vte$util_count) <- 'Number of days health care in use pre-index date'
label(df_vte$days_ip) <- 'Number of days hospitalized in year prior to index date' ### confirm this
label(df_vte$elix_cat) <- 'Charlson/Elixhauser comorbidity score'
label(df_vte$bmi) <- 'BMI'
label(df_vte$bmicat) <- 'BMI categories'
label(df_vte$diabetes) <- 'Diabetes status'
label(df_vte$insulin) <- 'Use of insulin'
label(df_vte$dysl) <- 'Dyslipidemia'
label(df_vte$statins) <- 'Use of a statin'
label(df_vte$nonstatins) <- 'Use of other lipid-lowering medication'
label(df_vte$systolic) <- 'Systolic Blood Pressure'
label(df_vte$diastolic) <- 'Diastolic Blood Pressure'
label(df_vte$missing_bp) <- 'Missing BP Measures'
label(df_vte$bp_elevated) <- 'Elevated blood pressure >140/90'
label(df_vte$htn_dx) <- 'Hypertension Diagnosis'
label(df_vte$htn_ace) <-  'Use of ACE inhibitors'
label(df_vte$htn_arb) <-  'Use of ARB'
label(df_vte$htn_non_acearb) <- 'Use of other antihypertensive medications'
label(df_vte$cardio) <- 'Cardiac event or ischemic heart disease diagnosis'
label(df_vte$cerebro) <- 'Ischemic or hemorrhagic stroke'
label(df_vte$Smoking2) <- 'Smoking status, self-reported'
label(df_vte$mh_cat) <- 'Mental Health Diagnosis'
label(df_vte$oc_index) <- 'Oral contraceptive use at index date'
label(df_vte$ht_index) <- 'Hormone therapy use at index date'
label(df_vte$anticoag_index) <- 'Anticoagulant use at index date'
label(df_vte$anticoag_30) <- 'Anticoagulant use 30 days prior to the index date'


units(df_vte$age) <- 'years'
units(df_vte$agecat) <- 'years'
units(df_vte$bmi) <- 'kg/m2'
units(df_vte$bmicat) <- 'kg/m2'

table1(~bs_type + age + agecat + GENDER + raceeth + site + index_cat +  inscat + 
         util_count + days_ip + elix_cat + bmi + bmicat + diabetes + insulin + 
         dysl + statins + nonstatins + systolic + diastolic + bp_elevated + 
         missing_bp + htn_dx + htn_ace + htn_arb + htn_non_acearb + cardio + 
         cerebro + Smoking2 + mh_cat + oc_index + ht_index + anticoag_index + 
         anticoag_30 | surg_status,
       data = df_vte, 
       render.continuous = render.continuous,
       render.strat = render.strat,
       render.categorical = render.categorical,
       overall = F)


df_vte %>% 
  filter(!is.na(bp_elevated)) %>% 
  group_by(surg_cont) %>% 
  summarise('n_bp_high' = sum(as.numeric(bp_elevated)),
            'freq' = mean(as.numeric(bp_elevated)))
