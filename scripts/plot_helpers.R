library(tidyverse)

### Custom ggplot theme
theme_set(theme_bw() +
            theme(plot.title = element_text(hjust = 0.5, size = 24),
                  plot.subtitle = element_text(hjust = 0.5, size = 18),
                  axis.title = element_text(size = 20),
                  strip.text = element_text(size = 12),
                  strip.text.y = element_text(size = 8),
                  plot.caption = element_text(size = 10),
                  legend.text = element_text(size = 12),
                  legend.position = "bottom"))

### Get HR as Functions of time for each model
extract_hr <- function(hr_summary, cox_ph) {
  time <- seq(1, 3920, 1)
  
  ### Cox PH
  log_hr_ph <- 
    tidy(cox_ph) %>% 
    filter(grepl('surg_cont', term)) %>% 
    pull(estimate)
  
  ### Log(t)
  beta_logt <- 
    hr_summary %>% 
    filter(model == 'Interaction with log(t)') %>% 
    filter(grepl('surg_cont', term)) %>% 
    pull(estimate)
  
  log_hr_logt <- beta_logt[1] + beta_logt[2] * log(time)
  
  ### NCS df = 1
  beta_ncs1 <- 
    hr_summary %>% 
    filter(model == 'NCS with df = 1') %>% 
    filter(grepl('surg_cont', term)) %>% 
    pull(estimate)
  
  log_hr_ncs1 <- beta_ncs1[1] + as.vector(beta_ncs1[-1] %*% t(ns(time, df = 1)))
  
  ### NCS df = 2
  beta_ncs2 <- 
    hr_summary %>% 
    filter(model == 'NCS with df = 2') %>% 
    filter(grepl('surg_cont', term)) %>% 
    pull(estimate)
  
  log_hr_ncs2 <- beta_ncs2[1] + as.vector(beta_ncs2[-1] %*% t(ns(time, df = 2)))
  
  ### NCS df = 3
  beta_ncs3 <- 
    hr_summary %>% 
    filter(model == 'NCS with df = 3') %>% 
    filter(grepl('surg_cont', term)) %>% 
    pull(estimate)
  
  log_hr_ncs3 <- beta_ncs3[1] + as.vector(beta_ncs3[-1] %*% t(ns(time, df = 3)))
  
  ### NCS df = 6
  beta_ncs6 <- 
    hr_summary %>% 
    filter(model == 'NCS with df = 6') %>% 
    filter(grepl('surg_cont', term)) %>% 
    pull(estimate)
  
  log_hr_ncs6 <- beta_ncs6[1] + as.vector(beta_ncs6[-1] %*% t(ns(time, df = 6)))
  
  ### NCS df = 8
  beta_ncs8 <- 
    hr_summary %>% 
    filter(model == 'NCS with df = 8') %>% 
    filter(grepl('surg_cont', term)) %>% 
    pull(estimate)
  
  log_hr_ncs8 <- beta_ncs8[1] + as.vector(beta_ncs8[-1] %*% t(ns(time, df = 8)))
  
  ### NCS df = 10
  beta_ncs10 <- 
    hr_summary %>% 
    filter(model == 'NCS with df = 10') %>% 
    filter(grepl('surg_cont', term)) %>% 
    pull(estimate)
  
  log_hr_ncs10 <- beta_ncs10[1] + as.vector(beta_ncs10[-1] %*% t(ns(time, df = 10)))
  
  df_log_hr <- 
    bind_rows(
      tibble('time' = time, 'log_hr' = log_hr_ph, 'model' = 'Proportional Hazards'),
      tibble('time' = time, 'log_hr' = log_hr_logt, 'model' = 'Interaction with log(t)'),
      tibble('time' = time, 'log_hr' = log_hr_ncs1, 'model' = 'NCS with df = 1'),
      tibble('time' = time, 'log_hr' = log_hr_ncs2, 'model' = 'NCS with df = 2'),
      tibble('time' = time, 'log_hr' = log_hr_ncs3, 'model' = 'NCS with df = 3'),
      tibble('time' = time, 'log_hr' = log_hr_ncs6, 'model' = 'NCS with df = 6'),
      tibble('time' = time, 'log_hr' = log_hr_ncs8, 'model' = 'NCS with df = 8'),
      tibble('time' = time, 'log_hr' = log_hr_ncs10, 'model' = 'NCS with df = 10')
    ) %>% 
    mutate('outcome' = hr_summary$outcome[1]) %>% 
    mutate('model' = fct_relevel(model, 'Proportional Hazards', 'Interaction with log(t)')) %>% 
    mutate('model' = fct_relevel(model, 'NCS with df = 10', after = nlevels(model)-1))
  
  return(df_log_hr)
}
