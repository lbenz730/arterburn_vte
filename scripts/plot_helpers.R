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
extract_hr <- function(hr_summary, cox_ph, cov_mat, knots) {
  time <- seq(1, 3920, 1)
  
  ### Cox PH
  log_hr_ph <- 
    tidy(cox_ph) %>% 
    filter(grepl('surg_cont', term)) %>% 
    pull(estimate)
  
  se_log_hr_ph <- 
    tidy(cox_ph) %>% 
    filter(grepl('surg_cont', term)) %>% 
    pull(std.error)
  
  out_ph <- 
    list('log_hr' = log_hr_ph,
         'std_err' = se_log_hr_ph)
  
  ### Log(t)
  df_logt <- 
    hr_summary %>% 
    filter(model == 'Interaction with log(t)') %>% 
    filter(grepl('surg_cont', term)) 
  
  out_logt <- 
    log_hr_se(X = cbind(matrix(rep(1, length(time))), matrix(log(time))),
              beta = df_logt$estimate,
              Sigma = cov_mat[['Interaction with log(t)']][df_logt$term, df_logt$term])
  
  ### NCS df = 1
  df_ncs1 <- 
    hr_summary %>% 
    filter(model == 'NCS with df = 1') %>% 
    filter(grepl('surg_cont', term))
  
  out_ncs1 <- 
    log_hr_se(X = cbind(matrix(rep(1, length(time))), ns(time, df = 1)),
              beta = df_ncs1$estimate,
              Sigma = cov_mat[['NCS with df = 1']][df_ncs1$term, df_ncs1$term])
  
  ### NCS df = 2
  df_ncs2 <- 
    hr_summary %>% 
    filter(model == 'NCS with df = 2') %>% 
    filter(grepl('surg_cont', term))
  
  out_ncs2 <- 
    log_hr_se(X = cbind(matrix(rep(1, length(time))), ns(time, df = 2, knots = knots[['2']])),
              beta = df_ncs2$estimate,
              Sigma = cov_mat[['NCS with df = 2']][df_ncs2$term, df_ncs2$term])
  
  ### NCS df = 3
  df_ncs3 <- 
    hr_summary %>% 
    filter(model == 'NCS with df = 3') %>% 
    filter(grepl('surg_cont', term))
  
  out_ncs3 <- 
    log_hr_se(X = cbind(matrix(rep(1, length(time))), ns(time, df = 3, knots = knots[['3']])),
              beta = df_ncs3$estimate,
              Sigma = cov_mat[['NCS with df = 3']][df_ncs3$term, df_ncs3$term])
  
  ### NCS df = 6
  df_ncs6 <- 
    hr_summary %>% 
    filter(model == 'NCS with df = 6') %>% 
    filter(grepl('surg_cont', term))
  
  out_ncs6 <- 
    log_hr_se(X = cbind(matrix(rep(1, length(time))), ns(time, df = 6, knots = knots[['6']])),
              beta = df_ncs6$estimate,
              Sigma = cov_mat[['NCS with df = 6']][df_ncs6$term, df_ncs6$term])
  
  ### NCS df = 8
  df_ncs8 <- 
    hr_summary %>% 
    filter(model == 'NCS with df = 8') %>% 
    filter(grepl('surg_cont', term))
  
  out_ncs8 <- 
    log_hr_se(X = cbind(matrix(rep(1, length(time))), ns(time, df = 8, knots = knots[['8']])),
              beta = df_ncs8$estimate,
              Sigma = cov_mat[['NCS with df = 8']][df_ncs8$term, df_ncs8$term])
  
  ### NCS df = 10
  df_ncs10 <- 
    hr_summary %>% 
    filter(model == 'NCS with df = 10') %>% 
    filter(grepl('surg_cont', term))
  
  out_ncs10 <- 
    log_hr_se(X = cbind(matrix(rep(1, length(time))), ns(time, df = 10, knots = knots[['10']])),
              beta = df_ncs10$estimate,
              Sigma = cov_mat[['NCS with df = 10']][df_ncs10$term, df_ncs10$term])
  
  df_log_hr <- 
    bind_rows(
      tibble('time' = time, 'log_hr' = out_ph$log_hr, 'std_err' = out_ph$std_err, 'model' = 'Proportional Hazards'),
      tibble('time' = time, 'log_hr' = out_logt$log_hr, 'std_err' = out_logt$std_err, 'model' = 'Interaction with log(t)'),
      tibble('time' = time, 'log_hr' = out_ncs1$log_hr, 'std_err' = out_ncs1$std_err, 'model' = 'NCS with df = 1'),
      tibble('time' = time, 'log_hr' = out_ncs2$log_hr, 'std_err' = out_ncs2$std_err, 'model' = 'NCS with df = 2'),
      tibble('time' = time, 'log_hr' = out_ncs3$log_hr, 'std_err' = out_ncs3$std_err, 'model' = 'NCS with df = 3'),
      tibble('time' = time, 'log_hr' = out_ncs6$log_hr, 'std_err' = out_ncs6$std_err, 'model' = 'NCS with df = 6'),
      tibble('time' = time, 'log_hr' = out_ncs8$log_hr, 'std_err' = out_ncs8$std_err, 'model' = 'NCS with df = 8'),
      tibble('time' = time, 'log_hr' = out_ncs10$log_hr, 'std_err' = out_ncs10$std_err, 'model' = 'NCS with df = 10')
    ) %>% 
    mutate('outcome' = hr_summary$outcome[1]) %>% 
    mutate('model' = fct_relevel(model, 'Proportional Hazards', 'Interaction with log(t)')) %>% 
    mutate('model' = fct_relevel(model, 'NCS with df = 10', after = nlevels(model)-1))
  
  return(df_log_hr)
}


### Compute Log HR and SE (pointwise)
log_hr_se <- function(X, beta, Sigma) {
  log_hr <- as.vector(t(X %*% beta))
  std_err <- sqrt(map_dbl(1:nrow(X), ~{t(X[.x,]) %*% Sigma %*% X[.x,]}))
  
  return(list('log_hr' = log_hr,
              'std_err' = std_err))
}
