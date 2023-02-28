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
extract_hr <- function(hr_summary, cov_mat, knots, interaction = F) {
  time <- seq(1, 3920, 1)
  
  ### Cox PH
  df_ph <- 
    hr_summary %>% 
    filter(model == 'Proportional Hazards') %>%
    filter(grepl('surg_cont', term)) 
  
  out_ph <- 
    log_hr_se(X = matrix(rep(1, length(time))),
              beta = df_ph$estimate,
              Sigma = cov_mat[['Proportional Hazards']][df_ph$term, df_ph$term],
              interaction = interaction) %>% 
    mutate('model' = 'Proportional Hazards')
  
  
  ### Log(t)
  df_logt <- 
    hr_summary %>% 
    filter(model == 'Interaction with log(t)') %>%
    filter(grepl('surg_cont', term)) 
  
  out_logt <- 
    log_hr_se(X = cbind(matrix(rep(1, length(time))), matrix(log(time))),
              beta = df_logt$estimate,
              Sigma = cov_mat[['Interaction with log(t)']][df_logt$term, df_logt$term],
              interaction = interaction) %>% 
    mutate('model' = 'Interaction with log(t)')
  
  ### NCS df = 1
  df_ncs1 <- 
    hr_summary %>% 
    filter(model == 'NCS with df = 1') %>% 
    filter(grepl('surg_cont', term))
  
  out_ncs1 <- 
    log_hr_se(X = cbind(matrix(rep(1, length(time))), ns(time, df = 1)),
              beta = df_ncs1$estimate,
              Sigma = cov_mat[['NCS with df = 1']][df_ncs1$term, df_ncs1$term],
              interaction = interaction )%>% 
    mutate('model' = 'NCS with df = 1')
  
  ### NCS df = 2
  df_ncs2 <- 
    hr_summary %>% 
    filter(model == 'NCS with df = 2') %>% 
    filter(grepl('surg_cont', term))
  
  out_ncs2 <- 
    log_hr_se(X = cbind(matrix(rep(1, length(time))), ns(time, df = 2, knots = knots[['2']])),
              beta = df_ncs2$estimate,
              Sigma = cov_mat[['NCS with df = 2']][df_ncs2$term, df_ncs2$term],
              interaction = interaction) %>% 
    mutate('model' = 'NCS with df = 2')
  
  ### NCS df = 3
  df_ncs3 <- 
    hr_summary %>% 
    filter(model == 'NCS with df = 3') %>% 
    filter(grepl('surg_cont', term))
  
  out_ncs3 <- 
    log_hr_se(X = cbind(matrix(rep(1, length(time))), ns(time, df = 3, knots = knots[['3']])),
              beta = df_ncs3$estimate,
              Sigma = cov_mat[['NCS with df = 3']][df_ncs3$term, df_ncs3$term],
              interaction = interaction) %>% 
    mutate('model' = 'NCS with df = 3')
  
  ### NCS df = 4
  df_ncs4 <- 
    hr_summary %>% 
    filter(model == 'NCS with df = 4') %>% 
    filter(grepl('surg_cont', term))
  
  out_ncs4 <- 
    log_hr_se(X = cbind(matrix(rep(1, length(time))), ns(time, df = 4, knots = knots[['4']])),
              beta = df_ncs4$estimate,
              Sigma = cov_mat[['NCS with df = 4']][df_ncs4$term, df_ncs4$term],
              interaction = interaction) %>% 
    mutate('model' = 'NCS with df = 4')
  
  ### NCS df = 5
  df_ncs5 <- 
    hr_summary %>% 
    filter(model == 'NCS with df = 5') %>% 
    filter(grepl('surg_cont', term))
  
  out_ncs5 <- 
    log_hr_se(X = cbind(matrix(rep(1, length(time))), ns(time, df = 5, knots = knots[['5']])),
              beta = df_ncs5$estimate,
              Sigma = cov_mat[['NCS with df = 5']][df_ncs5$term, df_ncs5$term],
              interaction = interaction) %>% 
    mutate('model' = 'NCS with df = 5')
  
  ### NCS df = 6
  df_ncs6 <- 
    hr_summary %>% 
    filter(model == 'NCS with df = 6') %>% 
    filter(grepl('surg_cont', term)) 
  
  out_ncs6 <- 
    log_hr_se(X = cbind(matrix(rep(1, length(time))), ns(time, df = 6, knots = knots[['6']])),
              beta = df_ncs6$estimate,
              Sigma = cov_mat[['NCS with df = 6']][df_ncs6$term, df_ncs6$term],
              interaction = interaction) %>% 
    mutate('model' = 'NCS with df = 6')
  
  ### NCS df = 7
  df_ncs7 <- 
    hr_summary %>% 
    filter(model == 'NCS with df = 7') %>% 
    filter(grepl('surg_cont', term))
  
  out_ncs7 <- 
    log_hr_se(X = cbind(matrix(rep(1, length(time))), ns(time, df = 7, knots = knots[['7']])),
              beta = df_ncs7$estimate,
              Sigma = cov_mat[['NCS with df = 7']][df_ncs7$term, df_ncs7$term],
              interaction = interaction) %>% 
    mutate('model' = 'NCS with df = 7')
  
  ### NCS df = 8
  df_ncs8 <-
    hr_summary %>%
    filter(model == 'NCS with df = 8') %>%
    filter(grepl('surg_cont', term)) 
  
  out_ncs8 <-
    log_hr_se(X = cbind(matrix(rep(1, length(time))), ns(time, df = 8, knots = knots[['8']])),
              beta = df_ncs8$estimate,
              Sigma = cov_mat[['NCS with df = 8']][df_ncs8$term, df_ncs8$term],
              interaction = interaction) %>% 
    mutate('model' = 'NCS with df = 8')
  
  
  df_log_hr <- 
    bind_rows(out_ph, out_logt, out_ncs1, out_ncs2, out_ncs3, 
              out_ncs4, out_ncs5, out_ncs6, out_ncs7, out_ncs8) %>% 
    mutate('outcome' = hr_summary$outcome[1]) %>% 
    mutate('model' = fct_relevel(model, 'Proportional Hazards', 'Interaction with log(t)'))
  
  return(df_log_hr)
}


### Compute Log HR and SE (pointwise)
log_hr_se <- function(X, beta, Sigma, interaction) {
  if(!interaction) {
    log_hr <- as.vector(t(X %*% beta))
    std_err <- sqrt(map_dbl(1:nrow(X), ~{t(X[.x,]) %*% Sigma %*% X[.x,]}))
    
    df <- 
      tibble('log_hr' = log_hr,
             'std_err' = std_err,
             'time' = 1:nrow(X))
  } else {
    ### Group w/ Interaction
    X1 <- cbind(X, X) 
    log_hr1 <- as.vector(t(X1 %*% beta))
    std_err1 <- sqrt(map_dbl(1:nrow(X1), ~{t(X1[.x,]) %*% Sigma %*% X1[.x,]}))
    
    df1 <- 
      tibble('log_hr' = log_hr1,
             'std_err' = std_err1,
             'time' = 1:nrow(X),
             'interaction' = 1)
    
    ### Baseline Group
    X0 <- cbind(X, matrix(0, nrow = nrow(X), ncol = ncol(X))) 
    log_hr0 <- as.vector(t(X0 %*% beta))
    std_err0 <- sqrt(map_dbl(1:nrow(X0), ~{t(X0[.x,]) %*% Sigma %*% X0[.x,]}))
    
    df0 <- 
      tibble('log_hr' = log_hr0,
             'std_err' = std_err0,
             'time' = 1:nrow(X),
             'interaction' = 0)
    
    df <- bind_rows(df1, df0)
  }
  
  return(df)
}
