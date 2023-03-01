library(tidyverse)
library(splines)

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
extract_hr <- function(hr_summary, cov_mat, knots, interaction = F, n_levels = 2) {
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
              interaction = interaction,
              n_levels = n_levels) %>% 
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
              interaction = interaction,
              n_levels = n_levels) %>% 
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
              interaction = interaction,
              n_levels = n_levels)%>% 
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
              interaction = interaction,
              n_levels = n_levels) %>% 
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
              interaction = interaction,
              n_levels = n_levels) %>% 
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
              interaction = interaction,
              n_levels = n_levels) %>% 
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
              interaction = interaction,
              n_levels = n_levels) %>% 
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
              interaction = interaction,
              n_levels = n_levels) %>% 
    mutate('model' = 'NCS with df = 6')
  
  
  df_log_hr <- 
    bind_rows(out_ph, out_logt, out_ncs1, out_ncs2, 
              out_ncs3, out_ncs4, out_ncs5, out_ncs6) %>% 
    mutate('outcome' = hr_summary$outcome[1]) %>% 
    mutate('model' = fct_relevel(model, 'Proportional Hazards', 'Interaction with log(t)'))
  
  return(df_log_hr)
}


### Compute Log HR and SE (pointwise)
log_hr_se <- function(X, beta, Sigma, interaction, n_levels = 2) {
  if(!interaction) {
    log_hr <- as.vector(t(X %*% beta))
    std_err <- sqrt(map_dbl(1:nrow(X), ~{t(X[.x,]) %*% Sigma %*% X[.x,]}))
    
    df <- 
      tibble('log_hr' = log_hr,
             'std_err' = std_err,
             'time' = 1:nrow(X))
  } else {
    df <- NULL
    zero_matrix <- matrix(0, nrow = nrow(X), ncol = ncol(X) * (n_levels - 1))
    
    ### X_ = modified X matrix for interactions 
    ### It's X matrix once and then in the corresponding interactions slot 
    for(i in 0:(n_levels-1)) {
      X_ <- cbind(X, zero_matrix) 
      
      ### Place X in appropriate Interaction group columns 
      if(i > 0) {
        x_ix <- seq(ncol(X) + i, by = n_levels - 1, length.out = ncol(X))
        X_[,x_ix] <- X
      }
      
      log_hr <- as.vector(t(X_ %*% beta))
      std_err <- sqrt(map_dbl(1:nrow(X_), ~{t(X_[.x,]) %*% Sigma %*% X_[.x,]}))
      
      df_ <- 
        tibble('log_hr' = log_hr,
               'std_err' = std_err,
               'time' = 1:nrow(X),
               'interaction' = i)
      
      df <- bind_rows(df, df_)
    }
    
    return(df)
  }
}

get_hr_best <- function(size, analysis, eval_times, knots, interaction = F, n_levels = 0) {
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
  
  df_best <- 
    map_dfr(model_files, read_csv) %>% 
    group_by(outcome) %>% 
    filter(bic == min(bic)) %>% 
    ungroup()
  
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
                     n_levels = n_levels))
  
  df_summary <- 
    bind_rows(df_log_hr) %>% 
    inner_join(df_best, by = c("model", "outcome")) %>% 
    filter(time %in% eval_times) %>% 
    select(outcome, time, log_hr, std_err) %>% 
    mutate('time' = round(time/365.25))
    
  
  return(df_summary)
  
}

  