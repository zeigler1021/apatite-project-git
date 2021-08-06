bootstrap.linreg <- function(df, param, df_name) {
  
  if (param == "ft") {
    sample_df <- df %>% 
      select(sample, gem, s.ft,  db.ft) %>% 
      rename(twoD = s.ft, threeD = db.ft) 
  
  }
  if (param == "volume") {
    sample_df <- df %>% 
      select(sample, gem, s.v,  db.v) %>% 
      rename(twoD = s.v,  threeD = db.v)
      
  }
  
  if (param == "esr") {
    sample_df <- df %>%
      select(sample, gem, s.esr.ft, db.esr.ft) %>%
      rename(twoD = s.esr.ft, threeD = db.esr.ft)
  }
  
  # Set set.seed a starting number to generate a sequence of random numbers so that we can get reproducible results
  set.seed(123)
  #Perform bootstrap 
  sample_boot <- bootstraps(sample_df,
                            times = 1000,
                            apparent = TRUE)
  
  #Run linear regression on each bootstrap
  sample_models <- sample_boot %>% 
    mutate(model = map(splits, ~ lm(twoD ~ 0 + threeD,
                                    data = .) ),
           coef_inf = map(model, tidy))
  
  #Get coefficients
  sample_coefs <- sample_models %>% 
    unnest(coef_inf)
  
  #Get confidence interval 
  percentile_intervals <- int_pctl(sample_models,
                                   coef_inf)
  
  #Store residuals for each bootstrapped sample
  res <- matrix(nrow = nrow(df), ncol = 1001)

  for (i in 1:1001) {
    res[,i] <- unname(sample_coefs[[3]][[i]]$residuals)
    assign(paste(glue("{df_name}"), "residuals", glue("{param}"), sep = "_"), res, envir = parent.frame())
  }

  #Store results
  results_boot <<- as.data.frame(cbind(percentile_intervals$.estimate, mean(sample_coefs$std.error), (1/percentile_intervals$.estimate))) %>%
    rename(slope = V1, std.error = V2, plot.slope= V3)
  
  assign(paste(glue("{df_name}"), "slopes", glue("{param}"), sep = "_"), sample_coefs$estimate, envir = parent.frame())
  
  print(results_boot)
}
