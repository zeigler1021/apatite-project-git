bootstrap.linreg <- function(df, param) {
  
  if (param == "ft") {
    sample_df <- df %>% select(sample, s.ft,  db.ft) %>% rename(twoD = s.ft,  threeD = db.ft)
  }
  if (param == "volume") {
    sample_df <- df %>% select(sample, s.v,  db.v) %>% rename(twoD = s.v,  threeD = db.v)
  }
  
  # Set set.seed a starting number to generate a sequence of random numbers so that we can get reproducible results
  set.seed(123)
  #Perform bootstrap 
  sample_boot <- bootstraps(sample_df,
                            times = 1e3,
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
  
  #Store results
  results <<- as.data.frame(cbind(percentile_intervals$.estimate, sd(sample_coefs$estimate),mean(sample_coefs$std.error))) %>%
    rename(slope = V1, sd = V2, std.error = V3)
  
  print(results)
}
