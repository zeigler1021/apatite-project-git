bootstrap.linreg.nest.not.fixed <- function (x, param) {
  
  # Set set.seed a starting number to generate a sequence of random numbers so that we can get reproducible results
  set.seed(123)
  #Perform bootstrap 
  sample_boot <- bootstraps(x,
                            times = 1000,
                            apparent = TRUE)
  
  #Run linear regression on each bootstrap
  if (param == "ft") {
    sample_models <- sample_boot %>%
      mutate(model = map(splits, ~ lm(s.ft ~ db.ft,
                                      data = .)), 
             coef_inf = map(model, tidy))
    
    #Get coefficients
    sample_coefs <- sample_models %>% 
      unnest(coef_inf)
    
    #Get confidence interval 
    #percentile_intervals <- int_pctl(sample_models,coef_inf)
    #Retrieve intercepts and means
    intercept_coefs <- sample_coefs %>% select(term, estimate) %>% filter(term == "(Intercept)")
    slope_coefs <- sample_coefs %>% select(term, estimate) %>% filter(term == "db.ft")
  }
  
  if (param == "volume")  {
    sample_models <- sample_boot %>%
      mutate(model = map(splits, ~ lm(s.v ~ db.v,
                                      data = .)), 
             coef_inf = map(model, tidy))
    
    #Get coefficients
    sample_coefs <- sample_models %>% 
      unnest(coef_inf)
    
    #Get confidence interval 
    #percentile_intervals <- int_pctl(sample_models,coef_inf)
    #Retrieve intercepts and means
    intercept_coefs <- sample_coefs %>% select(term, estimate) %>% filter(term == "(Intercept)")
    slope_coefs <- sample_coefs %>% select(term, estimate) %>% filter(term == "db.v")
  }
  
  if (param == "esr") {
    sample_models <- sample_boot %>%
      mutate(model = map(splits, ~ lm(s.esr.ft ~ db.esr.ft,
                                      data = .)), 
             coef_inf = map(model, tidy))
    #Get coefficients
    sample_coefs <- sample_models %>% 
      unnest(coef_inf)
    
    #Get confidence interval 
    #percentile_intervals <- int_pctl(sample_models,coef_inf)
    #Retrieve intercepts and means
    intercept_coefs <- sample_coefs %>% select(term, estimate) %>% filter(term == "(Intercept)")
    slope_coefs <- sample_coefs %>% select(term, estimate) %>% filter(term == "db.esr.ft")
  }
  
  
  #Store results
  slope <-  mean(slope_coefs$estimate)
  intercept <- mean(intercept_coefs$estimate)
  std.err <- mean(sample_coefs$std.error)
  plot.slope <- 1/(mean(slope_coefs$estimate))
  
  intercept_boot <- sample_coefs %>% select(term, estimate) %>% filter(term == "(Intercept)") %>% select(estimate)
  slope_boot <- slope_coefs %>% select(estimate)
  results_boot <- as.data.frame(cbind(slope, intercept, std.err, plot.slope))
  
  results_boot_list <- list(results_boot, intercept_boot, slope_boot)
  return(results_boot_list)
  
  #assign(paste(glue("{df_name}"), "slopes", glue("{param}"), sep = "_"), sample_coefs$estimate, envir = parent.frame())
  #assign("boot_slopes", slopes_boot, envir = parent.frame())
} 

#############################################################################################

bootstrap.intercepts <- function (x, param) {
  # Set set.seed a starting number to generate a sequence of random numbers so that we can get reproducible results
  set.seed(123)
  #Perform bootstrap 
  sample_boot <- bootstraps(x,
                            times = 1000,
                            apparent = TRUE)
  
  #Run linear regression on each bootstrap
  if (param == "ft") {
    sample_models <- sample_boot %>%
      mutate(model = map(splits, ~ lm(s.ft ~ db.ft,
                                      data = .)), 
             coef_inf = map(model, tidy))
  }
  
  if (param == "volume")  {
    sample_models <- sample_boot %>%
      mutate(model = map(splits, ~ lm(s.v ~ db.v,
                                      data = .)), 
             coef_inf = map(model, tidy))
  }
  
  if (param == "esr") {
    sample_models <- sample_boot %>%
      mutate(model = map(splits, ~ lm(s.esr.ft ~ db.esr.ft,
                                      data = .)), 
             coef_inf = map(model, tidy))
  }
  
  #Get coefficients
  sample_coefs <- sample_models %>% 
    unnest(coef_inf)
  
  intercept_boot <- sample_coefs %>% select(term, estimate) %>% filter(term == "(Intercept)") %>% select(estimate)
}
