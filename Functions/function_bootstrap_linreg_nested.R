bootstrap.linreg.nest <- function (x, param) {

  # Set set.seed a starting number to generate a sequence of random numbers so that we can get reproducible results
  set.seed(123)
  #Perform bootstrap 
  sample_boot <- bootstraps(x,
                            times = 1000,
                            apparent = TRUE)
  
  #Run linear regression on each bootstrap
 if (param == "ft") {
   sample_models <- sample_boot %>%
    mutate(model = map(splits, ~ lm(s.ft ~ 0 + db.ft,
                                    data = .)), 
           coef_inf = map(model, tidy))
 }
  
if (param == "volume")  {
  sample_models <- sample_boot %>%
    mutate(model = map(splits, ~ lm(s.v ~ 0 + db.v,
                                    data = .)), 
           coef_inf = map(model, tidy))
}
  
if (param == "esr") {
  sample_models <- sample_boot %>%
    mutate(model = map(splits, ~ lm(s.esr.ft ~ 0 + db.esr.ft,
                                    data = .)), 
           coef_inf = map(model, tidy))
}
  
  #Get coefficients
  sample_coefs <- sample_models %>% 
    unnest(coef_inf)
  
  #Get confidence interval 
  #percentile_intervals <- int_pctl(sample_models,coef_inf)
  
  #GET RESIDUALS
  residuals_og <- unname(sample_coefs[[3]][[1001]]$residuals)
  
  
  #Store results
  slope <-  mean(sample_coefs$estimate)
  std.err <- mean(sample_coefs$std.error)
  plot.slope <- 1/(mean(sample_coefs$estimate))
  slopes_boot <- sample_coefs$estimate
  results_boot <- as.data.frame(cbind(slope, std.err, plot.slope))
  
  results_boot_list <- list(results_boot, slopes_boot, residuals_og)
  return(results_boot_list)
  
  
  
  #assign(paste(glue("{df_name}"), "slopes", glue("{param}"), sep = "_"), sample_coefs$estimate, envir = parent.frame())
  #assign("boot_slopes", slopes_boot, envir = parent.frame())
} 
