residual.sd <- function (geo_df, results_df, twoD, threeD, df_name) {
  
  corr <- results_df$correction[1]
  
  sample_df <- cbind(select(geo_df, sample, twoD, threeD, ri, size.cat, j.w1), corr) %>%
    rename(twoD = 2, threeD = 3) %>% 
    mutate(residual = (twoD - (corr * threeD)), 
           percent_diff = (residual/twoD) * 100)
  
  sample_df$size.cat <- factor(sample_df$size.cat, levels = c("rare- small", "common",  "rare- large"))
  
  
  var <- "ellip"
  var <- as.data.frame(var)
  
  if (nrow(geo_df) > 200) {
    residual_results <- sample_df %>% 
      group_by(ifelse(twoD == "s.v", ri, size.cat)) %>% 
      summarize(sd_pd = sd(percent_diff)) %>% 
      rename(var = `ifelse(twoD == \"s.v\", ri, size.cat)`)
    
    residual_results$var <- factor(residual_results$var, levels = c("1", "2",  "3"))
    levels(residual_results$var) <- list(small = "1", average = "2", large = "3")
  } else {
    residual_results <- sample_df %>% 
      summarize(sd_pd = sd(percent_diff)) %>%
      bind_cols(var) %>%
      relocate(var, .before = sd_pd)
  }
  
  results_df <- select(sample_df, sample, corr, residual, percent_diff)
  
  assign(paste(glue("{df_name}"), "rr", sep = "_"), residual_results, envir = parent.frame())  
  assign(paste(glue("{df_name}"), "residuals", sep = "_"), results_df, envir = parent.frame())
  
}