ttest_compare <- function (x, y) {
  set.seed(123)
  
  x <- sample(x, 100)
  y <- sample(y, 100)
  #Made thesample size 100% because sample size should be ~10% of population. Population = 1000. 
  
  t <- t.test(x,y, conf.level = 0.6827)
  
  #Ho: the slopes are equal
  #H1: the slopes are not equal
  #if p < 0.05, reject the null hypothesis (ie. H1 is true!)
  
  interpretation <- if (t$p.value > 0.05) {
    "The slopes are the same"
  } else {
    "The slopes are different"
  }
  
  results_ttest <<- as.data.frame(cbind(t$p.value, interpretation)) %>%
    rename(p.value = V1)
  return(results_ttest)
}
