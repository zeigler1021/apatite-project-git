#Quadrature for Ft

#function
error.propagation <- function(df, category, parameter, compare1, compare2) {
  df <- df %>%
    filter(category == "category") %>% #method
    filter(parameter == "parameter") %>% #ft or volume
    select(compare, slope, sigma.slope)
  
  compare1 <- df %>% filter(compare == "compare1")
  compare2 <- df %>% filter(compare == "compare2")
  
  diff <- compare1$slope - compare2$slope
  uncert <- sqrt(compare1$sigma.slope^2 + compare2$sigma.slope^2)
  
  if (!is.null(abs(diff) < abs(uncert))) {
    print("Within 1sigma")
  } else {
    print("Not within 1sigma")
  }
}
