#Quadrature for Ft

#function
error.propagation <- function(df, category, parameter, compare1, compare2) {

  category = enquo(category)
  parameter = enquo(parameter)
  compare1 = enquo(compare1)
  compare2 = enquo(compare2)
  
  df <- df %>%
    filter(category == !!category) %>% #method- ie. which grains did you run in the analysis/what type of analysis?
    filter(parameter == !!parameter) %>% #ft or volume 
    select(compare, slope, sigma.slope)
  
  compare1 <- df %>% filter(compare == !!compare1) #gem, gc, ri, np
  compare2 <- df %>% filter(compare == !!compare2) #gem, gc, ri, np
  
  diff <- abs(compare1$slope - compare2$slope)
  uncert <- abs(sqrt(compare1$sigma.slope^2 + compare2$sigma.slope^2))
  
  if (diff < uncert) {
    print("Within 1sigma")
  } else {
    print("Not within 1sigma")
  }
}


