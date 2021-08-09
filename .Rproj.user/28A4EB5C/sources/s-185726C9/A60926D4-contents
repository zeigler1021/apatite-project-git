taylor.uncertainty <- function (df, param) {
  
  
  #perform sigma slope calculaions
  if (param == "ft") {
    
    slope <- summary(lm(df$s.ft ~ 0 + df$db.ft))$coefficients[1,1]
    
    delta <- nrow(df) * sum(df$db.ft^2) - (sum(df$db.ft))^2
    sigma.y <- sqrt(1/(nrow(df)-2)*sum((df$s.ft - 0 - df$db.ft * slope)^2))
    uncert.slope <- sigma.y * sqrt(nrow(df)/delta)
  }
  
  if (param == "volume") {
    slope <- summary(lm(df$s.v ~ 0 + df$db.v))$coefficients[1,1]
    
    delta <- nrow(df) * sum(df$db.v^2) - (sum(df$db.v))^2
    sigma.y <- sqrt(1/(nrow(df)-2)*sum((df$s.v - 0 - df$db.v * slope)^2))
    uncert.slope <- sigma.y * sqrt(nrow(df)/delta)
  }
  
  if (param == "esr") {
    slope <- summary(lm(df$s.esr.ft ~ 0 + df$db.esr.ft))$coefficients[1,1]
    
    delta <<- nrow(df) * sum(df$db.esr.ft^2) - (sum(df$db.esr.ft))^2
    sigma.y <<- sqrt(1/(nrow(df)-2)*sum((df$s.esr.ft - 0 - df$db.esr.ft * slope)^2))
    uncert.slope <<- sigma.y * sqrt(nrow(df)/delta)
  }
  
  #extract results   
  results <- as.data.frame(cbind(slope=slope, sigma.slope = uncert.slope, plot.slope = 1/slope))
  
}


