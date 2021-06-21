#Function to print the uncertianty on the slope of the linear regression

slope.uncertainty <- function(df, param) {
  
  if (param == "ft") {
    y <- df$db.ft
    x <- df$s.ft
    fit <- summary(lm(y ~ 0 + x, data= df))
    std_err <- fit$sigma
    fit <- data.frame(fit$coefficients)
    df_values <- data.frame(twoD = df$s.ft,
                            threeD = df$db.ft, 
                            slope = rep(fit[1,1], len= nrow(df))) 
    
    delta <- nrow(df_values) * sum(df_values$twoD^2) - (sum(df_values$twoD))^2
    sigma.y <- sqrt(1/(nrow(df_values)-2)*sum((df_values$threeD - 0 - df_values$twoD*df_values$slope)^2))
    
    uncert.slope <- sigma.y * sqrt(nrow(df_values)/delta)
    
    results <- rbind(slope=fit[1,1], sigma.slope=uncert.slope, intercept=0, sigma.intercept=0, std.error=std_err)
    results <<- as.data.frame(t(as.matrix(results)))
  }
  
  if (param == "volume") {
    y <- df$db.v
    x <- df$s.v
    fit <- summary(lm(y ~ 0 + x, data= df))
    std_err <- fit$sigma
    fit <- data.frame(fit$coefficients)
    df_values <- data.frame(twoD = df$s.v,
                            threeD = df$db.v, 
                            slope = rep(fit[1,1], len= nrow(df)))
    
    delta <- nrow(df_values) * sum(df_values$twoD^2) - (sum(df_values$twoD))^2
    sigma.y <- sqrt(1/(nrow(df_values)-2)*sum((df_values$threeD - 0 - df_values$twoD*df_values$slope)^2))
    
    uncert.slope <- sigma.y * sqrt(nrow(df_values)/delta)
    
    results <- rbind(slope=fit[1,1], sigma.slope=uncert.slope, intercept=0, sigma.intercept=0, std.error=std_err)
    results <<- as.data.frame(t(as.matrix(results)))
  }
  
  if (param == "esr") {
    y <- df$db.esr.ft
    x <- df$s.esr.ft
    fit <- summary(lm(y ~ 0 + x, data= df))
    std_err <- fit$sigma
    fit <- data.frame(fit$coefficients)
    df_values <- data.frame(twoD = df$s.esr.ft,
                            threeD = df$db.esr.ft, 
                            slope = rep(fit[1,1], len= nrow(df)))
    
    delta <- nrow(df_values) * sum(df_values$twoD^2) - (sum(df_values$twoD))^2
    sigma.y <- sqrt(1/(nrow(df_values)-2)*sum((df_values$threeD - 0 - df_values$twoD*df_values$slope)^2))
    
    uncert.slope <- sigma.y * sqrt(nrow(df_values)/delta)
    
    results <- rbind(slope=fit[1,1], sigma.slope=uncert.slope, intercept=0, sigma.intercept=0, std.error=std_err)
    results <<- as.data.frame(t(as.matrix(results)))
  }
}









#Function to print the uncertianty on the slope of the linear regression
#Switched the value that "has all the uncertainty" FROM the threeD to twoD

slope.uncertainty <- function(df, param) {
  
  if (param == "ft") {
    x <- df$db.ft
    y <- df$s.ft
    fit <- summary(lm(y ~ 0 + x, data= df))
    std_err <- fit$sigma
    fit <- data.frame(fit$coefficients)
    df_values <- data.frame(twoD = x,
                            threeD = y, 
                            slope = rep(fit[1,1], len= nrow(df))) 
    
    delta <- nrow(df_values) * sum(df_values$twoD^2) - (sum(df_values$twoD))^2
    sigma.y <- sqrt(1/(nrow(df_values)-2)*sum((df_values$threeD - 0 - df_values$twoD*df_values$slope)^2))
    
    uncert.slope <- sigma.y * sqrt(nrow(df_values)/delta)
    
    results <- rbind(slope=fit[1,1], sigma.slope=uncert.slope, intercept=0, sigma.intercept=0, std.error=std_err)
    results <<- as.data.frame(t(as.matrix(results)))
  }
  
  if (param == "volume") {
    y <- df$db.v
    x <- df$s.v
    fit <- summary(lm(y ~ 0 + x, data= df))
    std_err <- fit$sigma
    fit <- data.frame(fit$coefficients)
    df_values <- data.frame(twoD = df$s.v,
                            threeD = df$db.v, 
                            slope = rep(fit[1,1], len= nrow(df)))
    
    delta <- nrow(df_values) * sum(df_values$twoD^2) - (sum(df_values$twoD))^2
    sigma.y <- sqrt(1/(nrow(df_values)-2)*sum((df_values$threeD - 0 - df_values$twoD*df_values$slope)^2))
    
    uncert.slope <- sigma.y * sqrt(nrow(df_values)/delta)
    
    results <- rbind(slope=fit[1,1], sigma.slope=uncert.slope, intercept=0, sigma.intercept=0, std.error=std_err)
    results <<- as.data.frame(t(as.matrix(results)))
  }
  
  if (param == "esr") {
    y <- df$db.esr.ft
    x <- df$s.esr.ft
    fit <- summary(lm(y ~ 0 + x, data= df))
    std_err <- fit$sigma
    fit <- data.frame(fit$coefficients)
    df_values <- data.frame(twoD = df$s.esr.ft,
                            threeD = df$db.esr.ft, 
                            slope = rep(fit[1,1], len= nrow(df)))
    
    delta <- nrow(df_values) * sum(df_values$twoD^2) - (sum(df_values$twoD))^2
    sigma.y <- sqrt(1/(nrow(df_values)-2)*sum((df_values$threeD - 0 - df_values$twoD*df_values$slope)^2))
    
    uncert.slope <- sigma.y * sqrt(nrow(df_values)/delta)
    
    results <- rbind(slope=fit[1,1], sigma.slope=uncert.slope, intercept=0, sigma.intercept=0, std.error=std_err)
    results <<- as.data.frame(t(as.matrix(results)))
  }
}
