#Function to print the uncertianty on the slope of the linear regression

slope.uncertainty <- function(df, param) {
  
  if (param == "ft") {
    y <- df$s.ft
    x <- df$db.ft
    fit <- summary(lm(y ~ 0 + x, data= df))
    std_err <- fit$sigma
    fit <- data.frame(fit$coefficients)
    df_values <- data.frame(y = df$s.ft,
                            x = df$db.ft, 
                            slope = rep(fit[1,1], len= nrow(df))) 
    
    delta <<- nrow(df_values) * sum(df_values$x^2) - (sum(df_values$x))^2
    sigma.y <- sqrt(1/(nrow(df_values)-2)*sum((df_values$y - 0 - df_values$x*df_values$slope)^2))
    
    uncert.slope <- sigma.y * sqrt(nrow(df_values)/delta)

   
    #b <<- ((nrow(df_values) * sum(df_values$x * df_values$y)) - (sum(df_values$x) * sum(df_values$y)))/delta
    #checking if the taylor equation for slope gives me the same as lm(), it does. 
    
    results <<- as.data.frame(cbind(slope=fit[1,1], sigma.slope=uncert.slope, std.error=std_err, plot.slope = 1/fit[1,1]))
  }
  
  if (param == "volume") {
    y <- df$s.v
    x <- df$db.v
    fit <- summary(lm(y ~ 0 + x, data= df))
    std_err <- fit$sigma
    fit <- data.frame(fit$coefficients)
    df_values <- data.frame(y = df$s.v,
                            x = df$db.v, 
                            slope = rep(fit[1,1], len= nrow(df)))
    
    delta <- nrow(df_values) * sum(df_values$x^2) - (sum(df_values$x))^2
    sigma.y <- sqrt(1/(nrow(df_values)-2)*sum((df_values$y - 0 - df_values$x*df_values$slope)^2))
    
    uncert.slope <- sigma.y * sqrt(nrow(df_values)/delta)
    
    results <<- as.data.frame(cbind(slope=fit[1,1], sigma.slope=uncert.slope, std.error=std_err, plot.slope = 1/fit[1,1]))
  }
  
  if (param == "esr") {
    y <- df$s.esr.ft
    x <- df$db.esr.ft
    fit <- summary(lm(y ~ 0 + x, data= df))
    std_err <- fit$sigma
    fit <- data.frame(fit$coefficients)
    df_values <- data.frame(y = df$s.esr.ft,
                            x = df$db.esr.ft, 
                            slope = rep(fit[1,1], len= nrow(df)))
    
    delta <- nrow(df_values) * sum(df_values$x^2) - (sum(df_values$x))^2
    sigma.y <- sqrt(1/(nrow(df_values)-2)*sum((df_values$y - 0 - df_values$x*df_values$slope)^2))
    
    uncert.slope <- sigma.y * sqrt(nrow(df_values)/delta)
    
    results <<- as.data.frame(cbind(slope=fit[1,1], sigma.slope=uncert.slope, std.error=std_err, plot.slope = 1/fit[1,1]))
  }
  print(results)
}



############# slope uncert via Excel 
slope.uncertainty.excel <- function(df, param) {
 if (param == "ft") {
   x <- df$db.ft
   y <- df$s.ft
 }
  
fit <- lm(y ~ 0 + x, data= df)  
df_values <- as.data.frame(cbind(df$s.ft, fit$fitted.values, df$db.ft, rep(mean(df$db.ft), len = nrow(df)))) %>% 
  rename(y = V1, yhat = V2, x = V3,  meanx = V4)  

var.yx <- (1/(nrow(df_values)-2)) *  sum((df_values$y  - df_values$yhat)^2)
ss.xx <- sum((df_values$x-df_values$meanx)^2)

sigma.slope <- sqrt(var.yx/ss.xx)

results <<- as.data.frame(cbind(fit$coefficients, sigma.slope, summary(fit)$sigma))  %>%
  rename(slope = V1, std.err = V3)

print(results)
}
#https://pages.mtu.edu/~fmorriso/cm3215/UncertaintySlopeInterceptOfLeastSquaresFit.pdf