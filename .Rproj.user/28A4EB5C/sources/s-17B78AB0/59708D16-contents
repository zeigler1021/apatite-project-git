#BOOTSTRAPPED SLOPES
#I chose 261 samples because that is the length of the original dataset.

#The uncertainty should be in th 2D values! 

bootstrap.linreg <- function(df) {
  #Bootstrapping 2d and 3d values of my parameter 
  twoD <- df$s.ft
  threeD <- df$db.ft
  
  boots_2d <- lapply(1:261, function(i) sample(twoD, replace=TRUE))
  boots_3d <- lapply(1:261, function(i) sample(threeD, replace=TRUE))
  
  #Get data into a dataframe
  boots_2d <- as.data.frame(do.call(cbind, boots_2d)) 
  boots_3d <- as.data.frame(do.call(cbind, boots_3d)) 
  
  boots_2d <<- boots_2d #a dataframe with nrow= df and ncol=500
  boots_3d <<- boots_3d #a dataframe with nrow= df and ncol=500
  
  slope <- list()  #initalize vector to store slopes
  
  #use the 261 samples inside the linear regression and produce a list of slopes of length 261 
  for (i in 1:261) {
    linreg <- summary(lm(boots_3d[,i] ~ 0 + boots_2d[,i]))
    slope[i] <- linreg$coefficients[1]
  }
  #do summary statistics
  slope <- unlist(slope)
  
  results <<- as.data.frame(cbind(mean(slope), sd(slope))) %>%
    rename(slope = V1, sd_slope = V2)
  return(results)
}
