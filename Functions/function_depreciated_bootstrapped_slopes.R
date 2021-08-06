#BOOTSTRAPPED SLOPES
#I chose 261 samples because that is the length of the original dataset.

#The uncertainty should be in the 2D values! 

bootstrap.linreg.depreciated <- function(df, param) {
  #if (param == "ft") {
    #twoD <- df$s.ft
    #threeD <- df$db.ft
    #}
  
  #if (param == "volume") {
    twoD <- df$s.v
    threeD <- df$db.v
    
    #Bootstrapping 2d and 3d values of my parameter 
    boots_2d <- lapply(1:261, function(i) sample(twoD, replace=TRUE))
    boots_3d <- lapply(1:261, function(i) sample(threeD, replace=TRUE))
    
    #Get data into a dataframe
    boots_2d <- as.data.frame(do.call(cbind, boots_2d)) 
    boots_3d <- as.data.frame(do.call(cbind, boots_3d)) 
    
    boots_2d <<- boots_2d #a dataframe with nrow= df and ncol=500
    boots_3d <<- boots_3d #a dataframe with nrow= df and ncol=500
    
    slope <- list()  #initalize vector to store slopes
    std_err <- list()  #initalize vector to store std errs
    
    #use the 261 samples inside the linear regression and produce a list of slopes of length 261 
    for (i in 1:261) {
      linreg <- summary(lm(boots_2d[,i] ~ 0 + boots_3d[,i]))
      slope[i] <- linreg$coefficients[1]
      std_err[i] <- linreg$sigma
    }
    
    #do summary statistics
    slope <- unlist(slope)
    std_err <- unlist(std_err)
    
    
    results <<- as.data.frame(cbind(round(mean(slope), 4), round(sd(slope), 4), round(mean(std_err), 4))) %>%
      rename(slope = V1, sd_slope = V2, std_err = V3)
    
    return(results)
  }
  
  
#}

#Switch x and y in the linear regression ie regress 2D on 3D 


########Broken for volume, all testing: 
#bootstrap.test <- function(df) {
twoD <- common_apatite$s.v
threeD <- common_apatite$db.v

#Bootstrapping 2d and 3d values of my parameter 
boots_2d <- lapply(1:261, function(i) sample(twoD, replace=TRUE))
boots_3d <- lapply(1:261, function(i) sample(threeD, replace=TRUE))

#Get data into a dataframe
boots_2d.df <- as.data.frame(do.call(cbind, boots_2d)) 
boots_3d.df <- as.data.frame(do.call(cbind, boots_3d)) 

#rowmean2d <- rowMeans(boots_2d.df)
#rowmean3d <- rowMeans(boots_3d.df)

#initalize vector to store slopes and std errors
slope <- list()  
std_err <- list() 

#use the 261 samples inside the linear regression and produce a list of slopes of length 261 
for (i in 1:261) {
  linreg <- summary(lm(boots_2d.df[,i] ~ 0 + boots_3d.df[,i]))
  slope[i] <- linreg$coefficients[1]
  std_err[i] <- linreg$sigma
}

#do summary statistics
slope <- unlist(slope)
std_err <- unlist(std_err)

slopemean <- mean(slope)
slopesd <- sd(slope)

results <<- as.data.frame(cbind(round(mean(slope), 4), round(sd(slope), 4), round(mean(std_err), 4))) %>%
  rename(slope = V1, sd_slope = V2, std_err = V3)

return(results)
#}

```





