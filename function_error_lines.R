#Function to set theme and plot error lines for ggplot

error.lines <- function(large.error = FALSE) {
  
  theme_set(theme_light())
  
  if (large.error == FALSE) { 
    p <<- ggplot() + 
      geom_abline(slope=1, intercept = 0) + 
      geom_abline(slope=.95, intercept=0, size= .1, linetype= 2) + #5%
      geom_abline(slope=1.05263, intercept=0, size=.1, linetype= 2) + #5%
      geom_abline(slope=.9, intercept=0, size=.1, linetype= 2) + #10%
      geom_abline(slope=1.1111111, intercept=0, size=.1, linetype= 2) + #10%
      geom_abline(slope=.8, intercept=0, size=.1, linetype= 2) + #20%
      geom_abline(slope=1.25, intercept=0, size=.1, linetype= 2)  #20%
  }
  
  if (large.error == TRUE) { 
    p <<- ggplot() +
      geom_abline(slope=1, intercept = 0) + 
      geom_abline(slope=.95, intercept=0, size= .1, linetype= 2) + #5%
      geom_abline(slope=1.05263, intercept=0, size=.1, linetype= 2) + #5%
      geom_abline(slope=.9, intercept=0, size=.1, linetype= 2) + #10%
      geom_abline(slope=1.1111111, intercept=0, size=.1, linetype= 2) + #10%
      geom_abline(slope=.8, intercept=0, size=.1, linetype= 2) + #20%
      geom_abline(slope=1.25, intercept=0, size=.1, linetype= 2)  #20%
    geom_abline(slope=.7, intercept=0, size=.1, linetype= 2) + #30%
      geom_abline(slope=1.42, intercept=0, size=.1, linetype= 2) + #30%
      geom_abline(slope=.6, intercept=0, size=.1, linetype= 2) + #40%
      geom_abline(slope=1.66, intercept=0, size=.1, linetype= 2)  #40%
  }
}
