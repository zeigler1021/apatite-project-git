plot.results <- function(df, x, y, gem, param) {
  
  greens <- c("#C777D6", "#41b6c4", "#004529")
  pinks <- c("#fe9929", "#dd3497", "#49126a")
  
  x.var <- unlist(df[,x])
  y.var <- unlist(df[,y])
  
  if (gem == "geo") {
    p <- ggplot(df, mapping=aes(x = x.var, y = y.var, color = gc), size = 2) + 
      geom_abline(slope=1, intercept = 0) + 
      geom_abline(slope=.95, intercept=0, size= .1, linetype= 2) + #5%
      geom_abline(slope=1.05263, intercept=0, size=.1, linetype= 2) + #5%
      geom_abline(slope=.9, intercept=0, size=.1, linetype= 2) + #10%
      geom_abline(slope=1.1111111, intercept=0, size=.1, linetype= 2) + #10%
      geom_abline(slope=.8, intercept=0, size=.1, linetype= 2) + #20%
      geom_abline(slope=1.25, intercept=0, size=.1, linetype= 2) + #20%
      geom_abline(slope=.7, intercept=0, size=.1, linetype= 2) + #30%
      geom_abline(slope=1.42, intercept=0, size=.1, linetype= 2) + #30%
      geom_abline(slope=.6, intercept=0, size=.1, linetype= 2) + #40%
      geom_abline(slope=1.66, intercept=0, size=.1, linetype= 2) + #40%
      labs(x = "2D", y= "3D") + 
      theme(legend.position = c(.89, .15)) +
      scale_color_manual(values = pinks,
                         name= "Geometric Index",
                         labels= c("A", "B", "C")) +
      geom_point()
  }
  
  if (gem == "rough") {
    p <- ggplot() + 
      geom_abline(slope=1, intercept = 0) + 
      geom_abline(slope=.95, intercept=0, size= .1, linetype= 2) + #5%
      geom_abline(slope=1.05263, intercept=0, size=.1, linetype= 2) + #5%
      geom_abline(slope=.9, intercept=0, size=.1, linetype= 2) + #10%
      geom_abline(slope=1.1111111, intercept=0, size=.1, linetype= 2) + #10%
      geom_abline(slope=.8, intercept=0, size=.1, linetype= 2) + #20%
      geom_abline(slope=1.25, intercept=0, size=.1, linetype= 2) + #20%
      geom_abline(slope=.7, intercept=0, size=.1, linetype= 2) + #30%
      geom_abline(slope=1.42, intercept=0, size=.1, linetype= 2) + #30%
      geom_abline(slope=.6, intercept=0, size=.1, linetype= 2) + #40%
      geom_abline(slope=1.66, intercept=0, size=.1, linetype= 2) + #40%
      labs(x = "2D", y= "3D") + 
      theme(legend.position = c(.89, .15)) +
      scale_color_manual(values = greens, 
                         name = "Roughness Index",
                         labels = c("1", "2", "3")) +
      geom_point(df, mapping=aes(x = x.var, y = y.var, color = as.factor(ri)), size = 2)
  }
  
  if(param == "surface") {
    return(p + xlim(0, 75000) + ylim(0, 75000))
  }
  
  if(param == "esr") {
    return(p)
  }
  
  if(param == "volume") {
    return(p + xlim(0,750000) + ylim(0, 750000))
  }
  
  if(param == "ft") {
    return(p + xlim(.25, .9) + ylim(.25, .9))
  } 
  
}