#Model Diagnostics Function
mod.diagnostics <- function(modelsum) {
  
  x <- modelsum$model[,1]
  y <- modelsum$model[,-1]
  resid <- modelsum$residuals
  y.est <- modelsum$fitted
  
  df.num <- as.data.frame(cbind(x, y, resid, y.est))
  
  theme_set(theme_linedraw())
  library(patchwork)
  
  #Set binwidth
  bw <- 2 * IQR(resid) / length(resid)^(1/3) #Freedman–Diaconis' choice
  
  #Plot histogram with KDE PDF overlain
  cols <- c("KDE"="#CC0C00FF", "NORM" = "#84BD00FF")
  
  p1 <- ggplot() +
    geom_histogram(df.num, mapping=aes(x=resid, y= ..density..), binwidth = bw, fill = "transparent", color = "black") +
    geom_density(df.num, mapping=aes(x=resid, y= ..density.., color = "KDE")) +
    stat_function(fun = dnorm, args = list(mean = mean(resid), sd = sd(resid)), mapping=aes(color= "NORM")) +
    scale_color_manual(name = NULL, values= cols, labels = c("KDE", "Normal")) +
    theme(legend.position = c(0.2, 0.75),
          legend.text = element_text(size =9)) +
    labs(title= "Distribution of Residuals", subtitle= "Non-parametric PDF Overlay", x= "Residuals", y="Density")
  p1  
  
  #Plot QQPlot of the residuals to check normality
  library(ggpubr)
  
  p2 <- ggqqplot(df.num, x="resid") +
    labs(title= "Normal Q-Q Plot of Residuals", subtitle= "95% Confidence Interval Shaded", x= "Theoretical Quantiles", y= "Sample Quantiles")
  p2  
  
  # plot the Y estimates agains the residuals.. (check homoskedasticy?)
  p4 <- ggplot() +
    geom_point(df.num, mapping = aes(y.est, resid)) +
    geom_hline(aes(yintercept = 0)) +
    labs(title= "Residuals vs. Estimated Values", x= "Estimated Values", y= "Residuals")
  p4
  
  #plot the autocorrelation function - to see if the residuals are related to each other..
  
  auto <- acf(resid, plot = FALSE)
  autodf <- with(auto, data.frame(lag, acf))
  
  p6 <- ggplot(data = autodf, mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0)) +
    geom_hline(aes(yintercept = 1.96/sqrt(length(resid))), linetype=2, color= "#5C88DAFF") +
    geom_hline(aes(yintercept = -1.96/sqrt(length(resid))), linetype=2, color= "#5C88DAFF") +
    labs(title= "Autocorrelation Plot", x= "Lag", y= "ACF") #blue lines indicate where AC is statistically significently different from zero
  p6
  
  p1 + p2 + p4 + p6 
}