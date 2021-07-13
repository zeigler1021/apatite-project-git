
plot.gem <- function(df, param, category, gem, plotting.param) {
  
  theme_update(plot.title = element_text(hjust = 0)) #adjusts theme so that all titles are centered
  theme_update(plot.subtitle= element_text(hjust = 0))
  gem.title <- glue("{gem}")
  
  #Get slope and uncertainty
  category = enquo(category)
  param = enquo(param)
  gem = enquo(gem)
  
  linregresults <- read_excel("./Linear Reg Results.xlsx", sheet="comp") %>%
    filter(category == !!category) %>% #method- ie. which grains did you run in the analysis/what type of analysis?
    filter(parameter == !!param) %>% #ft or volume 
    filter(compare == !!gem) %>% #gem
    select(compare, plot.slope, sigma.slope)
  
  slope <- linregresults$plot.slope
  sigma.slope <- linregresults$sigma.slope
  
  
  
  if (plotting.param == "ft") {
    # x <- as.numeric(unlist(df %>% select(s.ft)))
    # y <- as.numeric(unlist(df %>% select(db.ft)))
    
    slope.r <- round(linregresults$plot.slope, 3)
    error.r <- round(linregresults$sigma.slope, 3)
    
    p <- df %>%
      filter(s.gem == !!gem) %>%
      ggplot(aes(x = s.ft, y= db.ft), size = 2) + 
            xlim(.47, .8) + ylim(.47, .8) +
            geom_abline(slope = 1, intercept = 0, linetype = 2) + 
            geom_abline(slope= slope, intercept= 0, color = "#8560e8") +
            geom_point() + 
            labs(title = glue("{gem.title}"),
                 subtitle = glue("slope = {slope.r}\nstd error = {error.r}"),
                 x= "2D Ft", y= "3D Ft")
    return(p)
  } 
  if (plotting.param == "volume") {
    # x <- as.numeric(unlist(df %>% select(!!s.v)))/10^5
    # y <- as.numeric(unlist(df %>% select(!!db.v)))/10^5
    
    slope.r <- round(linregresults$plot.slope, 3)
    error.r <- round(linregresults$sigma.slope, 3)
    
    p <- df %>%
      filter(s.gem == !!gem) %>%
      ggplot(aes(x = (s.v/10^5), y= (db.v/10^5)), size = 2) + 
      #xlim(.47, .8) + ylim(.47, .8) +
      geom_abline(slope = 1, intercept = 0, linetype = 2) + 
      geom_abline(slope= slope, intercept= 0, color = "#7daefd") +
      geom_point() + 
      labs(title = glue("{gem.title}"),
           subtitle = glue("slope = {slope.r}\nstd error = {error.r}"),
           x= "2D Vol. (10^5)", y= "3D Vol. (10^5)")

    return(p)
  }

}