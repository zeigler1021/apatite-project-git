taylor.overlap <- function (df, grouping, param, intercept_info = "fixed") {
  #param should be in the form "grouping_parameter" ie. "ft_gem" or "volume_size"
  
  df <- df %>%
    select(-name) %>%
    pivot_longer(cols = 2:4) %>%
    pivot_wider(names_from = grouping, values_from = value)
  
  slopes <- df %>% select(all_of(grouping)) %>% slice(1) %>% pivot_longer(everything()) %>% select(value)
  #Create df of slopes with grouping of interest
  slopes_grid <- suppressMessages(expand_grid(slopes, slopes, .name_repair = "unique") %>%
                                rename(var1 = value...1, var2 = value...2)) #Use expand_grid on the list
  
  sigmaslopes <- test %>% select(all_of(grouping)) %>% slice(2) %>% pivot_longer(everything()) %>% select(value)
  sigmaslopes_grid <- suppressMessages(expand_grid(sigmaslopes, sigmaslopes, .name_repair = "unique") %>%
                                         rename(var1 = value...1, var2 = value...2)) #Use expand_grid on the list
  
  #Iterate over every combination of grouping value and calculate a p-value
  quad_results <- vector(mode = "logical")
  
  for (i in 1:nrow(slopes_grid)) {
    diff <- abs(slopes_grid$var1[i] - slopes_grid$var2[i])
    uncert <- abs(sqrt(sigmaslopes_grid$var1[i]^2 + sigmaslopes_grid$var2[i]^2))
    
    quad_results[i] <- diff < uncert
    #TRUE = Within 1sigma
    #FALSE = Not within 1sigma
  }
  
  grouping <- grouping
  grouping_grid <- suppressMessages(expand_grid(grouping, grouping, .name_repair = "unique") %>%
                                      rename(var1 = grouping...1, var2 = grouping...2)) #expand grouping variable to dataframe that can be joined with pvalue data for plotting
  
  taylor <- cbind(quad_results, grouping_grid) #combine pvalue and grouping variable
  
  assign(paste("taylor", glue("{param}"), sep = "_"), taylor, envir = parent.frame()) #save results
  print(head(taylor))
  
  taylor_df <- taylor %>%
    mutate(discrete = ifelse(taylor == "FALSE", "Slopes are diff", "Slopes are same")) %>%
    mutate(discrete = ifelse(var1 == var2, "NA", discrete))
  
  intercept <- ifelse(intercept_info == "fixed", "intercept fixed at 0", "intercept not fixed")
  
  p <- ggplot(taylor_df, aes(x = var1, y = var2, fill = discrete)) + 
    geom_tile(color = "black") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust=1)) +
    theme(axis.title = element_blank()) +
    #geom_text(aes(label = round(pvalue,2)), size = 3, color = "black") +
    scale_fill_manual(values = tf_color,
                      breaks = c("Slopes are diff", "Slopes are same", "NA"),
                      name = "Legend") +
    labs(title = paste("Taylor Method:", glue("{params}"), sep = " "), subtitle = "1sigma", glue("{intercept}"))
  
  return(p)
}
