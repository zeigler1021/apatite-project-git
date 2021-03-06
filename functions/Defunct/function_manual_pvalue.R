
manual.pvalue <- function (df, grouping, param, upper, lower, intercept_info = "not") {
  #param should be in the form "grouping_parameter" ie. "gem_ft" or "size_volume"
  
  slopes <- df %>% select(all_of(grouping)) #Create df of slopes with grouping of interest
  slopes <- as.list(slopes) #Turn that into a list
  grid_df <- suppressMessages(expand_grid(slopes, slopes, .name_repair = "unique") %>%
                                rename(var1 = slopes...1, var2 = slopes...2)) #Use expand_grid on the list
  
  #Iterate over every combination of grouping value and calculate a p-value
  pvalue <- list()
 
  for (i in 1:nrow(grid_df)) {
    pvalue[i] <- sum(grid_df$var1[[i]] > grid_df$var2[[i]])/1001 
  }
  pvalue <- unlist(pvalue)
  
  grouping <- grouping
  grouping_grid <- suppressMessages(expand_grid(grouping, grouping, .name_repair = "unique") %>%
                                      rename(var1 = grouping...1, var2 = grouping...2)) #expand grouping variable to dataframe that can be joined with pvalue data for plotting
  
  pvalue <- cbind(pvalue, grouping_grid) #combine pvalue and grouping variable
  
  assign(paste("pvalue", glue("{param}"), sep = "_"), pvalue, envir = parent.frame()) #save results
  
  pvalue_df <- pvalue %>%
    mutate(discrete = ifelse(pvalue >= glue({upper}) | pvalue <= glue({lower}), "Slopes are diff", "Slopes are same")) %>%
    mutate(discrete = ifelse(var1 == var2, "NA", discrete))
  
  assign(paste("pvalue", glue("{param}"), sep = "_"), pvalue_df, envir = parent.frame())
  
 # intercept <- ifelse(intercept_info == "fixed", "intercept fixed at 0", "intercept not fixed")
  
  # p <- ggplot(pvalue_df, aes(x = var1, y = var2, fill = discrete)) + 
  #   geom_tile(color = "black") +
  #   theme_bw() +
  #   theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust=1)) +
  #   theme(axis.title = element_blank()) +
  #   geom_text(aes(label = round(pvalue,2)), size = 3, color = "black") +
  #   scale_fill_manual(values = tf_color,
  #                     breaks = c("Slopes are diff", "Slopes are same", "NA"),
  #                     name = "Legend") +
  #   labs(title = paste("Bootstrap Method:", glue("{params}"), sep = " "), subtitle = paste(glue("{upper}"), "confidence //", glue("{intercept}")))
  # 
  # return(p)
}
