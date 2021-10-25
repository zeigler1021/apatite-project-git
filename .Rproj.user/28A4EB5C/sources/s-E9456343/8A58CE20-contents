residual.uncertainty <- function (param, geo) {

if (param == "ft") {
  if (geo == "hex") {
      slope <- hex_results_ft %>% select(slope) %>% as.numeric()
      sample_df <- cbind(select(hex, sample, s.ft, db.ft, ri, size.cat, j.w1), slope)
  }
  if (geo == "ellip") {
    slope <- ellip_results_ft %>% select(slope) %>% as.numeric()
    sample_df <- cbind(select(ellip, sample, s.ft, db.ft, ri, size.cat, j.w1), slope)
  }
  percent_diff <- ((sample_df$s.ft - (sample_df$slope * sample_df$db.ft)) / sample_df$s.ft) * 100 
  #percent.diff = (actual - yhat) / actual * 100
  #(actual - yhat = residual)
}    
  
if (param == "volume") {
  if (geo == "hex") {
      slope <- hex_results_volume %>% select(slope) %>% as.numeric()
      sample_df <- cbind(select(hex, sample, s.v, db.v, ri, size.cat, j.w1), slope)
    }
  if (geo == "ellip") {
      slope <- ellip_results_volume %>% select(slope) %>% as.numeric()
      sample_df <- cbind(select(ellip, sample, s.v, db.v, ri, size.cat, j.w1), slope)
    }
    percent_diff <- ((sample_df$s.v - (sample_df$slope * sample_df$db.v)) / sample_df$s.v) * 100 
}      
  
if (param == "esr") {
  if (geo == "hex") {
      slope <- hex_results_esr %>% select(slope) %>% as.numeric()
      sample_df <- cbind(select(hex, sample, s.esr.ft, db.esr.ft, ri, size.cat, j.w1), slope)
    }
   if (geo == "ellip") {
      slope <- ellip_results_esr %>% select(slope) %>% as.numeric()
      sample_df <- cbind(select(ellip, sample, s.esr.ft, db.esr.ft, ri, size.cat, j.w1), slope)
    }
    percent_diff <- ((sample_df$s.esr.ft - (sample_df$slope * sample_df$db.esr.ft)) / sample_df$s.esr.ft) * 100 
}      
  
sample_df <- cbind(percent_diff, sample_df)

p.rough <- ggplot(sample_df, aes(j.w1, percent_diff)) +
  geom_hline(mapping=aes(yintercept = 0)) +
  geom_point(mapping = aes(color = ri)) +
  scale_color_jco(palette = "default") +
  labs(x = "Maximum Width (µm)", y = "Residuals as a % Difference")

p.size <- ggplot(sample_df, aes(j.w1, percent_diff)) +
  geom_hline(mapping=aes(yintercept = 0)) +
  geom_point(mapping = aes(color = size.cat)) +
  scale_color_jco(palette = "default") +
  labs(x = "Maximum Width (µm)", y = "Residuals as a % Difference")

plots <- list(p.rough, p.size)

rough_results <- tibble(sample_df %>%
         group_by(ri) %>%
         summarise(sd_residuals = sd(percent_diff))) %>% 
  rename(Roughness = ri, Uncertainty = sd_residuals)
size_results <- tibble(sample_df %>%
                         group_by(size.cat) %>%
                         summarise(sd_residuals = sd(percent_diff))) %>% 
  rename(`Size Category` = size.cat, Uncertainty = sd_residuals)
parameter_results <- list(rough_results, size_results)


assign(paste("table", glue("{geo}"), glue("{param}"), sep = "_"), parameter_results, envir = parent.frame())
assign(paste("plots", glue("{geo}"), glue("{param}"), sep = "_"), plots, envir = parent.frame())
assign(paste("uncert_result", glue("{geo}"), glue("{param}"), sep = "_"), sample_df, envir = parent.frame())
}

#################################### OLD

# residual.uncertainty <- function (parameter_df, parameter, group) {
#   #parameter_df --> a, b, c, ri1, ellip, etc. 
#   #parameter --> "ft", "volume", "esr"
#   #group --> "hexagonal", "term1", etc. (ie. the name of the grouping from the results_boot sheet)
# 
#   sample_df <- parameter_df %>%
#     select(sample, gem, gc, ri, np, broken, size.cat, j.w1, s.ft, db.ft, s.v, db.v, db.esr.ft, s.esr.ft)
#   
#   if (parameter == "ft") {
#     slope <- results_boot_ft %>% filter(grouping == group) %>% select(slope) %>% as.numeric()
#     
#     sample_df <- cbind(sample_df, slope)
#     
#     actual <- sample_df$s.ft
#     yhat <- sample_df$slope * sample_df$db.ft
#     resid <- actual - yhat 
#     percent.diff <- (resid / actual) * 100
#   }
#   
#   if (parameter == "volume") {
#     slope <- results_boot_volume %>% filter(grouping == group) %>% select(slope) %>% as.numeric() 
# 
#     sample_df <- cbind(sample_df, slope)
#     
#     actual <- sample_df$s.v
#     yhat <- sample_df$slope * sample_df$db.v
#     resid <- actual - yhat 
#     percent.diff <- (resid / actual) * 100
#   }
#   
#   if (parameter == "esr") {
#     slope <- results_boot_esr %>% filter(grouping == group) %>% select(slope) %>% as.numeric()
#     
#     sample_df <- cbind(sample_df, slope)
#     
#     actual <- sample_df$s.esr.ft
#     yhat <- sample_df$slope * sample_df$db.esr.ft
#     resid <- actual - yhat 
#     percent.diff <- (resid / actual) * 100
#   }
#   
# sample_df <- cbind(percent.diff, sample_df)
# assign(paste("residuals", glue("{parameter}"), glue("{group}"), sep = "_"), sample_df, envir = parent.frame())
#   
# p <- ggplot(sample_df, aes(j.w1, percent.diff)) +
#       geom_hline(mapping=aes(yintercept = 0)) +
#       labs(x = "Maximum Width (µm)", y = "Residuals as a % Difference", title = paste(glue("{parameter}"), "residuals for", glue("{group}"), "grains", sep = " ")) 
#   
#   assign(paste("uncertainty", glue({"parameter"}), glue({"group"}), sep = "_"), (sd(percent.diff)), envir = parent.frame()) 
#   
#   ri_summary <- sample_df %>%
#     select(sample, ri, size.cat, gc, gem, np, percent.diff) %>%
#     group_by(ri) %>%
#     summarise(sd_residuals = sd(percent.diff))
#   
#   size_summary <- sample_df %>%
#     select(sample, ri, size.cat, gc, gem, np, percent.diff) %>%
#     group_by(size.cat) %>%
#     summarise(sd_residuals = sd(percent.diff))
#   
#   print(ri_summary)
#   print(size_summary)
#   print(tibble(sd(percent.diff)))
#   
#   if (parameter == "volume" & group != "ellip") {
#     p <- p + geom_point(mapping= aes(color = ri)) + scale_color_uchicago(palette = "default")
#   } 
#   if (parameter == "ft" & group != "ellip") { 
#     p <- p + geom_point(mapping = aes(color = size.cat)) + scale_color_jco(palette = "default")
#   }
#   if (parameter == "esr" & group != "ellip") {
#     p <- p + geom_point(mapping = aes(color = size.cat)) + scale_color_jco(palette = "default")  
#   }
#   
#   if (group == "ellip") { #parameter == "ft" | parameter == "volume" & 
#     p <- p + geom_point(color = "darkseagreen4") 
#   }
#   
#   return(p)
# }
