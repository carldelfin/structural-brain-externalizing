if (!file.exists(here("analysis/data/correlation_results.rds")) || overwrite == TRUE) {
  
  data_scaled = data %>% 
  mutate_if(is.numeric, scale)

  brms_robust_correlation <- readRDS(here("analysis/scripts/brms/brms_robust_correlation.rds"))
  
  correlation_results <-
    rbind(corr_analysis(data, "general_disinhibition", "nogo_accuracy"),
          corr_analysis(data, "callous_aggression", "nogo_accuracy"),
          corr_analysis(data, "substance_abuse", "nogo_accuracy"),
          corr_analysis(data, "general_disinhibition", "callous_aggression"),
          corr_analysis(data, "general_disinhibition", "substance_abuse"),
          corr_analysis(data, "callous_aggression", "substance_abuse"))
  
  saveRDS(correlation_results, here("analysis/data/correlation_results.rds"))
} else if (file.exists(here("analysis/data/correlation_results.rds"))) {
  correlation_results <- readRDS(here("analysis/data/correlation_results.rds"))
}