if (!file.exists(here("analysis/data/regression_list.rds")) || overwrite == TRUE) {
  
  data_scaled <- data %>%
    mutate_if(is.numeric, scale)

  brms_robust_linear_regression <- readRDS(here("analysis/scripts/brms/brms_robust_linear_regression.rds"))
  
  regression_list <- list(
    
    # left hemisphere
    update(brms_robust_linear_regression,
           newdata = data_scaled,
           seed = seed,
           general_disinhibition ~ 0 + age + gender + years_of_education_total + lcaudalanteriorcingulate + lrostralanteriorcingulate + llateralorbitofrontal + lmedialorbitofrontal + lrostralmiddlefrontal),
    
    update(brms_robust_linear_regression,
           newdata = data_scaled,
           seed = seed,
           callous_aggression ~ 0 + age + gender + years_of_education_total + lcaudalanteriorcingulate + lrostralanteriorcingulate + llateralorbitofrontal + lmedialorbitofrontal + lrostralmiddlefrontal),
    
    update(brms_robust_linear_regression,
           newdata = data_scaled,
           seed = seed,
           substance_abuse ~ 0 + age + gender + years_of_education_total + lcaudalanteriorcingulate + lrostralanteriorcingulate + llateralorbitofrontal + lmedialorbitofrontal + lrostralmiddlefrontal),
    
    update(brms_robust_linear_regression,
           newdata = data_scaled,
           seed = seed,
           nogo_accuracy ~ 0 + age + gender + years_of_education_total + lcaudalanteriorcingulate + lrostralanteriorcingulate + llateralorbitofrontal + lmedialorbitofrontal + lrostralmiddlefrontal),

    # right hemisphere
    update(brms_robust_linear_regression,
                      newdata = data_scaled,
                      seed = seed,
                      general_disinhibition ~ 0 + age + gender + years_of_education_total + rcaudalanteriorcingulate + rrostralanteriorcingulate + rlateralorbitofrontal + rmedialorbitofrontal + rrostralmiddlefrontal),
    
    update(brms_robust_linear_regression,
                      newdata = data_scaled,
                      seed = seed,
                      callous_aggression ~ 0 + age + gender + years_of_education_total + rcaudalanteriorcingulate + rrostralanteriorcingulate + rlateralorbitofrontal + rmedialorbitofrontal + rrostralmiddlefrontal),
    
    update(brms_robust_linear_regression,
                      newdata = data_scaled,
                      seed = seed,
                      substance_abuse ~ 0 + age + gender + years_of_education_total + rcaudalanteriorcingulate + rrostralanteriorcingulate + rlateralorbitofrontal + rmedialorbitofrontal + rrostralmiddlefrontal),
    
    update(brms_robust_linear_regression,
                      newdata = data_scaled,
                      seed = seed,
                      nogo_accuracy ~ 0 + age + gender + years_of_education_total + rcaudalanteriorcingulate + rrostralanteriorcingulate + rlateralorbitofrontal + rmedialorbitofrontal + rrostralmiddlefrontal)
)
  
  saveRDS(regression_list, here("analysis/data/regression_list.rds"))
} else if (file.exists(here("analysis/data/regression_list.rds"))) {
  regression_list <- readRDS(here("analysis/data/regression_list.rds"))
}