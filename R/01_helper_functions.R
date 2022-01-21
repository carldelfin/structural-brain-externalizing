# ------------------------------------------------------------------------------
# Summarise posterior results
# ------------------------------------------------------------------------------

summarise_posterior <- function(model) {
  posterior_results(model) %>%
    mutate_if(is.numeric, round, 2) %>%
    mutate(Estimate = paste0(Estimate, " [", Q5, ", ", Q95, "]"),
           PD = paste0(PD, "%")) %>%
    select(variable, Estimate, PD)
}

# ------------------------------------------------------------------------------
# Correlation analysis
# ------------------------------------------------------------------------------

corr_analysis <- function(data, x, y) {
  
  data_scaled = data %>% 
    mutate_if(is.numeric, scale)
  
  tmp_data = data.frame(
    x = data_scaled[, x],
    y = data_scaled[, y])
  
  model = update(brms_robust_correlation,
                 newdata = tmp_data,
                 seed = seed)
  
  tmp = posterior_summary(model, probs = c(0.05, 0.95), robust = TRUE) %>%
    as.data.frame() %>%
    select(-2) %>%
    slice(-c(1:5, 7))
  
  samples <- posterior_samples(model) %>%
    dplyr::select(6)
  
  # probability of direction (in percent)
  pd_fun <- function(vector) {
    if (sign(median(vector)) == -1) {
      p_direction = round(mean(vector < 0) * 100, 0)
    } else if (sign(median(vector)) == 1) {
      p_direction = round(mean(vector > 0) * 100, 0)
    }
    return(p_direction)
  }
  
  tmp_pd <- sapply(samples, pd_fun) %>%
    reshape2::melt() %>%
    mutate(PD = value,
           variable1 = x,
           variable2 = y) %>%
    select(variable1, variable2, PD)
  
  results = as.data.frame(cbind(tmp_pd, tmp))
  
  return(results)
}

# ------------------------------------------------------------------------------
# Extract posterior results
# ------------------------------------------------------------------------------

posterior_results <- function(model) {
  tmp1 = posterior_summary(model, probs = c(0.05, 0.95), robust = TRUE)
  tmp2 = posterior_summary(model, probs = c(0.17, 0.83), robust = TRUE)
  
  tmp = as.data.frame(cbind(tmp1, tmp2)) %>%
    select(-c(2, 5, 6)) %>%
    slice(-c(1:5, 11:13))
  
  samples <- posterior_samples(model) %>%
    select(-c(1:5, 11:13))
  
  # probability of direction (in percent)
  pd_fun <- function(vector) {
    if (sign(median(vector)) == -1) {
      p_direction = round(mean(vector < 0) * 100, 0)
    } else if (sign(median(vector)) == 1) {
      p_direction = round(mean(vector > 0) * 100, 0)
    }
    return(p_direction)
  }
  
  tmp_pd <- sapply(samples, pd_fun) %>%
    reshape2::melt() %>%
    mutate(PD = value,
           variable = rownames(.)) %>%
    select(variable, PD)
  
  results = as.data.frame(cbind(tmp_pd, tmp))
  
  return(results)
}

# ------------------------------------------------------------------------------
# Coefficient plot
# ------------------------------------------------------------------------------

coef_plot <- function(model, hemisphere, title, axis_text, extend) {
  tmp = posterior_results(model) %>%
    mutate(color = as.factor(ifelse(PD > 89, "yes", "no")),
           color2 = as.factor(cut(PD,
                                  breaks = c(50, 60, 70, 80, 90, Inf),
                                  labels = c("50-60%", "61-70%", "71-80%", "81-90%", "> 90%"))))
  
  colors = c("50-60%" = "#9a9a9a",
             "61-70%" = "#9a9a9a",
             "71-80%" = "#9a9a9a",
             "81-90%" = "#41ab5d",
             "> 90%" = "#004529")
  
  if (hemisphere == "left") {
    
    tmp[, "variable"] <-
      factor(tmp[, "variable"],
             levels = rev(c("b_lcaudalanteriorcingulate",
                            "b_lrostralanteriorcingulate",
                            "b_llateralorbitofrontal",
                            "b_lmedialorbitofrontal",
                            "b_lrostralmiddlefrontal")))
    
    levels(tmp[, "variable"]) <- 
      rev(list("Caudal ACC" = "b_lcaudalanteriorcingulate",
               "Rostral ACC" = "b_lrostralanteriorcingulate",
               "Lateral OFC" = "b_llateralorbitofrontal",
               "Medial OFC" = "b_lmedialorbitofrontal",
               "DLPFC" = "b_lrostralmiddlefrontal"))
  } else if (hemisphere == "right") {
    
    tmp[, "variable"] <-
      factor(tmp[, "variable"],
             levels = rev(c("b_rcaudalanteriorcingulate",
                            "b_rrostralanteriorcingulate",
                            "b_rlateralorbitofrontal",
                            "b_rmedialorbitofrontal",
                            "b_rrostralmiddlefrontal")))
    
    levels(tmp[, "variable"]) <- 
      rev(list("Caudal ACC" = "b_rcaudalanteriorcingulate",
               "Rostral ACC" = "b_rrostralanteriorcingulate",
               "Lateral OFC" = "b_rlateralorbitofrontal",
               "Medial OFC" = "b_rmedialorbitofrontal",
               "DLPFC" = "b_rrostralmiddlefrontal"))
  }
  
  plot <- tmp %>%
    ggplot(aes(y = variable, x = Estimate)) +
    geom_vline(xintercept = 0, linetype = "solid", color = "black") +
    geom_linerange(aes(xmin = Q17, xmax = Q83, color = color2), size = 1.8) +
    geom_linerange(aes(xmin = Q5, xmax = Q95, color = color2), size = 0.8) +
    geom_point(aes(color = color2), size = 2.5) +
    scale_x_continuous(expand = c(0, 0.06),
                       limits = c(-0.8, 0.8),
                       breaks = seq(-0.8, 0.8, 0.4),
                       labels = scales::label_number(accuracy = 0.1)) +
    xlab(expression(beta)) +
    scale_color_manual(values = colors) +
    labs(title = title) +
    theme_classic() +
    theme(legend.text = element_blank(),
          legend.title = element_blank(),
          legend.position = "none",
          axis.text.x = element_text(size = axis_text_size,
                                     color = "black"),
          axis.text.y = element_text(size = axis_text_size,
                                     color = "black",
                                     vjust = 0.5),
          plot.title = element_text(size = title_text_size, hjust = 0.5, color = "black"),
          plot.subtitle = element_text(size = subtitle_text_size, color = "black"),
          axis.title.x = element_text(size = axis_title_size, color = "black",
                                      hjust = 0.5),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank())
  
  if (axis_text == "FALSE") {
    plot <- plot +
      theme(axis.text.y = element_blank())
  }
  
  return(plot)
  
}
