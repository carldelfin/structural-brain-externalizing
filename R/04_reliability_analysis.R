if (!file.exists(here("analysis/data/reliability_data")) || overwrite == TRUE) {
  esi_item_data <- readRDS(here("analysis/data/esi_item_data.rds")) %>%
    select(-KOD)

  esi_gen_dis_reliability <-
    esi_item_data %>%
    select(c(1, 9, 10, 19, 28, 36, 41, 44, 49, 65, 73, 84, 90, 92, 95, 112, 125, 143, 144, 152)) %>%
    omega(m = .,
          fm = "ml",
          n.factors = 1,
          plot = FALSE,
          lavaan = TRUE)
  
  esi_gen_dis_reliability <- as.data.frame(esi_gen_dis_reliability[c(3, 4)]) %>%
    mutate_if(is.numeric, round, 2)
  
  esi_callous_agg_reliability <-
    esi_item_data %>%
    select(c(2, 3, 5, 15, 22, 24, 26, 40, 48, 61, 63, 76, 88, 100, 108, 110, 115, 121, 146)) %>%
    omega(m = .,
          fm = "ml",
          n.factors = 1,
          plot = FALSE,
          lavaan = TRUE)
  
  esi_callous_agg_reliability <- as.data.frame(esi_callous_agg_reliability[c(3, 4)]) %>%
    mutate_if(is.numeric, round, 2)
  
  esi_sub_reliability <-
    esi_item_data %>%
    select(c(7, 8, 23, 27, 33, 37, 42, 50, 52, 67, 82, 89, 91, 96, 105, 109, 137, 150)) %>%
    omega(m = .,
          fm = "ml",
          n.factors = 1,
          plot = FALSE,
          lavaan = TRUE)
  
  esi_sub_reliability <- as.data.frame(esi_sub_reliability[c(3, 4)]) %>%
    mutate_if(is.numeric, round, 2)

  reliability_data <- rbind(esi_gen_dis_reliability,
                            esi_callous_agg_reliability,
                            esi_sub_reliability) %>%
    mutate(scale = c("General Disinhibition",
                     "Callous-Aggression",
                     "Substance Abuse")) %>%
    select(scale, alpha, omega.tot)
  
  saveRDS(reliability_data, here("analysis/data/reliability_data.rds"))
}