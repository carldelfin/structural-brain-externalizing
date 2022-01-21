if (!file.exists(here("analysis/data/esi_data.rds")) || overwrite == TRUE) {
  
  tmp_data <- readxl::read_excel(here("analysis/data/raw/DISALK_n95_ESI.xlsx"), sheet = "Data")

  # temporarily remove ID variable
  KOD <- tmp_data$KOD
  #ID <- data$`Respondent-ID`
  tmp_data <- tmp_data[, -1]
  
  # correct coding so that S=3, s=2, f=1, F=0
  tmp_data <- tmp_data - 1
  tmp_data <- 3 - tmp_data
  
  # correct names
  names(tmp_data) <- c(seq(1:160))
  
  # select items to reverse according to ESI coding manual,
  # so that S=0, s=1, f=2, F=3
  
  rev_items <- c("12", "38", "77", "90", "98", "127", 
                 "14", "43", "59", "78", "106", "125",
                 "35", "75", "110", "140", "159",
                 "111",
                 "5", "100", "121", "146", "148",
                 "55", "91", "109",
                 "31", "102", "122", "137", "150")
  
  # make sure we have no duplicates
  duplicated(rev_items)
  
  # reverse
  tmp_data[, rev_items] <- 3 - tmp_data[, rev_items]
  
  # sum items to total score
  tmp_data$esi_total_score <- rowSums(tmp_data[c(1:160)])
  
  # sum items to factors
  tmp_data$general_disinhibition <- rowSums(tmp_data[c(1, 9, 10, 19, 28, 36, 41, 44, 49, 65, 73, 84, 90, 92, 95, 112, 125, 143, 144, 152)])
  tmp_data$callous_aggression <- rowSums(tmp_data[c(2, 3, 5, 15, 22, 24, 26, 40, 48, 61, 63, 76, 88, 100, 108, 110, 115, 121, 146)])
  tmp_data$substance_abuse <- rowSums(tmp_data[c(7, 8, 23, 27, 33, 37, 42, 50, 52, 67, 82, 89, 91, 96, 105, 109, 137, 150)])
  
  # add KOD and ID back
  tmp_data$KOD <- KOD
  
  # save all item data
  esi_item_data <- tmp_data[, c(1:160, "KOD")]
  
  # remove item data from final data frame
  esi_data <- tmp_data[, -c(1:160)]
  
  # move KOD to first col
  esi_data <- esi_data %>% 
    select(KOD, everything())
  
  data_sbm <- read_csv(here("preprocessing/output/sbm/ROI_catROIs_aparc_DK40_thickness.csv")) %>%
    mutate(KOD = as.factor(names)) %>%
    select(KOD)
  
  esi_item_data <- merge(esi_item_data, data_sbm, by = "KOD")
  esi_data <- merge(esi_data, data_sbm, by = "KOD")
  
  saveRDS(esi_item_data, here("analysis/data/esi_item_data.rds"))
  saveRDS(esi_data, here("analysis/data/esi_data.rds"))
  
  rm(KOD, rev_items, esi_item_data, esi_data, tmp_data, data_sbm)
}