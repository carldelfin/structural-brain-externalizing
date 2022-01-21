if (!file.exists(here("analysis/data/data.rds")) || overwrite == TRUE) {
  
  # ------------------------------------------------------------------------------
  #  Read data
  # ------------------------------------------------------------------------------
  
  data_self_report <- readxl::read_excel(here("analysis/data/raw/DISALK_n95_demo.xlsx")) %>%
    mutate(gender = as.factor(gender),
           KOD = as.factor(KOD),
           neurological_disorders = as.factor(do_you_have_or_have_had_any_neurological_disorders_1),
           neurological_disorders_2 = as.character(do_you_have_or_have_had_any_neurological_disorders_2),
           psychiatric_disorders = as.factor(do_you_have_or_have_had_any_psychiatric_disorders_1),
           psychiatric_disorders_2 = as.character(do_you_have_or_have_had_any_psychiatric_disorders_2),
           current_medication = as.factor(are_you_currently_using_any_medication_1),
           current_medication_2 = as.character(are_you_currently_using_any_medication_2)) %>%
    select(KOD,
           age,
           gender,
           years_of_education_total,
           neurological_disorders,
           neurological_disorders_2,
           psychiatric_disorders,
           psychiatric_disorders_2,
           current_medication,
           current_medication_2)

  data_sbm <- read_csv(here("preprocessing/output/sbm/ROI_catROIs_aparc_DK40_thickness.csv")) %>%
    mutate(KOD = as.factor(names)) %>%
    select(-c(names, lunknown, runknown))
  
  data_esi <- readRDS(here("analysis/data/esi_data.rds")) %>%
    mutate(KOD = as.factor(KOD))
  
  callous_aggression_rh_ofc_roi <- read_csv(here("preprocessing/output/sbm/call_agg_pos_corr_rh_ofc.csv"), col_names = FALSE)[[1]]
  
  data <- merge(data_sbm, data_esi, by = "KOD")
  data <- merge(data_self_report, data, by = "KOD")
  data <- cbind(data, callous_aggression_rh_ofc_roi)
  
  data_gng <- read_csv(here("analysis/data/raw/gng_n_59.csv")) %>%
    mutate(KOD = as.factor(KOD))
  
  data <- merge(data, data_gng, by = "KOD")
  
  rm(data_self_report, data_sbm, data_esi, callous_aggression_rh_ofc_roi, data_gng)
  
  data <- data %>%
    select(contains(c(
      # demographics
      "KOD",
      "age",
      "gender",
      "years_of_education",
      "disorders",
      "medication",
      # ESI
      "general_disinhibition",
      "callous_aggression",
      "substance_abuse",
      # gng
      "nogo",
      # cingulate
      "caudalanteriorcingulate",
      "rostralanteriorcingulate",
      # ofc
      "lateralorbitofrontal",
      "medialorbitofrontal",
      # dlpfc
      "rostralmiddlefrontal"))) %>%
    na.omit()
  
  # ------------------------------------------------------------------------------
  #  Save data
  # ------------------------------------------------------------------------------
  
  saveRDS(data, here("analysis/data/data.rds"))
}

data <- readRDS(here("analysis/data/data.rds"))
