regression_summary <- lapply(regression_list, summarise_posterior)

dfl <- cbind(
  regression_summary[[1]],
  regression_summary[[2]],
  regression_summary[[3]],
  regression_summary[[4]]) %>%
  setNames(make.names(names(.), unique = TRUE)) %>% 
  select(-c(4, 7))

dfr <- cbind(
  regression_summary[[5]],
  regression_summary[[6]],
  regression_summary[[7]],
  regression_summary[[8]]) %>%
  setNames(make.names(names(.), unique = TRUE)) %>% 
  select(-c(4, 7))

tmp <- rbind(dfl, dfr)

names <- tmp$variable

replacements <- c("Left caudal ACC",
                  "Left rostral ACC",
                  "Left lateral OFC",
                  "Left medial OFC",
                  "Left DLPFC",
                  "Right caudal ACC",
                  "Right rostral ACC",
                  "Right lateral OFC",
                  "Right medial OFC",
                  "Right DLPFC")

tmp$variable <- str_replace_all(tmp$variable, names, replacements)
tmp$hemisphere <- as.factor(ifelse(str_detect(tmp$variable, "Left", negate = FALSE), "Left", "Right"))
