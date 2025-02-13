library(dplyr)

### interference data
tmt_interference_data <- sapply(c("10", "11", "16", "18", "32", "35"),
                                function(x){
                                  tmt_correction <- openxlsx::read.xlsx(paste0("TMT", x, "_correction.xlsx"))
                                  tmt_correction <- tmt_correction %>%
                                    dplyr::select(-2) %>%
                                    dplyr::mutate(Mass.Tag = sub("TMT -", "", Mass.Tag)) %>%
                                    tidyr::gather("rm", "value", -Mass.Tag) %>%
                                    dplyr::select(-rm) %>%
                                    dplyr::mutate(to = sub("\\)", "", sub(".*\\(", "", value)),
                                                  value = as.numeric(sub("%.*", "", value))) %>%
                                    dplyr::filter(!is.na(value));
                                  tmt_correction
                                }, simplify = FALSE)


### save data created
usethis::use_data(tmt_interference_data, overwrite = TRUE)
