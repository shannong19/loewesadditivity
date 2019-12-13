## code to prepare `DATASET` dataset goes here

load("data-raw/cyrpa_ripr.rda")
load("data-raw/rh5_ama1ron2.rda")



usethis::use_data(cyrpa_ripr, overwrite = TRUE)
usethis::use_data(rh5_ama1ron2, overwrite = TRUE)
