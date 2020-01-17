## code to prepare `DATASET` dataset goes here

load("data-raw/cyrpa_ripr.rda")
load("data-raw/rh5_ama1ron2.rda")



usethis::use_data(cyrpa_ripr, overwrite = TRUE)
usethis::use_data(rh5_ama1ron2, overwrite = TRUE)



rh5_rh4 <-read.csv("data-raw/Original GIA data for PLoS Pathog RH5 RH4.csv",
                   header=TRUE, na.strings = c(""," ","  "))

rh5_rh4 <- rh5_rh4 %>% dplyr::mutate(GIA = (GIA1 + GIA2) / 2) %>%
  dplyr::rename(RH4 = AntiRh4.mg.mL,
                RH5 = AntiRH5.mg.mL) %>%
  dplyr::select(RH4, RH5, GIA)

usethis::use_data(rh5_rh4, overwrite = TRUE)
