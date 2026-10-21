library(readr)
elephant_crossing_maha_oya_pinnawala <- read_csv("data-raw/elephant_crossing_maha_oya_pinnawala.csv")
View(elephant_crossing_maha_oya_pinnawala)
usethis::use_data(elephant_crossing_maha_oya_pinnawala, overwrite = TRUE)
