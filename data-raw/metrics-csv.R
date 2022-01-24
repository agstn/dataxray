## code to prepare `metrics.csv` dataset goes here
metrics <- read.csv("data-raw/metric.csv")
usethis::use_data(metrics, overwrite = TRUE)
