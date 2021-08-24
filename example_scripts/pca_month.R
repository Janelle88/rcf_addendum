### PCA Month

library(tidyverse)
library(here)
library(rcf)

my_directory <- here::here()
SiteID <- "BAND"

raw_data <- rcf_data(SiteID = "BAND",
                     latitude = 35.75758546,
                     longitude = -106.3054344,
                     units = "imperial")

thresholds <- calc_thresholds("BAND", data = raw_data, directory = my_directory, units = "imperial")

pca_summary <- summarize_for_pca("BAND", data = thresholds, future_year = 2040, directory = my_directory)

pca_data <- cf_pca("BAND", data = pca_summary, variables = "all_threshold", directory = my_directory)

thresholds_summary <- pca_thresholds("BAND", pca_data = pca_data, all_data = thresholds,  summarize_by = "month", directory = my_directory)