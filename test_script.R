## script to test package

library(tidyverse)

my_directory <- "C:/Users/jnchr/Documents/test"
directory <- my_directory
SiteID <- "BAND"
# rcf_data("SCBL",latitude = 41.83476, longitude = -103.707, directory = my_directory)


raw_data <- readr::read_csv('C:/Users/jnchr/Documents/test/site_testing/ANTI/ANTI_janelle.csv')

data <- raw_data

# band_small <- raw_data %>%
#   dplyr::filter(gcm %in% c("bcc-csm1-1.rcp45", "bcc-csm1-1.rcp85", "BNU-ESM.rcp45", "BNU-ESM.rcp85",
#                            "CanESM2.rcp45", "CanESM2.rcp85", "CCSM4.rcp45", "CCSM4.rcp85",
#                            "GFDL-ESM2G.rcp45", "GFDL-ESM2G.rcp85"))
#
# band_small <- dplyr::sample_n(band_small, 2500)
#
# readr::write_csv(band_small, "C:/Users/jnchr/Documents/test/BAND_smaller.csv")

# small_data <- dplyr::sample_n(raw_data, 10000)

# raw_data <- small_data

# readr::write_csv(small_data, "C:/Users/jnchr/Documents/R/rcf/data/BAND_small.csv")

# ----------------
# CALC THRESHOLDS
# ----------------

calc_thresholds("ANTI", data = raw_data, directory = my_directory, units = "imperial")

thresholds <- readr::read_csv('C:/Users/jnchr/Documents/test/BAND_thresholds.csv')
#%>%
#   filter(yr %in% c(1950:2005, 2025:2055))


# ----------------
# CF QUADRANT
# ----------------

# data <- thresholds

cf_quadrant("ANTI", data = thresholds, future_year = 2040, past_years = c(1950, 1999), summarize_by = "year", method = "quadrant", directory = my_directory)

cf_quadrant("ANTI", data = thresholds, future_year = 2040, summarize_by = "month", method = "quadrant", directory = my_directory)

cf_quadrant("ANTI", data = thresholds, future_year = 2040, summarize_by = "season", method = "quadrant", directory = my_directory)

cf_quadrant("ANTI", data = thresholds, future_year = 2040, summarize_by = "year", method = "corner", directory = my_directory)

cf_quadrant("ANTI", data = thresholds, future_year = 2040, summarize_by = "month", method = "corner", directory = my_directory)

cf_quadrant("ANTI", data = thresholds, future_year = 2040, summarize_by = "season", method = "corner", directory = my_directory)

# -------------
# SUMMARIZE FOR PCA
# -------------

summarize_for_pca("ANTI", data = thresholds, future_year = 2040, directory = my_directory)

pca_summary <- readr::read_csv('C:/Users/jnchr/Documents/test/ANTI/ANTI_pca_summary.csv')

# -------------
# CF PCA
# -------------

data <- pca_summary

cf_pca("ANTI", data = pca_summary, variables = "all_threshold", directory = my_directory)

pca_data <- readr::read_csv('C:/Users/jnchr/Documents/test/ANTI/ANTI_future_means_pca.csv')

# -------------
# PCA THRESHOLDS
# -------------

# data <- pca_data
# all_data <- thresholds

pca_thresholds("ANTI", pca_data = pca_data, all_data = thresholds, summarize_by = "year", directory = my_directory)

pca_thresholds("ANTI", pca_data = pca_data, all_data = thresholds,  summarize_by = "month", directory = my_directory)

pca_thresholds("ANTI", pca_data = pca_data, all_data = thresholds,  summarize_by = "season", directory = my_directory)

# variables if needed
# year = 2040
# rh_exists <-  any(names(data) == "rhmin")
# variables = c("precip", "tmin", "tmax", "tavg", "rhmin", "rhmax", "heat_index_ec", "heat_index_dan", "temp_over_95_pctl", "temp_over_99_pctl", "temp_over_95_pctl_length", "temp_under_freeze", "temp_under_freeze_length", "temp_under_5_pctl", "no_precip" , "no_precip_length", "precip_95_pctl", "precip_99_pctl", "precip_moderate", "precip_heavy", "freeze_thaw", "gdd", "gdd_count", "not_gdd_count", "frost", "grow_len")

# write_rds(site, "C:/Users/jnchr/Documents/R/rcf/inst/extdata/BAND_thresholds.rds")
