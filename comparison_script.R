library(tidyverse)

#for(sites in c("ANTI", "ARPO", "CIRO", "HOCU", "ORCA", "RUCA", "SCBL", "TICA", "WHSA")){

site <- "ANTI"

my_directory <- paste0("C:/Users/jnchr/Documents/test/site_testing/", site)


load(paste0("C:/Users/jnchr/Documents/test/site_testing/", site, "/", site, "_init_parsed.RData"))

# run these functions, then run the plot tables script

### Means ###

past_data_j <- Baseline_all %>%
  rename(rhmax = RHmaxCustom,
         rhmin = RHminCustom,
         tmax = TmaxCustom,
         tmin = TminCustom,
         precip = PrecipCustom,
         tavg = TavgCustom,
         gcm = GCM,
         date = Date) %>%
  mutate(yr = lubridate::year(date),
         units = "imperial")

future_data_j <- Future_all %>%
  rename(rhmax = RHmaxCustom,
         rhmin = RHminCustom,
         tmax = TmaxCustom,
         tmin = TminCustom,
         precip = PrecipCustom,
         tavg = TavgCustom,
         gcm = GCM,
         date = Date) %>%
  mutate(yr = lubridate::year(date),
         units = "imperial")

raw_data_j <- past_data_j %>%
  full_join(future_data_j)

readr::write_csv(raw_data_j, paste0("C:/Users/jnchr/Documents/test/site_testing/", site, "/", site, "_janelle.csv"))

load_all()

#calc_thresholds(SiteID = site, data = raw_data_j, directory = my_directory, units = "imperial")

source("C:/Users/jnchr/Documents/R/CCRP_Climate_Futures_v1.0/scripts/Climate-Futures/RSS_MACA_Plot_Table_Creation.R")

thresholds_j_f <- readr::read_csv(paste0("C:/Users/jnchr/Documents/test/site_testing/", site, "/", site, "_thresholds.csv"))

#cf_quadrant(SiteID = site, data = thresholds_j_f, future_year = 2040, past_years = c(1950, 1999), summarize_by = "year", method = "quadrant", directory = my_directory)

future_means_j <- readr::read_csv(paste0("C:/Users/jnchr/Documents/test/site_testing/", site, "/", site, "_future_means.csv"))

future_means_c <- future_means

library(compare)

compare(future_means_c, future_means_j)

library(janitor)

compare_df_cols(future_means_c, future_means_j)

library(diffdf)

mean_differences <- diffdf(future_means_c, future_means_j, file = paste0("C:/Users/jnchr/Documents/test/site_testing/", site, "/", site,"_mean_diff.txt"))

rm(future_means)
rm(future_means_j)
rm(future_means_c)
rm(Future_Means)


### Summary of Threshold values ###

thresholds_j <- readr::read_csv(paste0("C:/Users/jnchr/Documents/test/site_testing/", site, "/", site, "_thresholds.csv")) %>%
  filter(yr %in% c(1950:1999, 2025:2055)) %>%
  select(!c(quarter, grow_length, halfyr, doy)) %>%
  arrange(date, gcm)

compare(thresholds_c, thresholds_j, ignoreColOrder = TRUE)

compare_df_cols(thresholds_c, thresholds_j)
threshold_differences <- diffdf(thresholds_c, thresholds_j, file = paste0("C:/Users/jnchr/Documents/test/site_testing/", site, "/", site,"_threshold_diff.txt"))

# ----------------
# OTHER FUNCTIONS
# ----------------

# cf_quadrant(SiteID = site, data = thresholds_j_f, future_year = 2040, past_years = c(1950, 1999), summarize_by = "month", method = "quadrant", directory = my_directory)
#
# cf_quadrant(SiteID = site, data = thresholds_j_f, future_year = 2040, past_years = c(1950, 1999), summarize_by = "season", method = "quadrant", directory = my_directory)
#
# cf_quadrant(SiteID = site, data = thresholds_j_f, future_year = 2040, past_years = c(1950, 1999), summarize_by = "year", method = "corner", directory = my_directory)
#
# cf_quadrant(SiteID = site, data = thresholds_j_f, future_year = 2040, past_years = c(1950, 1999), summarize_by = "month", method = "corner", directory = my_directory)
#
# cf_quadrant(SiteID = site, data = thresholds_j_f, future_year = 2040, past_years = c(1950, 1999), summarize_by = "season", method = "corner", directory = my_directory)

# check quadrant comparison

f_annual_c <- F_annual %>%
  dplyr::rename(tmax = TmaxCustom,
                tmin = TminCustom,
                precip_yearly = PrecipCustom,
                tavg = TavgCustom,
                gcm = GCM,
                cf = CF,
                yr = Year,
                heat_index_ec = HI.EC,
                heat_index_dan = HI.Dan,
                temp_over_95_pctl = OverHighQ,
                temp_over_99_pctl = Tmax99,
                temp_under_freeze = UnderColdTemp,
                temp_under_5_pctl = UnderLowQ,
                # returns temperature of the 5th quantile
                no_precip = NoPrecip,
                no_precip_length = NoPrecipLength,
                precip_95_pctl = OverPrecip95,
                precip_99_pctl = OverPrecip99,
                precip_moderate = PrecipOver1,
                precip_heavy = PrecipOver2,
                freeze_thaw = FThaw,
                gdd = GDD) %>%
  select(!c(OverHotTemp, RHmean))%>%
  mutate(yr = as.numeric(yr),
         units = "imperial")

#HI, month, season, date, temp_over_95_pctl_length, temp_under_freeze_length, gdd_count, not_gdd_count, frost

annual_cf_summary_c <- f_annual_c %>%
  select(-gcm) %>%
  group_by(cf, yr) %>%
  summarize(precip_yearly = mean(precip_yearly),
         tmin = mean(tmin),
         tmax = mean(tmax),
         tavg = mean(tavg),
         heat_index_ec = mean(heat_index_ec) ,
         heat_index_dan = mean(heat_index_dan),
         temp_over_95_pctl = mean(temp_over_95_pctl),
         temp_over_99_pctl = mean(temp_over_99_pctl),
         temp_under_freeze = mean(temp_under_freeze),
         temp_under_5_pctl = mean(temp_under_5_pctl),
         # returns temperature of the 5th quantile
         no_precip = mean(no_precip),
         no_precip_length = mean(no_precip_length),
         precip_95_pctl = mean(precip_95_pctl),
         precip_99_pctl = mean(precip_99_pctl),
         precip_moderate = mean(precip_moderate),
         precip_heavy = mean(precip_heavy),
         gdd = mean(gdd),
         freeze_thaw = mean(freeze_thaw)) %>%
  filter(yr > 2025)

annual_cf_summary_j_1 <- readr::read_csv((paste0("C:/Users/jnchr/Documents/test/site_testing/", site, "/", site, "_year_summary_q.csv"))) %>%
  filter(yr > 2025)

annual_cf_summary_j_1$cf <- factor(annual_cf_summary_j_1$cf, levels = c("Warm Wet", "Hot Wet",  "Central",  "Warm Damp", "Hot Damp"))

annual_cf_summary_j <- annual_cf_summary_j_1 %>%
  arrange(cf) %>%
  select(!c(heat_index, temp_over_95_pctl_length, temp_under_freeze_length, gdd_count, not_gdd_count, frost, time, rhmin, rhmax, units, grow_length))

quarter_differences <- diffdf(annual_cf_summary_c, annual_cf_summary_j, file = paste0("C:/Users/jnchr/Documents/test/site_testing/", site, "/", site,"_quarters_diff.txt"))

# check model comparison


annual_cf_summary_j_c1 <- readr::read_csv((paste0("C:/Users/jnchr/Documents/test/site_testing/", site, "/", site, "_year_summary_c.csv"))) %>%
  filter(yr > 2025)


annual_cf_summary_j_c <- annual_cf_summary_j_c1 %>%
  arrange(corner) %>%
  select(!c(heat_index, temp_over_95_pctl_length, temp_under_freeze_length, gdd_count, not_gdd_count, frost, time, rhmin, rhmax, units, grow_length, cf))

models <- annual_cf_summary_j_c %>%
  select(gcm, corner) %>%
  summarize(gcm = unique(gcm),
            corner = unique(corner))

annual_cf_summary_c_c <- f_annual_c %>%
  full_join(models, by = "gcm") %>%
  group_by(corner, yr) %>%
  summarize(gcm = unique(gcm),
            precip_yearly = mean(precip_yearly),
            tmin = mean(tmin),
            tmax = mean(tmax),
            tavg = mean(tavg),
            heat_index_ec = mean(heat_index_ec) ,
            heat_index_dan = mean(heat_index_dan),
            temp_over_95_pctl = mean(temp_over_95_pctl),
            temp_over_99_pctl = mean(temp_over_99_pctl),
            temp_under_freeze = mean(temp_under_freeze),
            temp_under_5_pctl = mean(temp_under_5_pctl),
            # returns temperature of the 5th quantile
            no_precip = mean(no_precip),
            no_precip_length = mean(no_precip_length),
            precip_95_pctl = mean(precip_95_pctl),
            precip_99_pctl = mean(precip_99_pctl),
            precip_moderate = mean(precip_moderate),
            precip_heavy = mean(precip_heavy),
            gdd = mean(gdd),
            freeze_thaw = mean(freeze_thaw)) %>%
  filter(yr > 2025) %>%
  drop_na() %>%
  arrange(corner)

corner_differences <- diffdf(annual_cf_summary_c_c, annual_cf_summary_j_c, file = paste0("C:/Users/jnchr/Documents/test/site_testing/", site, "/", site,"_corner_diff.txt"))



# -------------
# SUMMARIZE FOR PCA
# -------------

# summarize_for_pca(SiteID = site, data = thresholds_j_f, future_year = 2040, past_years = c(1950, 1999), directory = my_directory)
#
# pca_summary <- readr::read_csv(paste0("C:/Users/jnchr/Documents/test/site_testing/", site, "/", site, "_pca_summary.csv"))

# -------------
# CF PCA
# -------------

# data <- pca_summary
#
# cf_pca(SiteID = site, data = pca_summary, variables = "all_threshold", directory = my_directory)
#
# pca_data <- readr::read_csv(paste0("C:/Users/jnchr/Documents/test/site_testing/", site, "/", site, "_future_means_pca.csv"))

# -------------
# PCA THRESHOLDS
# -------------

# data <- pca_data
# all_data <- thresholds

pca_thresholds(SiteID = site, pca_data = pca_data, all_data = thresholds_j_f, past_years = c(1950, 1999), summarize_by = "year", directory = my_directory)

pca_thresholds(SiteID = site, pca_data = pca_data, all_data = thresholds_j_f,  past_years = c(1950, 1999), summarize_by = "month", directory = my_directory)

pca_thresholds(SiteID = site, pca_data = pca_data, all_data = thresholds_j_f,  past_years = c(1950, 1999), summarize_by = "season", directory = my_directory)

rm(list = ls())

#}


quadrant_cf_gcm_test_j_mean <- quadrant_cf_gcm %>%
  group_by(gcm, yr) %>%
  filter(yr > 2025) %>%
  summarise(mean_precip = mean(precip) * 365,
            heat_index_ec = sum(heat_index_ec),
            cf = unique(cf)) %>%
  ungroup() %>%
  group_by(cf, yr) %>%
  summarise(mean_precip = mean(mean_precip),
            heat_index_ec = mean(heat_index_ec, na.rm = TRUE))


quadrant_cf_gcm_test_j_numgcm <- quadrant_cf_gcm %>%
  group_by(cf, yr) %>%
  filter(yr > 2025) %>%
  summarise(mean_precip = mean(precip) * 365,
            heat_index_ec = sum(heat_index_ec, na.rm = TRUE) / unique(num_of_gcms))



quadrant_cf_gcm_test_c <- F_annual %>%
  group_by(CF, Year) %>%
  summarize(mean_precip = mean(PrecipCustom),
            heat_index_ec = mean(HI.EC)) %>%
  filter(Year > 2025)



