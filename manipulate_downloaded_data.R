SiteID <-  "BAND"
df1 <- read_csv(here::here("BAND.csv"))

df <- df1 %>%
  dplyr::mutate(precip = .data$pr/25.4,
                # data source needs to be specified using rlang::.data
                # error `no visible binding for global variable x` will be thrown
                tmax = .data$tasmax * (9/5) - 459.67,
                # decided to not label as tmax_f because it could cause conflicts
                # if want to name as tmax_f will need to incorporate if statements
                # to not cause any issues down the line if users choose "metric"
                tmin = .data$tasmin * (9/5) - 459.67,
                tavg = (.data$tmax + .data$tmin) / 2,
                rhmin = rhsmin,
                rhmax = rhsmax,
                gcm = paste(.data$model, .data$rcp, sep = "."),
                date = as.POSIXlt(.data$date,format="%Y-%m-%d"),
                yr = lubridate::year(date),
                units = "imperial") %>%
  dplyr::select(.data$gcm, .data$date, .data$yr, .data$precip, .data$tmin, .data$tmax, .data$tavg, .data$rhmin, .data$rhmax, .data$units)

readr::write_csv(df, here::here(paste0(SiteID, ".csv"))) #will overwrite the csv with the incorrect formatting
