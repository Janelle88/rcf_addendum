### Quadrant Month

library(tidyverse)
library(here)
library(rcf)
library(ggrepel)

my_directory <- here::here()
SiteID <- "BAND"

# raw_data <- rcf_data(SiteID = "BAND",
#                      latitude = 35.75758546,
#                      longitude = -106.3054344,
#                      units = "imperial")

raw_data <- read_csv(here::here("BAND.csv"))

thresholds <- calc_thresholds("BAND", data = raw_data, directory = my_directory, units = "imperial")

quadrant_month <- cf_quadrant("BAND", data = thresholds, future_year = 2040, summarize_by = "month", method = "quadrant", directory = my_directory)

means <- read_csv(here::here("BAND_future_means.csv"))

### Scatterplot

ggplot(means, aes(x = tavg_change,
                  y = precip_change,
                  xmin = min(tavg_change) - 0.15 * min(tavg_change),
                  xmax = max(tavg_change) + 0.15 * max(tavg_change),
                  ymin = min(precip_change) - 0.15 * min(precip_change),
                  ymax = max(precip_change) + 0.15 * max(precip_change))) +
  geom_text_repel(aes(label = gcm,
                      color = cf),
                  position = position_jitter(0,.2)) +
  geom_point(size=5,colour="black") +
  geom_point(aes(color = cf),
             size = 4) +
  labs(title = "Changes in climate means centered on 2040 (2025-2055)\n relative to historical period (1950-2000) by GCM run",
       x = "Change in temperature (F)",
       y = "Change in precipitation (in)") +
  scale_color_manual(values = c("gray", "#E10720",  "#8386CC", "darksalmon", "#12045C")) +
  scale_fill_manual(values = c("gray", "#E10720",  "#8386CC", "#darksalmon", "#12045C")) +
  theme_minimal() +
  theme(text = element_text(size = 20))

ggsave(here::here(paste(SiteID, "_quadrant_scatterplot.png",sep=""), width = 15, height = 9))


bar_graph <- function(future1, future2, variable){

  cap_str <- function(y) {
    c <- strsplit(y, "_")[[1]]
    paste(toupper(substring(c, 1,1)), substring(c, 2),
          sep="", collapse=" ")
  }

  variable_name <- ifelse(variable == "tavg", "Average Temperature",
                          ifelse(variable == "precip_yearly", "Precipitation",
                                 ifelse(variable == "tmax", "Maximum Temperature",
                                        ifelse(variable == "tmin", "Minimum Temperature",
                                               ifelse(variable == "rhmin",
                                                      "Minimum Relative Humidity",
                                                      ifelse(variable == "rhmax",
                                                             "Maximum Relative Humidity", cap_str(variable)))))))

  quadrant_month_future <- quadrant_month %>%
    filter(cf %in% c(future1, future2)) %>%
    filter(time %in% c("Future")) %>%
    mutate(month = lubridate::month(month, label = TRUE))

  ggplot(data = quadrant_month_future, aes(x = month, y = .data[[variable]])) +
    geom_col(aes(fill = cf,
                 color = cf),
             position = "dodge",
             alpha = 0.7)  +
    labs(y = variable_name,
         title = paste("Comparison of", variable_name, "between the", future1, "and", future2, "climate futures"))

  ggsave(here::here(paste(SiteID, "_month_", future1, "_", future2, "_", variable, ".png",sep="")), width = 15, height = 9)

}

bar_graph("Warm Wet", "Hot Wet", "tavg")
bar_graph("Warm Wet", "Warm Dry", "heat_index")
bar_graph("Hot Wet", "Hot Dry", "temp_over_99_pctl")
