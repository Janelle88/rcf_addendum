### Quadrant Month

library(tidyverse)
library(here)
library(rcf)
library(ggrepel)

my_directory <- "C:/Users/jnchr/Documents/test"
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

### Avg Temp

graphs <- function(future, variable){
  quadrant_month <- quadrant_month %>%
    filter(cf %in% c(future)) %>%
    mutate(month = lubridate::month(month, label = TRUE))

  variable = "tavg"
  future = "Warm Wet"

  ggplot(data = quadrant_month_ww, aes(x = month, y = variable)) +
    geom_col(aes(fill = time,
                 color = time),
             position = "dodge",
             alpha = 0.7) +
    scale_color_manual(values = c("darksalmon", "#E10720")) +
    scale_fill_manual(values = c("darksalmon", "#E10720")) +
    theme_minimal() +
    theme(text = element_text(size = 20),
          legend.title = element_blank(),
          axis.title.x = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    labs(y = ifelse(variable == "tavg", "Average Temperature (F)",
                    "Precipitation (in)"),
         title = ifelse(variable == "tavg", paste("Comparison of historical and future average temperature in the", future, "climate future"),
                                                  paste("Comparison of historical and future precipitation in the", future, "climate future")))

  ggsave(here::here(paste(SiteID, "_", future, "_", variable, ".png",sep=""), width = 15, height = 9))

}

graphs("Warm Wet", "Hot Wet", "tavg")

### Warm Wet

quadrant_month_ww <- quadrant_month %>%
  filter(cf %in% c("Warm Wet")) %>%
  mutate(month = lubridate::month(month, label = TRUE))

ggplot(data = quadrant_month_ww, aes(x = month, y = tavg)) +
  geom_col(aes(fill = time,
               color = time),
           position = "dodge",
           alpha = 0.7) +
  scale_color_manual(values = c("darksalmon", "#E10720")) +
  scale_fill_manual(values = c("darksalmon", "#E10720")) +
  theme_minimal() +
  theme(text = element_text(size = 20),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(y = "Average Temperature (F)",
       title = "Comparison of historical and future average temperature in the 'Warm Wet' climate future")

ggsave(here::here(paste(SiteID, "_warm_wet_tavg.png",sep=""), width = 15, height = 9))

###Warm Dry

quadrant_month_wd <- quadrant_month %>%
  filter(cf %in% c("Warm Dry")) %>%
  mutate(month = lubridate::month(month, label = TRUE))

ggplot(data = quadrant_month_wd, aes(x = month, y = tavg)) +
  geom_col(aes(fill = time,
               color = time),
           position = "dodge",
           alpha = 0.7) +
  scale_color_manual(values = c("darksalmon", "#E10720")) +
  scale_fill_manual(values = c("darksalmon", "#E10720")) +
  theme_minimal() +
  theme(text = element_text(size = 20),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(y = "Average Temperature (F)",
       title = "Comparison of historical and future average temperature in the 'Warm Dry' climate future")

ggsave(here::here(paste(SiteID, "_warm_dry_tavg.png",sep=""), width = 15, height = 9))

### Hot Wet

quadrant_month_hw <- quadrant_month %>%
  filter(cf %in% c("Hot Wet")) %>%
  mutate(month = lubridate::month(month, label = TRUE))

ggplot(data = quadrant_month_hw, aes(x = month, y = tavg)) +
  geom_col(aes(fill = time,
               color = time),
           position = "dodge",
           alpha = 0.7) +
  scale_color_manual(values = c("darksalmon", "#E10720")) +
  scale_fill_manual(values = c("darksalmon", "#E10720")) +
  theme_minimal() +
  theme(text = element_text(size = 20),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(y = "Average Temperature (F)",
       title = "Comparison of historical and future average temperature in the 'Hot Wet' climate future")

ggsave(here::here(paste(SiteID, "_hot_wet_tavg.png",sep=""), width = 15, height = 9))

###Hot Dry

quadrant_month_hd <- quadrant_month %>%
  filter(cf %in% c("Hot Dry")) %>%
  mutate(month = lubridate::month(month, label = TRUE))

ggplot(data = quadrant_month_ww, aes(x = month, y = tavg)) +
  geom_col(aes(fill = time,
               color = time),
           position = "dodge",
           alpha = 0.7) +
  scale_color_manual(values = c("darksalmon", "#E10720")) +
  scale_fill_manual(values = c("darksalmon", "#E10720")) +
  theme_minimal() +
  theme(text = element_text(size = 20),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(y = "Average Temperature (F)",
       title = "Comparison of historical and future average temperature in the 'Warm Wet' climate future")

ggsave(here::here(paste(SiteID, "_warm_wet_tavg.png",sep=""), width = 15, height = 9))


