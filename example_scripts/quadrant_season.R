### Quadrant Season

library(tidyverse)
library(here)
library(rcf)

my_directory <- "C:/Users/jnchr/Documents/test"
SiteID <- "BAND"

raw_data <- rcf_data(SiteID = "BAND",
                     latitude = 35.75758546,
                     longitude = -106.3054344,
                     units = "imperial")

thresholds <- calc_thresholds("BAND", data = raw_data, directory = my_directory, units = "imperial")

quadrant_month <- cf_quadrant("BAND", data = thresholds, future_year = 2040, summarize_by = "season", method = "quadrant", directory = my_directory)

means <- read_csv(here::here("BAND_future_means.csv"))

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