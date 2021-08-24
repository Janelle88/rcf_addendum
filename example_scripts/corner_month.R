### Corner Month

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

quadrant_month <- cf_quadrant("BAND", data = thresholds, future_year = 2040, summarize_by = "month", method = "corner", directory = my_directory)

means <- read_csv(here::here("BAND_future_means.csv"))

ggplot(means, aes(x = tavg_change,
                  y = precip_change,
                  xmin = min(tavg_change) - 0.15 * min(tavg_change),
                  xmax = max(tavg_change) + 0.15 * max(tavg_change),
                  ymin = min(precip_change) - 0.15 * min(precip_change),
                  ymax = max(precip_change) + 0.15 * max(precip_change))) +
  geom_text_repel(aes(label = gcm,
                      color = corner),
                  position = position_jitter(0,.2)) + 
  geom_point(aes(label = gcm,
                 color = corner),
             size = 5) +
  labs(title = "Changes in climate means centered on 2040 by GCM", 
       x = "Change in temperature (F)", 
       y = "Change in precipitation (in)") + 
  scale_color_manual(values = c("#E10720",  "#12045C", "darksalmon", "#8386CC")) +
  scale_fill_manual(values = c("#E10720",  "#12045C", "darksalmon", "#8386CC")) +
  theme_minimal() +
  theme(text = element_text(size = 20))

ggsave(here::here(paste(SiteID, "_corner_scatterplot.png",sep=""), width = 15, height = 9))