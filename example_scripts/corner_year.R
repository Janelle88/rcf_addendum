### Corner Year

library(tidyverse)
library(here)
library(rcf)
library(ggrepel)

my_directory <- here::here()
SiteID <- "BAND"

raw_data <- read_csv(here::here("BAND.csv"))

# raw_data <- rcf_data(SiteID = "BAND",
#                      latitude = 35.75758546,
#                      longitude = -106.3054344,
#                      units = "imperial")

thresholds <- calc_thresholds("BAND", data = raw_data, directory = my_directory, units = "imperial")

corner_year <- cf_quadrant("BAND", data = thresholds, future_year = 2040, summarize_by = "year", method = "corner", directory = my_directory)

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

ggsave(here::here(paste(SiteID, "_corner_scatterplot.png",sep="")), width = 15, height = 9)

boxplot_graph <- function(variable){

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

  ggplot(data = corner_year, aes(x = time, y = .data[[variable]])) +
    geom_boxplot(aes(color = time,
                     fill = time),
                 alpha = 0.2) +
    geom_jitter(aes(color = time),
                size = 2.5,
                alpha =.6) +
    facet_wrap(~corner) +
    labs(y = variable_name,
         title = paste("Yearly", variable_name, "for individual GCMs representative of 4 climate futures"))

  ggsave(here::here(paste(SiteID, "_yearly_", variable, "_c.png",sep="")), width = 15, height = 9)
}

boxplot_graph("precip_yearly")
boxplot_graph("tavg")
boxplot_graph("heat_index")
