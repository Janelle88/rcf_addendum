### PCA Year

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

pca_summary <- summarize_for_pca("BAND", data = thresholds, future_year = 2040, directory = my_directory)

pca_data <- cf_pca("BAND", data = pca_summary, variables = "all_threshold", directory = my_directory)

pca_year <- pca_thresholds("BAND", pca_data = pca_data, all_data = thresholds,  summarize_by = "year", directory = my_directory)

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

  ggplot(data = pca_year, aes(x = time, y = .data[[variable]])) +
    geom_boxplot(aes(color = time,
                     fill = time),
                 alpha = 0.2) +
    geom_jitter(aes(color = time),
                size = 2.5,
                alpha =.6) +
    facet_wrap(~pca_type) +
    labs(y = variable_name,
         title = paste("Yearly", variable_name, "for individual GCMs representative of 4 climate futures"))

  ggsave(here::here(paste(SiteID, "_yearly_", variable, "_c.png",sep="")), width = 15, height = 9)
}

boxplot_graph("precip_yearly")
boxplot_graph("tavg")
boxplot_graph("heat_index")
