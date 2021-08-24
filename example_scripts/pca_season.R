### PCA Season

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

thresholds <- calc_thresholds("BAND", data = raw_data, directory = my_directory, units = "imperial")

pca_summary <- summarize_for_pca("BAND", data = thresholds, future_year = 2040, directory = my_directory)

pca_data <- cf_pca("BAND", data = pca_summary, variables = "all_threshold", directory = my_directory)

pca_season <- pca_thresholds("BAND", pca_data = pca_data, all_data = thresholds,  summarize_by = "season", directory = my_directory)

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

  pca_season_future <- pca_season %>%
    filter(pca_type %in% c(future1, future2)) %>%
    filter(time %in% c("Future"))

  ggplot(data = pca_season_future, aes(x = quarter, y = .data[[variable]])) +
    geom_col(aes(fill = pca_type,
                 color = pca_type),
             position = "dodge",
             alpha = 0.7)  +
    labs(y = variable_name,
         title = paste("Comparison of", variable_name, "between the", future1, "and", future2, "climate futures"))

  ggsave(here::here(paste(SiteID, "_season_", future1, "_", future2, "_", variable, "_pca.png",sep="")), width = 15, height = 9)

}

bar_graph("PC1 Max", "PC2 Min", "tavg")
bar_graph("PC1 Max", "PC2 Min", "heat_index")
bar_graph("PC1 Max", "PC2 Min", "temp_over_99_pctl")
