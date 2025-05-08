# Script to combine data for latest year of CIF 6.4.1
# 6.4.1 Water quality in Canadian rivers
# Source: https://www.canada.ca/en/environment-climate-change/services/environmental-indicators/water-quality-canadian-rivers.html#DSM

# Outputs a temp csv file containing the data to append to indicator_6-4-1.csv

# Manually change the year to match the reference year for the data at the source
data_year <- "2021-2023"

library(dplyr)
library(readr)
library(stringr)
library(tidyr)

url_national <- "https://www.canada.ca/content/dam/eccc/documents/csv/cesindicators/water-quality-canadian-rivers/2025/Figure.1_En-water-quality.csv"
# url_regional <- "https://www.canada.ca/content/dam/eccc/documents/csv/cesindicators/water-quality-canadian-rivers/2025/Figure.2_En-quality-regional.csv"
url_atlantic <- "https://www.canada.ca/content/dam/eccc/documents/csv/cesindicators/water-quality-canadian-rivers/2025/Figure.3_En-quality-Atlantic.csv"
url_greatlakes <- "https://www.canada.ca/content/dam/eccc/documents/csv/cesindicators/water-quality-canadian-rivers/2025/Figure.4_En-quality-GL.csv"
url_hudson <- "https://www.canada.ca/content/dam/eccc/documents/csv/cesindicators/water-quality-canadian-rivers/2025/Figure.5_En-quality-Hudson.csv"
url_mackenzie <- "https://www.canada.ca/content/dam/eccc/documents/csv/cesindicators/water-quality-canadian-rivers/2025/Figure.6_En-quality-Mackenzie.csv"
url_pacific <- "https://www.canada.ca/content/dam/eccc/documents/csv/cesindicators/water-quality-canadian-rivers/2025/Figure.7_En-quality-Pacific.csv"

read_csv_by_landuse_category <- function(url, region_name, year, skip = 2) {
  read_csv(url, skip = skip, show_col_types = FALSE) %>%
    na.omit() %>%
    rename("Land use category" = "Landuse Category") %>%
    gather(key = "Water quality", value = "Value", -`Land use category`) %>%
    mutate(
      Year = year,
      Units = case_when(
        str_detect(`Water quality`, "number of sites") ~ "Number of sites",
        str_detect(`Water quality`, "percentage of sites") ~ "Percentage",
      ),
      Geography = region_name,
      `Water quality` = str_remove(`Water quality`, " \\(.+\\)")
    ) %>%
    relocate(Year, Units, Geography, `Land use category`, `Water quality`, Value)
}

national <- read_csv_by_landuse_category(url_national, "Canada", data_year, 1)
atlantic <- read_csv_by_landuse_category(url_atlantic, "Atlantic Ocean", data_year)
greatlakes <- read_csv_by_landuse_category(url_greatlakes, "Great Lakes and St. Lawrence River", data_year)
hudson <- read_csv_by_landuse_category(url_hudson, "Hudson Bay", data_year)
mackenzie <- read_csv_by_landuse_category(url_mackenzie, "Mackenzie River", data_year)
pacific <- read_csv_by_landuse_category(url_pacific, "Pacific Ocean", data_year)

new_data <- bind_rows(national, atlantic, greatlakes, hudson, mackenzie, pacific)

write.csv(new_data, paste0(data_year, ".csv"), na = "", row.names = FALSE, fileEncoding = "UTF-8")
