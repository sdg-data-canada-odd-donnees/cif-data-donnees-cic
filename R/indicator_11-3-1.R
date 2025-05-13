# CIF 11.3.1

library(dplyr)
library(readr)
library(stringr)

# Fetch CESI data
url <- "https://www.canada.ca/content/dam/eccc/documents/csv/cesindicators/population-exposure-air-pollutants/2025/percentage-canadians-below-at-caaqs-2021.csv"

data <- read_csv(url, skip = 2, show_col_types = FALSE) %>%
  na.omit() %>%
  rename(
    "Year" = "Period",
    "Value" = "Proportion of the population where air pollutants were at or below the standards (percentage)"
  ) %>%
  mutate(
    Year = str_replace(Year, " to ", "-")
  )

# Write data to csv
write.csv(
  data,
  "data/indicator_11-3-1.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
