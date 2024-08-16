## CIF 14.2.1

library(dplyr)
library(readr)
library(tidyr)

# Status of key fish stocks, Canada, 2011 to 2022

# Get data from source
# To fetch older or newer versions, change the year in the URL
eccc_data_url <- "https://www.canada.ca/content/dam/eccc/documents/csv/cesindicators/status-major-fish-stocks/2024/1_Fish-stocks-year.csv"

raw_data <- read_csv(eccc_data_url, skip = 2, show_col_types = FALSE) %>%
  na.omit(raw_data)

fish_stocks <- raw_data %>%
  # Calculate number of fish stocks in cautious and healthy zones
  mutate(`Cautious and healthy zones (number of stocks)` = `Healthy zone (number of stocks)` + `Cautious zone (number of stocks)`)

# Add new columns calculating the percentages in each status category  
for (column in colnames(fish_stocks)[-1]) {
  newcol <- substr(column, 1, nchar(column)-19)
  fish_stocks <- mutate(fish_stocks, "{newcol}" := round(fish_stocks[[column]] / `Total (number of stocks)` * 100, digits = 2))
}

# Tidy final data
final_data <- fish_stocks %>%
  select(!starts_with("Total")) %>%
  gather(key = "Status", value = "Value", -Year) %>%
  # Add units column
  mutate(Units = ifelse(endsWith(Status, "(number of stocks)"), "Number of stocks", "Percentage"),
         Status = ifelse(endsWith(Status, " (number of stocks)"), substr(Status, 1, nchar(Status)-19), Status)
  ) %>%
  relocate(Units, .after = Year) %>%
  # blank out headline data
  mutate(Status = replace(Status, Status == "Cautious and healthy zones", ""))

# Write data to csv
write.csv(final_data, "data/indicator_14-2-1.csv",
          na = "", row.names = FALSE, fileEncoding = "UTF-8")
