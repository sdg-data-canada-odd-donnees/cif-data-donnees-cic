## CIF 14.2.1

library(dplyr)
library(readr)
library(tidyr)

# Status of key fish stocks, Canada, 2011 to 2022

# Get data from source
# To fetch older or newer versions, change the year in the URL
eccc_data_url <- "https://www.canada.ca/content/dam/eccc/documents/csv/cesindicators/status-major-fish-stocks/2024/1_Fish-stocks-year.csv"

fish_stocks <- read_csv(eccc_data_url, skip = 2, show_col_types = FALSE) %>%
  na.omit(fish_stocks) %>%
  # Tidy and filter data
  gather(key = "Status", value = "Value", -Year) %>%
  filter(Status != "Total (number of stocks)") %>%
  # Add units column
  mutate(Units = "Number of stocks") %>%
  relocate(Units, .before = Status) %>%
  # Remove (number of stocks) designation from Stock group
  mutate(Status = substr(Status, 1, nchar(Status)-19))

# Calculate percent of fish stocks in each group
fish_stocks_percentage <- fish_stocks %>%
  mutate(Units = "Percentage") %>%
  group_by(Year) %>%
  mutate(Value = round(Value / sum(Value) * 100, digits = 2))

# Combine percentages and total numbers
final_data <- bind_rows(fish_stocks_percentage,
                        fish_stocks)

# Write data to csv
write.csv(final_data, "data/indicator_14-2-1.csv",
          na = "", row.names = FALSE, fileEncoding = "UTF-8")
