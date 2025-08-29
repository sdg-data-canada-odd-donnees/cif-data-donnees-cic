## CIF 14.2.1

library(dplyr)
library(readr)
library(tidyr)

# Status of key fish stocks, Canada, 2011 to 2022

# Get data from source
# To fetch older or newer versions, change the year in the URL
national_data_url <- "https://www.canada.ca/content/dam/eccc/documents/csv/cesindicators/status-major-fish-stocks/2025/1_Status-of-key-fish-stocks.csv"

regions_data_2025_url <- "https://www.canada.ca/content/dam/eccc/documents/csv/cesindicators/status-major-fish-stocks/2025/2_By-region.csv"
regions_data_2024_url <- "https://www.canada.ca/content/dam/eccc/documents/csv/cesindicators/status-major-fish-stocks/2024/2_Fish-stocks-regions.csv"
regions_data_2023_url <- "https://www.canada.ca/content/dam/eccc/documents/csv/cesindicators/status-major-fish-stocks/2023/2_Fish-stocks-regions.csv"

stocks_data_2025_url <- "https://www.canada.ca/content/dam/eccc/documents/csv/cesindicators/status-major-fish-stocks/2025/6_By-species-group.csv"
stocks_data_2024_url <- "https://www.canada.ca/content/dam/eccc/documents/csv/cesindicators/status-major-fish-stocks/2024/3_Fish-stocks-groups.csv"
stocks_data_2023_url <- "https://www.canada.ca/content/dam/eccc/documents/csv/cesindicators/status-major-fish-stocks/2023/3_Fish-stocks-groups.csv"
stocks_data_2022_url <- "https://www.canada.ca/content/dam/eccc/documents/csv/cesindicators/status-major-fish-stocks/2022/Fish-stocks-groups.csv"

national_data <- read_csv(national_data_url, skip = 2, show_col_types = FALSE) %>%
  na.omit()

regions_data_2023 <- read_csv(regions_data_2025_url, skip = 2, show_col_types = FALSE) %>%
  mutate(
    Status = case_when(
      Status == "Healthy Zone" ~ "Healthy zone",
      Status == "Cautious Zone" ~ "Cautious zone",
      Status == "Critical Zone" ~ "Critical zone",
      TRUE ~ Status
    )
  ) %>%
  na.omit() %>%
  mutate(Year = "2023") %>%
  slice(-n())

regions_data_2022 <- read_csv(regions_data_2024_url, skip = 2, show_col_types = FALSE) %>%
  na.omit() %>%
  mutate(Year = "2022")

regions_data_2021 <- read_csv(regions_data_2023_url, skip = 2, show_col_types = FALSE) %>%
  na.omit() %>%
  mutate(Year = "2021")

stocks_data_2023 <- read_csv(stocks_data_2025_url, skip = 2, show_col_types = FALSE) %>%
  select(`Stock group` = `Species group`,
         `Healthy zone (number of stocks)` = `Healthy Zone (number of stocks)`,
         `Cautious zone (number of stocks)` = `Cautious Zone  (number of stocks)`,
         `Critical zone (number of stocks)` = `Critical Zone (number of stocks)`,
         `Status uncertain (number of stocks)`
  ) %>%
  na.omit() %>%
  mutate(Year = "2023")

stocks_data_2022 <- read_csv(stocks_data_2024_url, skip = 2, show_col_types = FALSE) %>%
  select(`Stock group`,
         `Healthy zone (number of stocks)`,
         `Cautious zone (number of stocks)`,
         `Critical zone (number of stocks)`,
         `Status uncertain (number of stocks)`
  ) %>%
  na.omit() %>%
  mutate(Year = "2022")

stocks_data_2021 <- read_csv(stocks_data_2023_url, skip = 2, show_col_types = FALSE) %>%
  select(`Stock group`,
         `Healthy zone (number of stocks)`,
         `Cautious zone (number of stocks)`,
         `Critical zone (number of stocks)`,
         `Status uncertain (number of stocks)`
  ) %>%
  na.omit() %>%
  mutate(Year = "2021")

stocks_data_2020 <- read_csv(stocks_data_2022_url, skip = 2, show_col_types = FALSE) %>%
  select(`Stock group`,
         `Healthy zone (number of stocks)`,
         `Cautious zone (number of stocks)`,
         `Critical zone (number of stocks)`,
         `Status uncertain (number of stocks)`
  ) %>%
  na.omit() %>%
  mutate(Year = "2020")


fish_stocks <- national_data %>%
  # Tidy national data
  select(Year,
         `Healthy zone` = `Healthy Zone (number of stocks)`,
         `Cautious zone` = `Cautious Zone  (number of stocks)`,
         `Critical zone` = `Critical Zone (number of stocks)`,
         `Status uncertain` = `Status uncertain (number of stocks)`,
  ) %>%
  gather(key = "Status", value = "Number of stocks", -Year) %>%
  # Calculate percentages
  group_by(Year) %>%
  mutate(Percentage = `Number of stocks` / sum(`Number of stocks`) * 100) %>%
  # Tidy units
  gather(key = "Units", value = "Value", -Year, -Status) %>%
  relocate(Units, .after = Year) %>%
  # Set region for national fish stocks data to Canada
  mutate(Region = "Canada") %>%
  relocate(Region, .before = Value)

fish_stocks_healthy_and_cautious <- fish_stocks %>%
  filter(Status %in% c("Healthy zone", "Cautious zone")) %>%
  ungroup() %>%
  summarise(Value = sum(Value), .by = c(Year, Units, Region)) %>%
  # blank out headline data
  mutate(Status = NA,
         Region = NA)

regions_stocks <- bind_rows(regions_data_2023, regions_data_2022, regions_data_2021) %>%
  rename_at(vars(ends_with("(number of stocks)")), ~ substr(., 1, nchar(.)-19)) %>%
  gather(key = "Region", value = "Number of stocks", -Year, -Status) %>%
  group_by(Year, Region) %>%
  mutate(Percentage = `Number of stocks` / sum(`Number of stocks`) * 100) %>%
  gather(key = "Units", value = "Value", -Year, -Status, -Region)

regions_stocks_healthy_and_cautious <- regions_stocks %>%
  filter(Status %in% c("Healthy zone", "Cautious zone")) %>%
  ungroup() %>%
  summarise(Value = sum(Value), .by = c(Year, Units, Region)) %>%
  mutate(Status = "Healthy and cautious zones")

stock_groups <- bind_rows(stocks_data_2023, stocks_data_2022, stocks_data_2021, stocks_data_2020) %>%
  rename_at(vars(ends_with("(number of stocks)")), ~ substr(., 1, nchar(.)-19)) %>%
  filter(`Stock group` != "Total") %>%
  gather(key = "Status", value = "Number of stocks", -Year, -`Stock group`) %>%
  group_by(Year, `Stock group`) %>%
  mutate(Percentage = `Number of stocks` / sum(`Number of stocks`) * 100) %>%
  gather(key = "Units", value = "Value", -Year, -Status, -`Stock group`) %>%
  mutate(
    `Stock group` = case_when(
      `Stock group` == "Marine Mammals" ~ "Marine mammals",
      TRUE ~ `Stock group`
    )
  )

stock_groups_healthy_and_cautious <- stock_groups %>%
  filter(Status %in% c("Healthy zone", "Cautious zone")) %>%
  ungroup() %>%
  summarise(Value = sum(Value), .by = c(Year, Units, "Stock group")) %>%
  mutate(Status = "Healthy and cautious zones")

final_data <- bind_rows(fish_stocks,
                        fish_stocks_healthy_and_cautious,
                        regions_stocks,
                        regions_stocks_healthy_and_cautious,
                        stock_groups,
                        stock_groups_healthy_and_cautious) %>%
  relocate(`Stock group`, .before = Value) %>%
  arrange(desc(Units))

# Write data to csv
write.csv(final_data, "data/indicator_14-2-1.csv",
          na = "", row.names = FALSE, fileEncoding = "UTF-8")
