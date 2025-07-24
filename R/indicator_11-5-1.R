# CIF 11.5.1 --------------------------------------------------------------------

# load libraries
library(dplyr)
library(tidyr)
library(cansim)
library(readr)
library(stringr)

geographies <- c(
  "Newfoundland and Labrador",
  "St. John's, Newfoundland and Labrador",
  "Prince Edward Island",
  "Nova Scotia",
  "Halifax, Nova Scotia",
  "New Brunswick",
  "Moncton, New Brunswick",
  "Saint John, New Brunswick",
  "Quebec",
  "Montréal, Quebec",
  "Ottawa - Gatineau (Quebec part), Quebec",
  "Québec, Quebec",
  "Saguenay, Quebec",
  "Sherbrooke, Quebec",
  "Trois-Rivières, Quebec",
  "Ontario",
  "Barrie, Ontario",
  "Belleville, Ontario",
  "Brantford, Ontario",
  "Greater Sudbury / Grand Sudbury, Ontario",
  "Guelph, Ontario",
  "Hamilton, Ontario",
  "Kingston, Ontario",
  "Kitchener - Cambridge - Waterloo, Ontario",
  "London, Ontario",
  "Oshawa, Ontario",
  "Ottawa - Gatineau (Ontario part), Ontario",
  "Peterborough, Ontario",
  "St. Catharines - Niagara, Ontario",
  "Thunder Bay, Ontario",
  "Toronto, Ontario",
  "Windsor, Ontario",
  "Manitoba",
  "Winnipeg, Manitoba",
  "Saskatchewan",
  "Regina, Saskatchewan",
  "Saskatoon, Saskatchewan",
  "Alberta",
  "Calgary, Alberta",
  "Edmonton, Alberta",
  "Lethbridge, Alberta",
  "British Columbia",
  "Abbotsford - Mission, British Columbia",
  "Kelowna, British Columbia",
  "Vancouver, British Columbia",
  "Victoria, British Columbia",
  "Yukon",
  "Northwest Territories",
  "Nunavut"
)


# 2016 data

raw_data_2016 <- get_cansim("23-10-0286-01")

data_2016 <- raw_data_2016 %>%
  filter(
    GEO %in% geographies,
    `Demographic, geodemographic and commuting` %in% c(
      "Public transit - percentage of commuters",
      "Active transport - percentage of commuters"
    ),
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Type of commute` = `Demographic, geodemographic and commuting`,
    GeoCode = GeoUID,
    Value = VALUE
  ) %>%
  mutate(
    `Type of commute` =
      recode(`Type of commute`,
        "Public transit - percentage of commuters" = "Public transit",
        "Active transport - percentage of commuters" = "Active transportation"
      ),
    Geography = str_replace_all(
      Geography,
      c(
        ", Newfoundland and Labrador" = " (CMA), N.L.",
        ", Nova Scotia" = " (CMA), N.S.",
        ", New Brunswick" = " (CMA), N.B.",
        ", Prince Edward Island" = " (CMA), P.E.I.",
        ", Quebec" = " (CMA), Que.",
        ", Ontario" = " (CMA), Ont.",
        ", Manitoba" = " (CMA), Man.",
        ", Saskatchewan" = " (CMA), Sask.",
        ", Alberta" = " (CMA), Alta.",
        ", British Columbia" = " (CMA), B.C.",
        "Greater Sudbury / Grand Sudbury" = "Greater Sudbury"
      )
    )
  ) %>%
  # Manual input for 2016 Canada-wide data
  # Source: https://www12.statcan.gc.ca/census-recensement/2016/as-sa/98-200-x/2016029/98-200-x2016029-eng.cfm
  add_row(Year = "2016", Geography = "Canada", `Type of commute` = "Active transportation", Value = 6.9) %>%
  add_row(Year = "2016", Geography = "Canada", `Type of commute` = "Public transit", Value = 12.4)

# Calculate the percentage using sustainable transportation
# Sustainable transportation = Public transit + Active transportation
data_2016_sustainable <- data_2016 %>%
  summarise(Value = sum(Value), .by = c(Year, Geography, GeoCode)) %>%
  mutate(`Type of commute` = "Sustainable transportation")

data_2016_concat <- bind_rows(data_2016_sustainable, data_2016) %>%
  # Set Gender = Total for all 2016 data
  mutate(Gender = "Total") %>%
  # Re-organize dataframe nicely
  # Arrange rows by type of commute , keeping original geography order
  arrange(match(Geography, unique(data_2016$Geography))) %>%
  select(Year, Geography, Gender, `Type of commute`, GeoCode, Value)


# 2021 data

connection.parquet <- get_cansim_connection("98-10-0465-01") %>%
  filter(
    `Time leaving for work (7)` == "Total - Time leaving for work",
    `Industry - Sectors - North American Industry Classification System (NAICS) 2017 (21)` == "Total - Industry - Sectors - North American Industry Classification System (NAICS) 2017",
    `Occupation - Broad category - National Occupational Classification (NOC) 2021 (11)` == "Total - Occupation - Broad category - National Occupational Classification (NOC) 2021",
    `Statistics (3)` == "Count",
    `Main mode of commuting (11A)` %in% c("Total - Main mode of commuting", "Sustainable transportation", "Public transit", "Active transportation")
  ) %>%
  collect_and_normalize()

data_2021 <- connection.parquet %>%
  filter(
    `Commuting duration (7)` == "Total - Commuting duration",
    !grepl("\\(CA\\)", GEO)
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    Gender = `Gender (3)`,
    `Main mode of commuting (11A)`,
    GeoCode = GeoUID,
    Value = VALUE
  ) %>%
  pivot_wider(names_from = `Main mode of commuting (11A)`, values_from = Value) %>%
  mutate(
    # Simplify "Total - Gender" as "Total"
    Gender = case_match(Gender, "Total - Gender" ~ "Total", .default = Gender),
    # Calculate the percentages of each form of sustainable transportation
    `Sustainable transportation` = round(`Sustainable transportation` / `Total - Main mode of commuting` * 100, 2),
    `Public transit` = round(`Public transit` / `Total - Main mode of commuting` * 100, 2),
    `Active transportation` = round(`Active transportation` / `Total - Main mode of commuting` * 100, 2)
  ) %>%
  gather(key = "Type of commute", value = Value, `Sustainable transportation`, `Public transit`, `Active transportation`) %>%
  # Re-organize dataframe nicely
  select(-`Total - Main mode of commuting`) %>%
  arrange(Geography, Gender) %>%
  relocate(GeoCode, .before = Value)


data_final <- bind_rows(data_2016_concat, data_2021) %>%
  # Set headline values
  mutate(
    across(
      c(Geography, Gender, `Type of commute`, GeoCode),
      ~ replace(
        .,
        Geography == "Canada" &
          (Gender == "Total" | is.na(Gender)) &
          `Type of commute` == "Sustainable transportation",
        NA
      )
    )
  )

write.csv(data_final, "data/indicator_11-5-1.csv",
  na = "", row.names = FALSE, fileEncoding = "UTF-8"
)
