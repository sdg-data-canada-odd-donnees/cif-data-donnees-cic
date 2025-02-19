# CIF 13.3.1 -------------------------------------------------------

library(dplyr)
library(cansim)
library(stringr)
library(tidyr)


# New table: Reference years 2020-present

raw_data <- get_cansim("34-10-0290-01", factors = FALSE)

assets <- c(
  "Total organizations who own the assets",
  "Total of organizations that factor in climate change adaptation or mitigation into decision making",
  "Climate change adaptation",
  "Climate change mitigation",
  "Both adaptation and mitigation"
)

proportion_municipalities_2020_ <- raw_data %>%
  filter(
    `Asset management practices` %in% assets,
    str_detect(`Hierarchy for Public Organizations`, "^5") # filter for all municipalities and categories falling under this category
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Core public infrastructure assets`,
    `Type of municipality by population size` = "Public Organizations",
    `Asset management practices`,
    GeoCode = GeoUID,
    Value = VALUE
  ) %>%
  pivot_wider(names_from = "Asset management practices", values_from = Value) %>%
  mutate(Value = case_when(
    # if denominator is zero, return NA
    `Total organizations who own the assets` == 0 ~ NA,
    # else, calculate percentage of municipal orgs factoring climate change
    # .default = (`Climate change adaptation` + `Both adaptation and mitigation`) / `Total organizations who own the assets` * 100
    .default = round((`Total of organizations that factor in climate change adaptation or mitigation into decision making` - `Climate change mitigation`) / `Total organizations who own the assets` * 100, digits = 2)
    )
  ) %>%
  select(-one_of(assets))



# Archived tables: Reference years 2016-2020

archive1 <- get_cansim("34-10-0277-01", factors = FALSE)
archive2 <- get_cansim("34-10-0261-01", factors = FALSE)

# Get number of municipal organizations who factored climage change adaptation into decision-making
climate_change_adaptation_2016_2018 <- archive1 %>%
  filter(
    REF_DATE < 2020, # exclude 2020 because already included in newer table
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Core public infrastructure assets`,
    `Type of municipality by population size`,
    GeoCode = GeoUID,
    `Count of municipal organizations who factored climate change adaptation into decision-making process` = VALUE
  )

total_municipalities_2016_2018 <- archive2 %>%
  filter(
    REF_DATE < 2020, # exclude 2020 because already included in newer table
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Core public infrastructure assets`,
    `Type of municipality by population size`,
    GeoCode = GeoUID,
    `Total municipalities` = VALUE
  )

proportion_municipalities_2016_2018 <-
  left_join(total_municipalities_2016_2018, climate_change_adaptation_2016_2018) %>%
  mutate(
    Value = case_when(
      # if denominator is zero, return NA
      `Total municipalities` == 0 ~ NA,
      # else, calculate percentage of municipal orgs factoring climate change
      .default = round(`Count of municipal organizations who factored climate change adaptation into decision-making process` / `Total municipalities` * 100, digits = 2)
    )
  ) %>%
  select(
    -`Count of municipal organizations who factored climate change adaptation into decision-making process`,
    -`Total municipalities`
  ) %>%
  mutate(
    # Rename core public infrastructure asset categories to match new table
    # ex: Public transit --> Public transit assets
    `Core public infrastructure assets` = case_when(
      `Core public infrastructure assets` %in% c("Public transit", "Potable water", "Stormwater", "Wastewater", "Solid waste") 
      ~ paste(`Core public infrastructure assets`, "assets"),
      .default = `Core public infrastructure assets`
    )
  )


# Combine archived and new data for all reference years 2016-present

final_data <- bind_rows(proportion_municipalities_2016_2018, proportion_municipalities_2020_)


write.csv(final_data, "data/indicator_13-3-1.csv",
          na = "", row.names = FALSE, fileEncoding = "UTF-8")
