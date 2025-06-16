# CIF 5.4.1 ---------------------------------------------------------------

# load libraries
library(cansim)
library(dplyr)
library(stringr)
library(tidyr)

# load CODR table from stc api
raw_data_2015 <- get_cansim("45-10-0014-01", factors = FALSE) # archived and inactive
raw_data_2022 <- get_cansim("45-10-0104-01", factors = FALSE)

filtered_2015 <- raw_data_2015 %>%
  filter(
    str_starts(`Hierarchy for Activity group`, "11"),
    Statistics == "Proportion of day, population"
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Activity group`,
    `Age group`,
    Gender = Sex,
    GeoCode = GeoUID,
    Value = VALUE
  ) %>%
  mutate(
    # Rename gender variables to align with most recent data
    Gender = case_match(
      Gender,
      "Both sexes" ~ "Total, all persons",
      "Male" ~ "Men+",
      "Female" ~ "Women+"
    ),
    # Rename activity group categories to align with most recent data
    `Activity group` = case_match(
      `Activity group`,
      "Household chores" ~ "Unpaid household work",
      "Care of household children under 18 years" ~ "Care of children under 18 years",
      "Care of household adults" ~ "Care of adults",
      # "Shopping for goods or services" ~ "Shopping for goods or services",
      .default = `Activity group`
    )
  )

filtered_2022 <- raw_data_2022 %>%
  filter(
    str_starts(`Hierarchy for Activity group`, "11"),
    Statistics == "Proportion of day, population"
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Activity group`,
    `Age group`,
    Gender,
    GeoCode = GeoUID,
    Value = VALUE
  ) %>%
  mutate(
    # Simplify activity group category names
    `Activity group` = str_remove(`Activity group`, " \\(household or family\\)")
  )

domestic_care <- bind_rows(filtered_2015, filtered_2022) %>%
  mutate(
    # Rename unpaid work activities category for clarity
    `Activity group` = replace(`Activity group`, `Activity group` == "Unpaid work activities", "Total, unpaid work activities"),
    # Remove geocode for Canada
    GeoCode = replace(GeoCode, GeoCode == 11124, NA)
  )

domestic_care$GeoCode <- as.integer(domestic_care$GeoCode)

progress <- domestic_care %>%
  filter(
    Gender != "Total, all persons"
  ) %>%
  pivot_wider(names_from = "Gender", values_from = "Value") %>%
  mutate(
    # Progress is based on the ratio of time spent by women to the time spent by men on unpaid domestic and care work
    # The target ratio is 1.
    # The absolute difference between the actual ratio and the target ratio is taken to account for situations where men spend more time than women (actual ratio < 1), which also represent a gender imbalance.
    # This means that men spending 50% more time than women gives is measured equally to women spending 50% more time than men.
    Progress = 1 + abs(`Women+` / `Men+` - 1),
    Gender = "Total, all persons"
  ) %>%
  select(-`Men+`, -`Women+`)

domestic_care_with_progress <- left_join(domestic_care, progress) %>%
  relocate(Progress, .before = GeoCode) %>%
  select(
    Year,
    `Activity group`,
    Gender,
    `Age group`,
    Geography,
    GeoCode,
    Progress,
    Value
  )

write.csv(domestic_care_with_progress, "data/indicator_5-4-1.csv",
  na = "", row.names = FALSE, fileEncoding = "UTF-8"
)
