# CIF 5.4.1 ---------------------------------------------------------------

# load libraries
library(dplyr)
library(cansim)

# load CODR table from stc api
raw_data_2015 <- get_cansim("45-10-0014-01", factors = FALSE) #archived and inactive
raw_data_2022 <- get_cansim("45-10-0104-01", factors = FALSE)

# Remove all data disaggregated in more than one field
filtered_2015 <- raw_data_2015 %>%
  filter(
    `Activity group` == "Unpaid work activities",
    Statistics == "Proportion of day, population",
    # Filter out all rows with more than 1 disaggregated field
    (GEO == "Canada" & `Age group` == "Total, 15 years and over") | 
      (GEO == "Canada" & Sex == "Both sexes") | 
      (`Age group` == "Total, 15 years and over" & Sex == "Both sexes")
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Age group`,
    Gender = Sex,
    GeoCode = GeoUID,
    Value = VALUE
  ) %>%
  # Rename gender variables to align with most recent data
  mutate(Gender = case_match(
    Gender,
    "Both sexes" ~ "Total, all persons",
    "Male" ~ "Men+",
    "Female" ~ "Women+"
    )
  )

filtered_2022 <- raw_data_2022 %>%
  filter(
    `Activity group` == "Unpaid work activities",
    Statistics == "Proportion of day, population"
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Age group`,
    Gender,
    GeoCode = GeoUID,
    Value = VALUE
  )
  
domestic_care <- bind_rows(filtered_2015, filtered_2022) %>%
  # Remove geocode for Canada
  mutate(GeoCode = replace(GeoCode, GeoCode == 11124, NA))

domestic_care$GeoCode <- as.integer(domestic_care$GeoCode)

write.csv(domestic_care, "data/indicator_5-4-1.csv",
          na = "", row.names = FALSE, fileEncoding = "UTF-8")
