# CIF 5.4.1 ---------------------------------------------------------------

# load libraries
library(dplyr)
library(cansim)

# load CODR table from stc api
raw_data_2015 <- get_cansim("45-10-0014-01", factors = FALSE) #archived and inactive
raw_data_2022 <- get_cansim("45-10-0104-01", factors = FALSE)

domestic_care <- 
  bind_rows(raw_data_2015, raw_data_2022) %>%
  filter(
    `Activity group` == "Unpaid work activities",
    Statistics == "Proportion of day, population"
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Age group`,
    Sex,
    Gender,
    GeoCode = GeoUID,
    Value = VALUE
  ) %>%
  # Combine Sex and Gender into a single column called Gender
  mutate(Gender = if_else(is.na(Gender), Sex, Gender)) %>%
  select(!Sex) %>%
  mutate(
    # Remove Canada geocode
    GeoCode = replace(GeoCode, GeoCode == 11124, NA),
    # Align sex categories in archived table with gender categories in new table
    Gender = case_match(
      Gender,
      "Both sexes" ~ "Total, all persons",
      "Male" ~ "Men+",
      "Female" ~ "Women+",
      .default = Gender
      ),
    # Set headline data
    # across(
    #   c("Geography", "Age group", "Gender"), 
    #   ~ replace(., Geography == "Canada" & `Age group` == "Total, 15 years and over" & Gender == "Total, all persons", NA)
    #   )
    )
  
domestic_care$GeoCode <- as.integer(domestic_care$GeoCode)

write.csv(domestic_care, "data/indicator_5-4-1.csv",
          na = "", row.names = FALSE, fileEncoding = "UTF-8")
