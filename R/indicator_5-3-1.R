# CIF 5.3.1 ---------------------------------------------------------------

# load libraries
library(dplyr)
library(cansim)

# load CODR table from stc api
Raw_data <- get_cansim("45-10-0014-01", factors = FALSE)


# load geocode
geocodes <- read.csv("geocodes.csv")


domestic_care <-
  Raw_data %>%
  filter(
    `Activity group` %in% c(
      "Unpaid work activities",
      "Household chores",
      "Care of household children under 18 years",
      "Care of household adults",
      "Shopping for goods or services"
    ),
    Statistics == "Proportion of day, population"
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Activity group`,
    Age = `Age group`,
    Sex,
    Value = VALUE
  ) %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = Value)


final_data <-
  domestic_care %>%
  mutate_at(2:5, ~ paste0("data.", .x)) %>% 
  rename_at(2:5, ~ paste0("data.", .x))


write.csv(final_data,
          "data/indicator_5-3-1.csv",
          na = "",
          row.names = FALSE)
