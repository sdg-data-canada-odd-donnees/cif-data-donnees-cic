# CIF 10.1.1 ---------------------------------------------------------------

# load libraries
library(dplyr)
library(cansim)


# load CODR table from stc api
Raw_data <- get_cansim("11-10-0134-01", factors = FALSE)


# load geocode
geocodes <- read.csv("geocodes.csv")


# Format table
selected_geographies <- c(
  "Canada",
  "Newfoundland and Labrador",
  "Prince Edward Island",
  "Nova Scotia",
  "New Brunswick",
  "Quebec",
  "Ontario",
  "Manitoba",
  "Saskatchewan",
  "Alberta",
  "British Columbia"
)


Gini <-
  Raw_data %>%
  filter(
    REF_DATE >= 2015,
    GEO %in% selected_geographies,
    `Income concept` == "Adjusted after-tax income"
  ) %>%
  select(Year = REF_DATE,
         Geography = GEO,
         Value = VALUE) %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = Value)


# Format the total and non total lines
total <-
  Gini %>%
  filter(Geography == "Canada") %>%
  mutate(Geography = "")


non_total <-
  Gini %>%
  filter(!(Geography == "Canada")) %>%
  mutate(Geography = paste0("data.", Geography))


# Bind the rows together and format the finished table
final_data <-
  bind_rows(total, non_total) %>%
  rename(data.Geography = Geography)


write.csv(
  final_data, 
  "data/indicator_10-1-1.csv", 
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
