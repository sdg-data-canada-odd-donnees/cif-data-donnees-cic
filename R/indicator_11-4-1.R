# CIF 11.4.1 --------------------------------------------------------------------

#load libraries
library(dplyr)
library(cansim)

Raw_data <- get_cansim("23-10-0286-01", factors = FALSE)

#load geocode
geocodes <- read.csv("geocodes.csv")


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


public_transit <-
  Raw_data %>%
  filter(
    GEO %in% geographies,
    `Demographic, geodemographic and commuting` == "Percentage of population near public transit stop"
  ) %>%
  select(Year = REF_DATE,
         Geography = GEO,
         Value = VALUE) %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = Value) %>%
  mutate(Geography = paste0("data.", Geography)) %>%
  rename(data.Geography = Geography)



write.csv(public_transit,
          "data/indicator_11-4-1.csv",
          na = "",
          row.names = FALSE,
          fileEncoding = "UTF-8")
