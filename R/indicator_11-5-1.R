

#23-10-0286-01

# CIF 11.5.1 --------------------------------------------------------------------

#load libraries 
library(dplyr)
library(tidyr)
library(cansim)
library(readr)



Raw_data <- get_cansim("23-10-0286-01", factors = FALSE)

#load geocode 

geocodes <- read_csv("geocodes.csv")



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



commuting <- 
  Raw_data %>% 
  filter(GEO %in% geographies,
         `Demographic, geodemographic and commuting` %in% 
           c("Public transit - percentage of commuters",
             "Active transport - percentage of commuters"),
         ) %>% 
  select(Year = REF_DATE,
         Geography = GEO,
         `Type of commute` = `Demographic, geodemographic and commuting`,
         Value = VALUE) %>% 
  mutate(`Type of commute` = 
           recode(`Type of commute`, 
                  "Public transit - percentage of commuters" = "Public transit",
                  "Active transport - percentage of commuters" = "Active transport")) %>% 
  left_join(geocodes, by = "Geography") %>% 
  relocate(GeoCode, .before = Value) %>% 
  mutate_at(2:(ncol(.)-2), ~ paste0("data.", .x))

write_csv(commuting, "CIF/data/indicator_11-5-1.csv", na = '')













