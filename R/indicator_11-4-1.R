# CIF 11.4.1 --------------------------------------------------------------------

#load libraries
library(dplyr)
library(cansim)

proximity_data <- get_cansim("23-10-0286-01", factors = FALSE)
convenient_access <- get_cansim("23-10-0311-01", factors = FALSE)

#load geocode
geocodes <- read.csv("geocodes.csv")

geographies <- c(
  "Canada",
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

proximity_data_filtered <-
  proximity_data %>%
  filter(
    GEO %in% geographies,
    `Demographic, geodemographic and commuting` == "Percentage of population near public transit stop"
  ) %>%
  select(Year = REF_DATE,
         Geography = GEO,
         Value = VALUE)

convenient_access_filtered <-
  convenient_access %>%
  filter(
    `Demographic and geodemographic` == "Percentage of population within 500 metres of a public transit stop"
  ) %>%
  select(Year = REF_DATE,
         Geography = GEO,
         Value = VALUE) %>%
  mutate(
    Geography = case_when(
      Geography == "St. John's" ~ "St. John's, Newfoundland and Labrador",
      Geography == "Halifax" ~ "Halifax, Nova Scotia",
      Geography == "Moncton" ~ "Moncton, New Brunswick",
      Geography == "Saint John" ~ "Saint John, New Brunswick",
      Geography == "Montréal" ~ "Montréal, Quebec",
      Geography == "Ottawa - Gatineau (partie du Québec / Quebec part)" ~ "Ottawa - Gatineau (Quebec part), Quebec",
      Geography == "Québec" ~ "Québec, Quebec",
      Geography == "Saguenay" ~ "Saguenay, Quebec",
      Geography == "Sherbrooke" ~ "Sherbrooke, Quebec",
      Geography == "Trois-Rivières" ~ "Trois-Rivières, Quebec",
      Geography == "Barrie" ~ "Barrie, Ontario",
      Geography == "Belleville - Quinte West" ~ "Belleville, Ontario",
      Geography == "Brantford" ~ "Brantford, Ontario",
      Geography == "Greater Sudbury / Grand Sudbury" ~ "Greater Sudbury / Grand Sudbury, Ontario",
      Geography == "Guelph" ~ "Guelph, Ontario",
      Geography == "Hamilton" ~ "Hamilton, Ontario",
      Geography == "Kingston" ~ "Kingston, Ontario",
      Geography == "Kitchener - Cambridge - Waterloo" ~ "Kitchener - Cambridge - Waterloo, Ontario",
      Geography == "London" ~ "London, Ontario",
      Geography == "Oshawa" ~ "Oshawa, Ontario",
      Geography == "Ottawa - Gatineau (Ontario part / partie de l'Ontario)" ~ "Ottawa - Gatineau (Ontario part), Ontario",
      Geography == "Peterborough" ~ "Peterborough, Ontario",
      Geography == "St. Catharines - Niagara" ~ "St. Catharines - Niagara, Ontario",
      Geography == "Thunder Bay" ~ "Thunder Bay, Ontario",
      Geography == "Toronto" ~ "Toronto, Ontario",
      Geography == "Windsor" ~ "Windsor, Ontario",
      Geography == "Winnipeg" ~ "Winnipeg, Manitoba",
      Geography == "Regina" ~ "Regina, Saskatchewan",
      Geography == "Saskatoon" ~ "Saskatoon, Saskatchewan",
      Geography == "Calgary" ~ "Calgary, Alberta",
      Geography == "Edmonton" ~ "Edmonton, Alberta",
      Geography == "Lethbridge" ~ "Lethbridge, Alberta",
      Geography == "Abbotsford - Mission" ~ "Abbotsford - Mission, British Columbia",
      Geography == "Kelowna" ~ "Kelowna, British Columbia",
      Geography == "Vancouver" ~ "Vancouver, British Columbia",
      Geography == "Victoria" ~ "Victoria, British Columbia",
      TRUE ~ Geography
    )
  ) %>%
  filter(
    Geography %in% geographies
  )

combined <-
  bind_rows(proximity_data_filtered, convenient_access_filtered) %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = Value)

total_line <-
  combined %>%
  filter(
    Geography == "Canada"
  ) %>%
  mutate(
    Geography = ""
  )

non_total <-
  combined %>%
  filter(
    !Geography == "Canada"
  )

data_final <-
  bind_rows(total_line,non_total)

write.csv(data_final,
          "data/indicator_11-4-1.csv",
          na = "",
          row.names = FALSE,
          fileEncoding = "UTF-8")
