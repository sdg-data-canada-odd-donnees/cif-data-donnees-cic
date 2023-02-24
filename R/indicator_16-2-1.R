# CIF 16.2.1 --------------------------------------------------------------------

#load libraries
library(dplyr)
library(cansim)
library(stringr)

Raw_data <- get_cansim("35-10-0026-01", factors = FALSE)

# load geocode
geocodes <- read.csv("geocodes.csv")

geographies <- c(
  "Canada",
  "Newfoundland and Labrador [10]",
  "Prince Edward Island [11]",
  "Nova Scotia [12]",
  "New Brunswick [13]",
  "Quebec [24]",
  "Ontario [35]",
  "Manitoba [46]",
  "Saskatchewan [47]",
  "Alberta [48]",
  "British Columbia [59]",
  "Yukon [60]",
  "Northwest Territories [61]",
  "Nunavut [62]"
)

crime <-
  Raw_data %>%
  filter(
    REF_DATE >= 2015,
    GEO %in% geographies,
    Statistics %in% c(
      "Crime severity index",
      "Violent crime severity index",
      "Youth crime severity index",
      "Youth violent crime severity index"
    )
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    Index = Statistics,
    Value = VALUE
  ) %>%
  mutate(Geography = str_remove(Geography, " \\[.*\\]")) %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = Value)


# Create the total line and non total line
total <-
  crime %>%
  filter(Geography == "Canada",
         Index == "Crime severity index") %>%
  mutate_at(2:(ncol(.) - 2), ~ "")

non_total <-
  crime %>%
  filter(!(Geography == "Canada" &
             Index == "Crime severity index")) %>%
  mutate_at(2:(ncol(.) - 2), ~ paste0("data.", .x))

# Create the final table and export to csv
final_data <-
  bind_rows(total, non_total) %>%
  rename_at(2:(ncol(.) - 2), ~ paste0("data.", .x))

write.csv(final_data,
          "data/indicator_16-2-1.csv",
          na = "",
          row.names = FALSE)
