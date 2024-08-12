# CIF 10.2.1

# Load required libraries
library(dplyr)
library(stringr)
library(tidyr)
library(cansim)

# Load cansim table
crime <- get_cansim("35-10-0191-01 ", factors = FALSE)

# Load geocodes from a CSV file
geocodes <- read.csv("geocodes.csv")

crime_filtered <-
  crime %>%
  filter(
    REF_DATE >= 2015,
    Statistics == "Rate per 100,000 population",
    !GEO %in% c(
      "Total Non-Census metropolitan area",
      "Total Census metropolitan area",
      "Canadian Forces Military Police"
    )
  ) %>%
  mutate(
    GEO = str_remove(GEO, " \\[.*\\]")
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    Value = VALUE
  ) %>%
  na.omit() %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = "Value")

total_line <-
  crime_filtered %>%
  filter(
    Geography == "Total police-reported hate crime"
  ) %>%
  mutate(
    Geography = ""
  )

non_total <- 
  crime_filtered %>%
  filter(
    !Geography == "Total police-reported hate crime"
  )

data_final <-
  bind_rows(total_line, non_total)

write.csv(data_final,
          "data/indicator_10-2-1.csv",
          na = "",
          row.names = FALSE,
          fileEncoding = "UTF-8")