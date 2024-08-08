# CIF 9.7.1

# Load required libraries
library(dplyr)
library(stringr)
library(tidyr)
library(cansim)

# Load cansim table
service_life_ratio <- get_cansim("36-10-0611-01", factors = FALSE)

# Load geocodes from a CSV file
geocodes <- read.csv("geocodes.csv")

service_life_ratio_filtered <-
  service_life_ratio %>%
  filter(
    REF_DATE >= 2015,
    Estimate == "Remaining useful life",
    `Asset function`== "All functions"
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    Asset,
    Value = VALUE
  ) %>%
  na.omit() %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = "Value")

total_line <-
  service_life_ratio_filtered %>%
  filter(
    Geography == "Canada",
    Asset == "Total assets"
  ) %>%
  mutate(
    Geography = "",
    Asset = ""
  )

non_total <-
  service_life_ratio_filtered %>%
  filter(
    !(
      Geography == "Canada" & Asset == "Total assets"
    )
  )

data_final <-
  bind_rows(total_line, non_total)

write.csv(data_final,
          "data/indicator_9-7-1.csv",
          na = "",
          row.names = FALSE,
          fileEncoding = "UTF-8")