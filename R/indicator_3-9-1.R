#CIF 3.9.1

library(dplyr)
library(cansim)
library(stringr)
library(tidyr)

vaccine_coverage <- get_cansim("13-10-0870-01", factors = FALSE)

geocodes <- read.csv("geocodes.csv")

data_final <-
  vaccine_coverage %>%
  filter(
    REF_DATE >= 2015,
    Characteristics == "Percentage vaccinated"
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Selected diseases` = `Antigen or vaccine`,
    `Target population`,
    Gender = Sex,
    Value = VALUE
  ) %>%
  na.omit() %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = Value)

write.csv(
  data_final,
  "data/indicator_3-9-1.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)