#CIF 3.13.1

library(dplyr)
library(stringr)
library(tidyr)
library(archive)

opioids <- read.csv(archive_read("https://health-infobase.canada.ca/src/doc/SRHD/HealthInfobase-SubstanceHarmsData.zip", file=2))
geocodes <- read.csv("geocodes.csv")

opioids_filtered <-
  opioids %>%
  filter(
    Substance == "Opioids",
    Time_Period == "By year",
    Unit == "Crude rate",
    Source == "Deaths"
  ) %>%
  select(
    Year = Year_Quarter,
    Geography = Region,
    Specific_Measure,
    Disaggregator,
    `Type of event` = Type_Event,
    Value
  ) %>%
  mutate(
    Value = str_remove_all(Value, "Suppr."),
    Value = str_remove_all(Value, "n/a"),
    Value = as.numeric(Value)
  ) %>%
  na.omit() %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = Value)

filter_for_sex <-
  opioids_filtered %>%
  filter(
    Specific_Measure == "Sex"
  ) %>%
  select(
    Year,
    Geography,
    `Type of event`,
    Sex = Disaggregator,
    GeoCode,
    Value
  )

total_line <-
  opioids_filtered %>%
  filter(
    Geography == "Canada",
    `Type of event` == "Total apparent opioid toxicity deaths"
  ) %>% 
  mutate(Geography = "",
         `Type of event` = "")

non_total <-
  opioids_filtered %>%
  filter(
    !(Geography == "Canada" & `Type of event` == "Total apparent opioid toxicity deaths")
  ) %>%
  filter(
    !(Specific_Measure == "Sex")
  )

data_final <-
  bind_rows(total_line,non_total,filter_for_sex) %>%
  select(
    Year,
    Geography,
    `Type of event`,
    Sex,
    GeoCode,
    Value
  )

write.csv(
  data_final,
  "data/indicator_3-13-1.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
