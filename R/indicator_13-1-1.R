## CIF 13.1.1

library(dplyr)
library(readr)
library(stringr)
library(tidyr)

geocodes <- read.csv("geocodes.csv")

total_emissions <- read.csv("https://data-donnees.az.ec.gc.ca/api/file?path=%2Fsubstances%2Fmonitor%2Fcanada-s-official-greenhouse-gas-inventory%2FB-Economic-Sector%2FEN_GHG_Econ_Can_Prov_Terr.csv")

total_emissions_filtered <-
  total_emissions %>%
  filter(
    !Region == "Northwest Territories and Nunavut",
    Total %in% c("y", " y"), # both "y" and " y" have been used in the past so look for either
    Sector == "",
    Sub.sector == "",
    Sub.sub.sector == ""
  ) %>%
  mutate(
    Value = round(as.numeric(CO2eq) / 1000, 2)
  ) %>%
  select(
    Year,
    Geography = Region,
    Source,
    Value
  ) %>%
  arrange(Year, Geography) %>%
  na.omit()

total_line <-
  total_emissions_filtered %>%
  filter(
    Geography == "Canada",
    Source == "National Inventory Total"
  ) %>%
  mutate(
    Geography = "",
    Source = ""
  )

non_total <-
  total_emissions_filtered %>%
  filter(
    !(
      Geography == "Canada" & Source == "National Inventory Total"
    )
  )

data_final <-
  bind_rows(total_line,non_total) %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = "Value")

write.csv(data_final,
          "data/indicator_13-1-1.csv",
          na = "",
          row.names = FALSE,
          fileEncoding = "UTF-8")
