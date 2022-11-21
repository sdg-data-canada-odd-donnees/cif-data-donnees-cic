# CIF update 12.1.1 -------------------------------------------------------

library(tidyverse)
library(cansim)

vehicle_data <- get_cansim("20-10-0021-01", factors = FALSE)
geocodes <- read_csv("gif-data-processing/geocodes.csv")

new_data <-
  vehicle_data %>%
  filter(
    REF_DATE >= 2015,
    `Vehicle type` == "Total, vehicle type",
    `Fuel type` %in% c("All fuel types", "Battery electric", "Plug-in hybrid electric")
  ) %>% 
  select(REF_DATE, GEO, `Fuel type`, VALUE) %>% 
  rename(Year = REF_DATE, Geography = GEO, Value = VALUE) %>% 
  mutate(
    `Fuel type` = ifelse(`Fuel type` == "All fuel types", `Fuel type`, "Electric")
  ) %>%
  group_by(Year, Geography, `Fuel type`) %>% 
  summarise(Value = sum(Value, na.rm = TRUE)) %>% 
  pivot_wider(names_from = `Fuel type`, values_from = Value) %>% 
  mutate(Value = round((Electric / `All fuel types`) * 100, 2)) %>% 
  filter(!is.na(Value)) %>% 
  select(Year, Geography, Value) %>% 
  ungroup() %>% 
  left_join(geocodes, by = "Geography") %>% 
  relocate(GeoCode, .before = "Value") %>% 
  mutate(Geography = paste0("data.", Geography)) %>% 
  rename(data.Geography = Geography)


write_csv(new_data, "gif-data-processing/CIF/tests/data/indicator_12-1-1.csv")
