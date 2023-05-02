# CIF update 12.1.1 -------------------------------------------------------

library(dplyr)
library(cansim)

vehicle_data <- get_cansim("20-10-0021-01", factors = FALSE)
geocodes <- read.csv("geocodes.csv")

selected_fuel_types <- c("All fuel types",
                         "Battery electric",
                         "Plug-in hybrid electric")

all_vehicles <-
  vehicle_data %>%
  filter(
    REF_DATE >= 2015,
    `Vehicle type` == "Total, vehicle type",
    `Fuel type` %in% selected_fuel_types
  ) %>%
  select(Year = REF_DATE,
         Geography = GEO,
         `Fuel type`,
         Value = VALUE) %>%
  mutate(`Fuel type` = ifelse(`Fuel type` == "All fuel types",
                              `Fuel type`,
                              "Electric")) %>%
  group_by(Year, Geography, `Fuel type`) %>%
  summarise(Value = sum(Value, na.rm = TRUE))

all_fuel <-
  all_vehicles %>%
  filter(`Fuel type` == "All fuel types") %>%
  rename(all_fuel = Value)

electric <-
  all_vehicles %>%
  filter(`Fuel type` == "Electric") %>%
  rename(electric = Value)

new_data <-
  left_join(all_fuel, electric, c("Year", "Geography")) %>%
  mutate(Value = round((electric / all_fuel) * 100, 2)) %>%
  select(-starts_with("Fuel")) %>%
  select(-c(3:4)) %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = "Value")

total_line <- 
  new_data %>% 
  filter(Geography == "Canada") %>% 
  mutate(Geography = "")

data_final <- 
  bind_rows(
    total_line,
    new_data %>% 
      filter(Geography != "Canada") %>% 
      mutate(Geography = paste0("data.", Geography))
  ) %>% 
  rename(data.Geography = Geography)

write.csv(data_final,
          "data/indicator_12-1-1.csv",
          na = "",
          row.names = FALSE,
          fileEncoding = "UTF-8")
