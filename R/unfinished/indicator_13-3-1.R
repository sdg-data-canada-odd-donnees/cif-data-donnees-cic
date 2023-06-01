# 13.3.1 ------------------------------------------------------------------

library(cansim)
library(dplyr)

geocodes <- read.csv("geocodes.csv")

infrastructure_assets <- c(
  "Potable water",
  "Solid waste",
  "Stormwater",
  "Wastewater"
)

table_filters <- function(data) {
  
  data %>% 
    filter(
      `Core public infrastructure assets` %in% infrastructure_assets
    ) %>% 
    select(
      Year = REF_DATE,
      Geography = GEO,
      `Core public infrastructure assets`,
      `Type of municipality by population size`,
      Value = VALUE
    )
  
}

cc_municipalities <- 
  get_cansim("34-10-0277-01", factors = FALSE) %>% 
  table_filters() %>% 
  rename(cc = Value)

all_municipalities <- 
  get_cansim("34-10-0261-01", factors = FALSE) %>% 
  table_filters() %>% 
  rename(total = Value)

municipalities <- 
  cc_municipalities %>% 
  left_join(all_municipalities) %>% 
  mutate(
    Value = round((cc/total)*100, 2),
    Year = as.numeric(Year)
  ) %>% 
  select(-c("cc", "total")) %>% 
  left_join(geocodes) %>% 
  relocate(GeoCode, .before = "Value") %>% 
  mutate_at(2:4, ~ paste0("data.", .x)) %>% 
  rename_at(2:4, ~ paste0("data.", .x))


total_line <- data.frame(
  Year = c(2018, 2020),
  Value = c(51.4, 58.5)
)

data_final <- bind_rows(municipalities, total_line)

write.csv(
  data_final, 
  "data/indicator_13-3-1.csv",
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)