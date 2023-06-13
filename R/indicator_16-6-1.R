
# CIF 16.6.1 --------------------------------------------------------------

# load libraries
library(cansim)
library(dplyr)

raw_data <- get_cansim("35-10-0154-01", factors = FALSE)
geocodes <- read.csv("geocodes.csv")

incarceration_rate <- 
  raw_data %>%
  filter(
    substr(REF_DATE, 1, 4) >= 2014,
    `Custodial and community supervision` == "Incarceration rates per 100,000 adults"
  ) %>% 
  select(
    Year = REF_DATE,
    Geography = GEO,
    Value = VALUE
  ) %>% 
  left_join(geocodes) %>% 
  relocate(GeoCode, .before = Value)

total_line <- 
  incarceration_rate %>% 
    filter(
      Geography == "Provinces and Territories"
    ) %>% 
    mutate(
      Geography = ""
    )

not_total_line <- 
incarceration_rate %>% 
  filter(
    Geography != "Provinces and Territories"
  ) %>% 
  mutate(
    Geography = paste0("data.", Geography)
  )

# append total and disaggregates together
data_final <- 
  bind_rows(total_line, not_total_line) %>% 
  rename(data.Geography = Geography)

# write data to csv
write.csv(
  data_final,
  "data/indicator_16-6-1.csv",
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


