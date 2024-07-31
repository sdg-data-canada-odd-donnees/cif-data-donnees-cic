# 6.3.1 -------------------------------------------------------------------

# load libraries
library(cansim)
library(dplyr)

# load cansim table
water_use <- get_cansim("38-10-0250-01", factors = FALSE)
population <- get_cansim("17-10-0005-01", factors = FALSE)

# selected sectors
sectors <- c("Total, industries and households",
             "Total, industries",
             "Households")

population_filtered <-
  population %>%
  filter(
    GEO == "Canada",
    Gender == "Total - gender",
    `Age group` == "All ages"
  ) %>%
  select(
    Year = REF_DATE,
    Population = VALUE
  )

# transform data
cubic_metres <- 
  water_use %>% 
  filter(Sector %in% sectors) %>% 
  select(
    Year = REF_DATE,
    Sector,
    Value = VALUE
  ) %>% 
  arrange(Sector, Year) %>%
  mutate(
    Sector = case_when(
      Sector == "Total, industries and households" ~ "",
      Sector == "Total, industries" ~ "Industries",
      TRUE ~ Sector
    ),
    Series = "Total water use",
    Units = "Cubic metres"
  ) %>% 
  group_by(Sector)

# calculate growth rate
growth_rate <- 
  cubic_metres %>% 
  transmute(
    Year, Sector,
    Value = ((Value - lag(Value)) / lag(Value)) * 100,
    Value = round(Value, 1),
    Series = "Water use growth rate",
    Units = "Percentage"
  )

# calculate water use per capita growth rate

households_water_use <-
  water_use %>%
  filter(
    Sector == "Households"
  ) %>% 
  select(
    Year = REF_DATE,
    Sector,
    Household_Value = VALUE
  )

household_water_use_capita <-
  households_water_use %>%
  inner_join(population_filtered) %>% 
  mutate(
    Value = round(Household_Value / Population, 6)
  )

households_growth_rate_per_capita <-
  household_water_use_capita  %>%
  transmute(
    Year, Sector,
    Value = ((Value - lag(Value)) / lag(Value)) * 100,
    Value = round(Value, 1),
    Series = "Water use per capita growth rate",
    Units = "Percentage"
  ) %>%
  mutate(
    Sector = ""
  )

# bind cubic metres data to growth rate data 
data_final <- 
  bind_rows(growth_rate, households_growth_rate_per_capita, cubic_metres) %>% 
  filter(Year >= 2013) %>% 
  ungroup() %>%
  select(
    Year,
    Series,
    Units,
    Sector,
    Value
  )

# write data to csv
write.csv(data_final,
          "data/indicator_6-3-1.csv",
          na = "",
          row.names = FALSE,
          fileEncoding = "UTF-8")
