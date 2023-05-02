# CIF 8.1.1 ---------------------------------------------------------------

# load libraries
library(dplyr)
library(cansim)


# load CODR table from stc api
Raw_data <- get_cansim("14-10-0327-01", factors = FALSE)
Raw_data2 <- get_cansim("14-10-0364-01", factors = FALSE) 
Raw_data3 <- get_cansim("14-10-0083-01", factors = FALSE)
Raw_data4 <- get_cansim("14-10-0393-01", factors = FALSE)


# load geocode
geocodes <- read.csv("geocodes.csv")


# Total population
total_pop <-
  Raw_data %>%
  filter(
    REF_DATE >= 2015,
    `Labour force characteristics` == "Unemployment rate"
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    Sex,
    `Age group`,
    Value = VALUE
  ) %>%
  mutate(Population = "Total population") %>%
  relocate(Population, .before = "Sex")


# Add Yukon, NWT and Nunavut 
Territories <- 
  Raw_data4 %>% 
  filter(
    GEO %in% c("Yukon","Northwest Territories", "Nunavut"),
    REF_DATE >= 2015,
    `Labour force characteristics` == "Unemployment rate"
  ) %>% 
  select(
    Year = REF_DATE, 
    Geography = GEO, 
    Value = VALUE
  ) %>% 
  mutate(
    Population = "Total population",
    `Age group` = "15 years and over",
    Sex = "Both sexes"
  )


# Indigenous table 
Indigenous <-
  Raw_data2 %>% 
  filter(
    REF_DATE >= 2015,
    `Indigenous group` == "Indigenous population",
    `Labour force characteristics` == "Unemployment rate",
  ) %>% 
  select(
    Year = REF_DATE,
    Geography = GEO,
    Sex,
    `Age group`,
    Value = VALUE
  ) %>% 
  mutate(Population = "Indigenous population")


# Immigrant status table 
Immigrant <-
  Raw_data3 %>%
  filter(
    REF_DATE >= 2015,
    GEO %in% c(
      "Canada",
      "Atlantic region",
      "Quebec",
      "Ontario",
      "Manitoba",
      "Saskatchewan",
      "Alberta",
      "British Columbia"
    ),
    `Immigrant status` %in% c(
      "Landed immigrants",
      "Immigrants, landed 5 or less years earlier",
      "Immigrants, landed more than 5 to 10 years earlier",
      "Immigrants, landed more than 10 years earlier"
    ),
    `Labour force characteristics` == "Unemployment rate"
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    Population = `Immigrant status`,
    `Age group`,
    Value = VALUE
  ) %>%
  mutate(Sex = "Both sexes")


# Add everything together in one dataframe
Total_unemployment <- 
  bind_rows(total_pop, Territories, Indigenous, Immigrant) %>% 
  left_join(geocodes, by = "Geography") %>% 
  relocate(GeoCode, .before = Value)

# Complete the aggregate and non=aggregate data to get final data 
total <- 
  Total_unemployment %>% 
  filter(
    Geography == "Canada",
    Population == "Total population",
    Sex == "Both sexes",
    `Age group` == "15 years and over"
  ) %>% 
  mutate_at(2:(ncol(.)-2), ~ "")


non_total <- 
  Total_unemployment %>% 
  filter(!(Geography == "Canada"&
           Population == "Total population" &
           Sex == "Both sexes" &
           `Age group` == "15 years and over")) %>% 
  mutate_at(2:(ncol(.)-2), ~ paste0("data.", .x))


final_data <- 
  bind_rows(total, non_total) %>% 
  rename_at(2:(ncol(.)-2), ~ paste0("data.", .x))


write.csv(
  final_data, 
  "data/indicator_8-1-1.csv", 
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)













