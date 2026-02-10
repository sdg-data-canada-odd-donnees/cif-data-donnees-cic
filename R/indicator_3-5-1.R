#CIF 3.5.1

# import libraries
library(dplyr)
library(cansim)
library(stringr)
library(tidyr)

# Retrieve data from CANSIM tables
physical_activity <- get_cansim("13-10-0821-01", factors = FALSE)
physical_activity_2024 <- get_cansim("13-10-0969-01", factors = FALSE)
names(physical_activity)

# Filter and select relevant data
physical_activity_final <-
  physical_activity %>%
  filter(
    REF_DATE >= 2015,
    `Age group` %in% c("Ages 5 to 17", "Ages 18 to 64", "Ages 65 to 79"),
    Categories == "Meeting guidelines",
    Measures == "Canadian 24-Hour Movement Guidelines",
    Characteristics == "Estimate"
  ) %>% 
  select(
    Year = REF_DATE,
    Sex,
    `Age group`,
    Value = VALUE
  ) %>%
  na.omit()

# Filter and select relevant data
physical_activity_2024_final <-
  physical_activity_2024 %>%
  filter(
    REF_DATE >= 2015,
    `Age group` %in% c("Ages 3 to 4", "Ages 5 to 17", "Ages 18 to 64", "Ages 65 to 79"),
    Measures == "Meets Canadian 24-Hour Movement Guidelines",
    Characteristics == "Percent"
  ) %>% 
  select(
    Year = REF_DATE,
    Sex,
    `Age group`,
    Value = VALUE
  ) %>%
  na.omit()

# merge
data_final <-
  bind_rows(physical_activity_final, physical_activity_2024_final)

# Write final data to CSV
write.csv(
  data_final,
  "data/indicator_3-5-1.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)