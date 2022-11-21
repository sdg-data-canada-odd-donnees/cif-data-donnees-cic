# CIF update 1.1.1 -------------------------------------------------------

library(tidyverse)
library(cansim)

low_income_data <- get_cansim("11-10-0135-01", factors = FALSE)
geocodes <- read_csv("gif-data-processing/geocodes.csv")

new_data <- 
  low_income_data %>%
  filter(
    REF_DATE >= 2015,
    Statistics == "Percentage of persons in low income",
    `Hierarchy for Persons in low income` %in% c("1", "1.2", "1.3", "1.4", "5", "5.6", "5.7", "5.8", "9", "9.10", "9.11", "9.12"),
    `Low income lines` == "Market basket measure, 2018 base"
  ) %>% 
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Persons in low income`,
    Value = VALUE
  ) %>% 
  left_join(geocodes, by = "Geography") %>% 
  relocate(GeoCode, .before = Value)

total_line <-
  new_data %>% 
  filter(Geography == "Canada", `Persons in low income` == "All persons") %>% 
  mutate_at(c(2, 3), ~ "")

final_data <-
  bind_rows(
    total_line,
    new_data %>% 
      filter(!(Geography == "Canada" & `Persons in low income` == "All persons"))
  ) %>% 
  mutate_at(c(2, 3), ~ paste0("data.", .x)) %>%
  rename_at(c(2, 3), ~ paste0("data.", .x))
  

write_csv(final_data, "gif-data-processing/CIF/data/indicator_12-1-1.csv")
