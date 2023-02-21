

#13-10-0373-01

# CIF 3.3.1 ---------------------------------------------------------------

# load libraries
library(dplyr)
library(tidyr)
library(cansim)
library(readr)

# load CODR table from stc api
Raw_data <- get_cansim("13-10-0373-01", factors = FALSE)


body_mass <- 
  Raw_data %>%
  filter(Characteristics == "Percent") %>%
  select(REF_DATE, GEO, Measures, Sex, `Age group`, VALUE) %>%
  rename(Year = REF_DATE, Geography = GEO, Value = VALUE) %>%
  mutate_at(2:(ncol(.)-1), ~ paste0("data.", .x))

names(body_mass)[2:(ncol(body_mass)-1)] <- paste0("data.",names(body_mass)[2:(ncol(body_mass)-1)])

write_csv(body_mass, "CIF/data/indicator_3-3-1.csv", na = "")
