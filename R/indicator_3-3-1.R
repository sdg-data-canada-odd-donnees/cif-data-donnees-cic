# CIF 3.3.1 ---------------------------------------------------------------

# load libraries
library(dplyr)
library(cansim)


# load CODR table from stc api
Raw_data <- get_cansim("13-10-0373-01", factors = FALSE)


body_mass <- 
  Raw_data %>%
  filter(Characteristics == "Percent") %>%
  select(REF_DATE, GEO, Measures, Sex, `Age group`, VALUE) %>%
  rename(Year = REF_DATE, Geography = GEO, Value = VALUE) %>%
  mutate_at(2:(ncol(.)-1), ~ paste0("data.", .x)) %>%
  rename_at(2:(ncol(.)-1), ~ paste0("data.", .x))


write.csv(body_mass,
          "data/indicator_3-3-1.csv",
          na = "",
          row.names = FALSE)

