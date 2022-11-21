
#13-10-0096-01

# CIF 3.5.1 ---------------------------------------------------------------

# load libraries
library(dplyr)
library(tidyr)
library(cansim)
library(readr)
library(stringr)

# load CODR table from stc api
Raw_data <- get_cansim("13-10-0096-01", factors = FALSE)

# load geocode
geocodes <- read_csv("geocodes.csv")

life_satisfied <- 
  Raw_data %>%
  filter(Indicators == "Life satisfaction, satisfied or very satisfied",
         Characteristics == "Percent") %>%
  select(REF_DATE, GEO, `Age group`, Sex, VALUE) %>%
  rename(Year = REF_DATE, Geography = GEO, Value = VALUE) %>%
  mutate(Geography = recode(Geography,
                            "Canada (excluding territories)" = "Canada")) %>% 
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = Value)


total <- 
  life_satisfied %>%
  filter(Geography == "Canada", 
         `Age group` == "Total, 12 years and over", 
         Sex == "Both sexes") %>%
  mutate_at(2:(ncol(.)-2), ~ "")

non_agg_line <- 
  life_satisfied %>%
  filter(!(Geography == "Canada" &
           `Age group`== "Total, 12 years and over" & 
           Sex == "Both sexes")) %>%
  mutate_at(2:(ncol(.)-2), ~ paste0("data.", .x))

final_data <- 
  bind_rows(total, non_agg_line)

names(final_data)[2:(ncol(final_data)-2)] <- paste0("data.", names(final_data)[2:(ncol(final_data)-2)])

write_csv(final_data, "CIF/data/indicator_3-5-1.csv", na = "")









