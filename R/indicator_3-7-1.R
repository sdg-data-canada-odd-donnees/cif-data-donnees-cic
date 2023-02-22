
#13-10-0096-01

# CIF 3.7.1 ---------------------------------------------------------------

# load libraries
library(dplyr)
library(tidyr)
library(cansim)
library(readr)

# load CODR table from stc api
Raw_data <- get_cansim("13-10-0096-01", factors = FALSE)

# load geocode
geocodes <- read_csv("geocodes.csv")



mental_health <- 
  Raw_data %>% 
  filter(Characteristics == "Percent", 
         Indicators == "Perceived mental health, very good or excellent") %>% 
  select(REF_DATE, GEO, `Age group`, Sex, VALUE) %>% 
  rename(Year = REF_DATE, Geography = GEO, Value = VALUE) %>% 
  mutate(Geography = recode(Geography,
                            "Canada (excluding territories)" = "Canada")) %>% 
  left_join(geocodes, by = "Geography") %>% 
  relocate(GeoCode, .before = Value)

total <- 
  mental_health %>% 
  filter(Geography == "Canada",
         `Age group` == "Total, 12 years and over",
         Sex == "Both sexes") %>% 
  mutate_at(2:(ncol(.)-2), ~ "")


non_total <- 
  mental_health %>% 
  filter(!(Geography == "Canada" &
           `Age group` == "Total, 12 years and over" &
           Sex == "Both sexes")) %>% 
  mutate_at(2:(ncol(.)-2), ~ paste0("data.", .x))


final_data <-
  bind_rows(total, non_total)

names(final_data)[2:(ncol(final_data)-2)] <- paste0("data.", 
                                                    names(final_data)[2:(ncol(final_data)-2)])

write_csv(final_data, "CIF/data/indicator_3-7-1.csv", na = "")







