# CIF 3.1.1 ---------------------------------------------------------------

# load libraries
library(dplyr)
library(cansim)
library(tidyr)

# load CODR table from stc api
raw_data <- get_cansim("13-10-0905-01", factors = FALSE)
disag_income_raw <- get_cansim("13-10-0906-01", factors = FALSE)

# load geocode
geocodes <- read.csv("geocodes.csv")

#Format table 

disag_income <-
  disag_income_raw %>%
  filter(
    Indicators == "Fruit and vegetable consumption, 5 times or more per day",
    Characteristics == "Percent"
  ) %>%
  mutate(
    `Age group` = "Total, 18 years and over",
    `Household income` = case_when(
      `Selected characteristic` == "Household income, first quintile" ~ "First quintile",
      `Selected characteristic` == "Household income, second quintile" ~ "Second quintile",
      `Selected characteristic` == "Household income, third quintile" ~ "Third quintile",
      `Selected characteristic` == "Household income, fourth quintile" ~ "Fourth quintile",
      `Selected characteristic` == "Household income, fifth quintile" ~ "Fifth quintile"
    ),
    `Highest level of education` = case_when(
      `Selected characteristic` == "Highest level of education, less than secondary school graduation" ~ "Less than secondary school graduation",
      `Selected characteristic` == "Highest level of education, secondary school graduation, no post-secondary education" ~ "Secondary school graduation, no post-secondary education",
      `Selected characteristic` == "Highest level of education, post-secondary certificate/diploma or university degree" ~ "Post-secondary certificate/diploma or university degree"
    )
  ) %>%
  select(Year = REF_DATE,
         Geography = GEO,
         `Age group`,
         `Household income`,
         `Highest level of education`,
         Value = VALUE) %>%
  drop_na(Value) %>%
  mutate(Geography = recode(Geography,
                            "Canada (excluding territories)" = "Canada")) %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = Value)

fruits_veg <-
  raw_data %>%
  filter(
    Indicators == "Fruit and vegetable consumption, 5 times or more per day",
    Characteristics == "Percent"
  ) %>%
  select(REF_DATE, GEO, `Age group`, Sex, VALUE) %>%
  rename(Year = REF_DATE,
         Geography = GEO,
         Value = VALUE) %>%
  mutate(Geography = recode(Geography,
                            "Canada (excluding territories)" = "Canada")) %>%
  na.omit() %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = Value) 

#Create the aggregate line
total <-
  fruits_veg %>%
  filter(Geography == "Canada",
         `Age group` == "Total, 18 years and over",
         Sex == "Both sexes") %>%
  mutate_at(2:(ncol(.) - 2), ~ "")

#Create the non - aggregate line
fruits_veggies <-
  fruits_veg %>%
  filter(!(
    Geography == "Canada" & `Age group` == "Total, 18 years and over" &
      Sex == "Both sexes"
  ))

#Add the two rows together 
final_data <-
  bind_rows(total, fruits_veggies, disag_income) %>%
  select(Year,
         Geography,
         `Age group`,
         Sex,
         `Household income`,
         `Highest level of education`,
         GeoCode,
         Value)

write.csv(final_data,
          "data/indicator_3-1-1.csv",
          na = "",
          row.names = FALSE,
          fileEncoding = "UTF-8")
