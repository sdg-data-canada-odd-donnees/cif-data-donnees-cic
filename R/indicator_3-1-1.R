# CIF 3.1.1 ---------------------------------------------------------------

# load libraries
library(dplyr)
library(cansim)


# load CODR table from stc api
raw_data <- get_cansim("13-10-0096-01", factors = FALSE)


# load geocode
geocodes <- read.csv("geocodes.csv")


#Format table 
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
         `Age group` == "Total, 12 years and over",
         Sex == "Both sexes") %>%
  mutate_at(2:(ncol(.) - 2), ~ "")


#Create the non - aggregate line
fruits_veggies <-
  fruits_veg %>%
  filter(!(
    Geography == "Canada" & `Age group` == "Total, 12 years and over" &
      Sex == "Both sexes"
  ))


#Add the two rows together 
final_data <-
  bind_rows(total, fruits_veggies)

write.csv(final_data,
          "data/indicator_3-1-1.csv",
          na = "",
          row.names = FALSE,
          fileEncoding = "UTF-8")
