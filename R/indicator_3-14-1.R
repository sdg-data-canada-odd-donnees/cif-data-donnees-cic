# CIF 3.14.1 ---------------------------------------------------------------

# load libraries
library(dplyr)
library(cansim)
library(tidyr)

# load CODR table from stc api
Raw_data <- get_cansim("13-10-0905-01", factors = FALSE)
disag_raw <- get_cansim("13-10-0906-01", factors = FALSE)

# load geocode
geocodes <- read.csv("geocodes.csv")

disag <-
  disag_raw %>%
  filter(Indicators == "Current smoker, daily or occasional",
         Characteristics == "Percent") %>%
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

smoking <-
  Raw_data %>%
  filter(Indicators == "Current smoker, daily or occasional",
         Characteristics == "Percent") %>%
  select(Year = REF_DATE,
         Geography = GEO,
         `Age group`,
         Sex,
         Value = VALUE) %>%
  drop_na(Value) %>%
  mutate(Geography = recode(Geography,
                            "Canada (excluding territories)" = "Canada")) %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = Value) 


total <-
  smoking %>%
  filter(Geography == "Canada",
         `Age group` == "Total, 18 years and over",
         Sex == "Both sexes") %>%
  mutate_at(2:(ncol(.) - 2), ~ "")


non_total_line <-
  smoking %>%
  filter(!(
    Geography == "Canada" &
      `Age group` == "Total, 18 years and over" &
      Sex == "Both sexes"
  ))


final_data <-
  bind_rows(total, non_total_line, disag) %>%
  select(Year,
         Geography,
         `Age group`,
         Sex,
         `Household income`,
         `Highest level of education`,
         GeoCode,
         Value)


write.csv(final_data,
          "data/indicator_3-14-1.csv",
          na = "",
          row.names = FALSE,
          fileEncoding = "UTF-8")
