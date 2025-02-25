# CIF 11.7.1 ---------------------------------------------------------------

#load libraries
library(tidyr)
library(dplyr)
library(cansim)

aged_18_or_more <- get_cansim("13-10-0905-01", factors = FALSE)
aged_12_or_more <- get_cansim("13-10-0096-01", factors = FALSE)
disag_18_or_more <- get_cansim("13-10-0906-01", factors = FALSE)

#load geocode
geocodes <- read.csv("geocodes.csv")

disaggregation_new <-
  disag_18_or_more %>%
  filter (
    Indicators == "Sense of belonging to local community, somewhat strong or very strong",
    Characteristics == "Percent"
  ) %>%
  mutate(
    Series = "Aged 18 years and older",
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
         Series,
         Value = VALUE) %>%
  drop_na(Value) %>%
  mutate(Geography = recode(Geography,
                            "Canada (excluding territories)" = "Canada")) %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = Value)

belonging_new <-
  aged_18_or_more %>%
  filter(
    Indicators == "Sense of belonging to local community, somewhat strong or very strong",
    Characteristics == "Percent"
  ) %>%
  mutate(
    Series = "Aged 18 years and older"
  ) %>%
  select(Year = REF_DATE,
         Geography = GEO,
         `Age group`,
         Sex,
         Series,
         Value = VALUE) %>%
  mutate(Geography = recode(Geography,
                            "Canada (excluding territories)" = "Canada")) %>%
  na.omit() %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = Value)

belonging_old <-
  aged_12_or_more %>%
  filter(
    Indicators == "Sense of belonging to local community, somewhat strong or very strong",
    Characteristics == "Percent",
    `Age group` == "12 to 17 years"
  )  %>%
  mutate(
    Series = "Aged 12 to 17 years"
  ) %>%
  select(Year = REF_DATE,
         Geography = GEO,
         Sex,
         Series,
         Value = VALUE) %>%
  mutate(Geography = recode(Geography,
                            "Canada (excluding territories)" = "Canada")) %>%
  na.omit() %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = Value) 


# Create the total and non total lines
total_new <-
  belonging_new %>%
  filter(Geography == "Canada",
         `Age group` == "Total, 18 years and over",
         Sex == "Both sexes") %>%
  mutate_at(2:(ncol(.) - 3), ~ "")


non_total_new <-
  belonging_new %>%
  filter(!(
    Geography == "Canada" &
      `Age group` == "Total, 18 years and over" &
      Sex == "Both sexes"
  ))

total_old <-
  belonging_old %>%
  filter(Geography == "Canada",
         Sex == "Both sexes") %>%
  mutate_at(2:(ncol(.) - 3), ~ "")


non_total_old <-
  belonging_old %>%
  filter(!(
    Geography == "Canada" &
      Sex == "Both sexes"
  ))

# Format the final table
final_data <-
  bind_rows(total_new, non_total_new, disaggregation_new, total_old, non_total_old) %>%
  select(Year,
         Geography,
         `Age group`,
         Sex,
         `Household income`,
         `Highest level of education`,
         Series,
         GeoCode,
         Value)

# Write the csv file
write.csv(final_data,
          "data/indicator_11-7-1.csv",
          na = "",
          row.names = FALSE,
          fileEncoding = "UTF-8")
