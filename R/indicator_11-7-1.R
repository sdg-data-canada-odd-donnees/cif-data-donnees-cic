# CIF 11.7.1 ---------------------------------------------------------------

#load libraries
library(dplyr)
library(cansim)

aged_18_or_more <- get_cansim("13-10-0905-01", factors = FALSE)
aged_12_or_more <- get_cansim("13-10-0096-01", factors = FALSE)

#load geocode
geocodes <- read.csv("geocodes.csv")


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
  bind_rows(total_new, non_total_new, total_old, non_total_old)


# Write the csv file
write.csv(final_data,
          "data/indicator_11-7-1.csv",
          na = "",
          row.names = FALSE,
          fileEncoding = "UTF-8")
