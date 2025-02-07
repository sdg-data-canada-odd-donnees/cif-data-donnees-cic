# CIF 11.7.1 ---------------------------------------------------------------

#load libraries
library(dplyr)
library(cansim)

Raw_data <- get_cansim("13-10-0905-01", factors = FALSE)

#load geocode
geocodes <- read.csv("geocodes.csv")


belonging <-
  Raw_data %>%
  filter(
    Indicators == "Sense of belonging to local community, somewhat strong or very strong",
    Characteristics == "Percent"
  ) %>%
  select(Year = REF_DATE,
         Geography = GEO,
         `Age group`,
         Sex,
         Value = VALUE) %>%
  mutate(Geography = recode(Geography,
                            "Canada (excluding territories)" = "Canada")) %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = Value) 


# Create the total and non total lines
total <-
  belonging %>%
  filter(Geography == "Canada",
         `Age group` == "Total, 18 years and over",
         Sex == "Both sexes") %>%
  mutate_at(2:(ncol(.) - 2), ~ "")


non_total <-
  belonging %>%
  filter(!(
    Geography == "Canada" &
      `Age group` == "Total, 18 years and over" &
      Sex == "Both sexes"
  ))


# Format the final table
final_data <-
  rbind(total, non_total)


# Write the csv file
write.csv(final_data,
          "data/indicator_11-7-1.csv",
          na = "",
          row.names = FALSE,
          fileEncoding = "UTF-8")
