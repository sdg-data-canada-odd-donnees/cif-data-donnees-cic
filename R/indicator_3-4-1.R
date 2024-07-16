# CIF 3.4.1 ---------------------------------------------------------------

# load libraries
library(dplyr)
library(cansim)


# load CODR table from stc api
Raw_data <- get_cansim("10-10-0010-01", factors = FALSE)


# load geocode
geocodes <- read.csv("geocodes.csv")


#Format Table
alcohol <-
  Raw_data %>%
  filter(
    REF_DATE >= "2014/2015",
    `Type of sales` == "Total per capita sales", 
    `Value, volume and absolute volume` == "Absolute volume for total per capita sales"
    ) %>%
  select(REF_DATE, GEO, `Type of beverage`, VALUE) %>%
  rename(Year = REF_DATE,
         Geography = GEO,
         Value = VALUE) %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = Value)


#create the aggregate line
total <-
  alcohol %>%
  filter(Geography == "Canada",
         `Type of beverage` == "Total alcoholic beverages"
         ) %>%
  mutate_at(2:(ncol(.) - 1), ~ "") %>%
  mutate(
    GeoCode = as.numeric(GeoCode)
  )


#create the non-aggregate line
non_total_line <-
  alcohol %>%
  filter(!(
    Geography == "Canada" & `Type of beverage` == "Total alcoholic beverages"
  ))


final_data <-
  bind_rows(total, non_total_line)


write.csv(final_data,
          "data/indicator_3-4-1.csv",
          na = "",
          row.names = FALSE,
          fileEncoding = "UTF-8")