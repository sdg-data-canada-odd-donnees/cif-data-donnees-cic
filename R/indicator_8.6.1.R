# CIF 8.6.1 ---------------------------------------------------------------

# load libraries
library(dplyr)
library(cansim)


# load CODR table from stc api
Raw_data <- get_cansim("36-10-0681-01", factors = FALSE)

# Format the data
jobs <- Raw_data %>%
  filter(`Economic variable` == "Employment",
         `Goods and services (products)` == "Total, clean technology products") %>%
  select(Year = REF_DATE,
         data.Geography = GEO,
         GeoCode = GeoUID,
         Value = VALUE) %>%
  mutate(GeoCode = replace(GeoCode, GeoCode == 11124, ""), # remove pre-populated geocodes for Canada and prepend
         data.Geography = paste("data.", data.Geography, sep=""), # prepend "data." to strings in data.Geography column
         data.Geography = replace(data.Geography, data.Geography == "data.Canada", "")) # remove "data.Canada" from data.Geography column

write.csv(jobs, "data/indicator_8-6-1.csv", na="", row.names=FALSE, fileEncoding="UTF-8")