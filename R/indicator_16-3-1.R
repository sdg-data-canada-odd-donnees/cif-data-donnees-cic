# CIF 16.3.1 --------------------------------------------------------------------

# load libraries
library(dplyr)
library(cansim)

raw_data <- get_cansim("35-10-0002-01", factors = FALSE)

cyber_crime <- raw_data %>%
  filter(REF_DATE >= 2015,
         Statistics == "Rate per 100,000 population",
         !is.na(VALUE),
         ) %>%
  select(Year = REF_DATE,
         data.Geography = GEO,
         GeoCode = GeoUID,
         Value = VALUE,
         ) %>%
  # Prepend data. to all geography names. Set Total police-reported cybercrime as main series.
  mutate(data.Geography = paste0("data.", data.Geography),
         data.Geography = replace(data.Geography,
                                  data.Geography == "data.Total police-reported cybercrime",
                                  NA)
         )


write.csv(cyber_crime, "data/indicator_16-3-1.csv",
          na = "", row.names = FALSE, fileEncoding = "UTF-8")
