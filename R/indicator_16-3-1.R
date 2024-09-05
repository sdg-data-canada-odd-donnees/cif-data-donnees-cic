# CIF 16.3.1 --------------------------------------------------------------------

# load libraries
library(dplyr)
library(cansim)

raw_data <- get_cansim("35-10-0002-01", factors = FALSE)

cyber_crime <- raw_data %>%
  filter(
    REF_DATE >= 2015,
    Statistics == "Rate per 100,000 population",
    !is.na(VALUE),
    ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    GeoCode = GeoUID,
    Value = VALUE,
    ) %>%
  mutate(
    # Set "Total police-reported cybercrime" as headline.
    Geography = replace(Geography,
                        Geography == "Total police-reported cybercrime",
                        NA),
    # Remove [number] from Geography column
    Geography = gsub(" \\[\\d+\\]$", "", Geography)
    )


write.csv(cyber_crime, "data/indicator_16-3-1.csv",
          na = "", row.names = FALSE, fileEncoding = "UTF-8")
