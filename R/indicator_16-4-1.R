# CIF 16.4.1 -------------------------------------------------------

# load libraries
library(cansim)
library(dplyr)

raw_data <- get_cansim("35-10-0116-01", factors = FALSE)

court_time <- raw_data %>%
  filter(substr(REF_DATE, 1, 4) >= 2014,
         `Level of court` == "Total civil court cases",
         `Elapsed time from case initiation to first disposition` != "Total active cases, elapsed time",
         ) %>%
  select(Year = REF_DATE,
         data.Geography = GEO,
         `data.Type of case` = `Type of case`,
         `data.Elapsed time` = `Elapsed time from case initiation to first disposition`,
         GeoCode = GeoUID,
         Value = VALUE,
         ) %>%
  # Calculate percentage of cases for each "time elapsed" category
  group_by(Year, data.Geography, `data.Type of case`) %>%
  mutate(Value = round(Value / sum(Value) * 100, digits = 2)) %>%
  # Textual formatting
  mutate_at(2:(ncol(.)-2), ~ paste0("data.", .x)) %>%
  mutate(GeoCode = replace(GeoCode, GeoCode == "11124", NA),
         `data.Elapsed time` = substr(`data.Elapsed time`, 1, nchar(`data.Elapsed time`) - 13))


# Write to csv
write.csv(court_time, "data/indicator_16-4-1.csv",
          na = "", row.names = FALSE, fileEncoding = "UTF-8")