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
         Geography = GEO,
         `Type of case`,
         `Elapsed time` = `Elapsed time from case initiation to first disposition`,
         GeoCode = GeoUID,
         Value = VALUE,
         ) %>%
  # Calculate percentage of cases for each "time elapsed" category
  group_by(Year, Geography, `Type of case`) %>%
  mutate(Value = round(Value / sum(Value) * 100, digits = 2),
         GeoCode = replace(GeoCode, GeoCode == "11124", NA),
         `Elapsed time` = substr(`Elapsed time`, 1, nchar(`Elapsed time`) - 13)
         )

# Add column for progress calculation
# Progress calculation is based on "Less than or equal to 3 months" + "Greater than 3 months to 6 months"
progress <- court_time %>%
  filter(
    # only calculate progress for Canada, Total cases
    Geography == "Canada",
    `Type of case` == "Total cases",
    `Elapsed time` %in% c("Less than or equal to 3 months", "Greater than 3 months to 6 months"),
  ) %>%
  group_by(Year, Geography, `Type of case`) %>%
  summarise(Progress = sum(Value)) %>%
  # progress values are for elapsed time <= 6 months
  # but join progress values on the <= 3 months category
  # so it displays properly and because there is no <= 6 months category
  mutate(
    `Elapsed time` = "Less than or equal to 3 months"
  )

data_with_progress <- left_join(court_time, progress) %>%
  relocate(Progress, .before = GeoCode)

# Write to csv
write.csv(data_with_progress, "data/indicator_16-4-1.csv",
          na = "", row.names = FALSE, fileEncoding = "UTF-8")