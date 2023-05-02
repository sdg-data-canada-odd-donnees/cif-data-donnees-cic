# CIF 16.3.1 --------------------------------------------------------------------

#load libraries
library(dplyr)
library(cansim)
library(stringr)

Raw_data <- get_cansim("35-10-0177-01", factors = FALSE)

# load geocode
geocodes <- read.csv("geocodes.csv")

geographies <- c(
  "Canada",
  "Newfoundland and Labrador [10]",
  "Prince Edward Island [11]",
  "Nova Scotia [12]",
  "New Brunswick [13]",
  "Quebec [24]",
  "Ontario [35]",
  "Manitoba [46]",
  "Saskatchewan [47]",
  "Alberta [48]",
  "British Columbia [59]",
  "Yukon [60]",
  "Northwest Territories [61]",
  "Nunavut [62]"
)

violations <- c(
  "Total violent Criminal Code violations [100]",
  "Homicide [110]",
  "Total other violations causing death [120]",
  "Attempted murder [1210]",
  "Sexual assault, level 3, aggravated [1310]",
  "Sexual assault, level 2, weapon or bodily harm [1320]",
  "Sexual assault, level 1 [1330]",
  "Total sexual violations against children [130]",
  "Assault, level 3, aggravated [1410]",
  "Assault, level 2, weapon or bodily harm [1420]",
  "Assault, level 1 [1430]",
  "Total robbery [160]",
  "Uttering threats [1627]",
  "Total other violent violations [180]"
)

crime_incidence <-
  Raw_data %>%
  filter(
    REF_DATE >= 2015,
    GEO %in% geographies,
    Violations %in% violations,
    Statistics == "Rate per 100,000 population"
  ) %>%
  select(Year = REF_DATE,
         Geography = GEO,
         Violations,
         Value = VALUE) %>%
  mutate(Geography =
           str_remove(Geography, " \\[.*\\]")) %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = Value)

# Create the total and non total line
total <-
  crime_incidence %>%
  filter(Geography == "Canada",
         Violations == "Total violent Criminal Code violations [100]") %>%
  mutate_at(2:(ncol(.) - 2), ~ "")

non_total <-
  crime_incidence %>%
  filter(!(
    Geography == "Canada" &
      Violations == "Total violent Criminal Code violations [100]"
  )) %>%
  mutate_at(2:(ncol(.) - 2), ~ paste0("data.", .x))

# Format the final table and export to csv
final_data <-
  bind_rows(total, non_total) %>%
  rename_at(2:(ncol(.) - 2), ~ paste0("data.", .x))

write.csv(final_data,
          "data/indicator_16-3-1.csv",
          na = "",
          row.names = FALSE,
          fileEncoding = "UTF-8")
