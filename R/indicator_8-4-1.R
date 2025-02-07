# CIF 8.4.1 ---------------------------------------------------------------

# load libraries
library(dplyr)
library(cansim)


# load CODR table from stc api
Raw_data <- get_cansim("14-10-0029-01", factors = FALSE)


# load geocode
geocodes <- read.csv("geocodes.csv")


selected_reasons <- c(
  "Part-time employment, all reasons",
  "Business conditions, did not look for full-time work in last month",
  "Could not find full-time work, did not look for full-time work in last month",
  "Business conditions, looked for full-time work in last month",
  "Could not find full-time work, looked for full-time work in last month"
)


# Format table
involuntary_work <-
  Raw_data %>%
  filter(REF_DATE >= 2015,
         `Reason for part-time work` %in% selected_reasons) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Reason for part-time work`,
    Gender,
    `Age group`,
    Value = VALUE
  ) %>%
  mutate(
    `Reason for part-time work` = ifelse(
      `Reason for part-time work` == "Part-time employment, all reasons",
      "All part-time",
      "Involuntary"
    )
  ) %>%
  group_by(Year, Geography, `Reason for part-time work`, Gender, `Age group`) %>%
  summarise(Value = sum(Value, na.rm = T)) %>%
  tidyr::pivot_wider(names_from = `Reason for part-time work`,
                     values_from = Value) %>%
  mutate(Value = round((Involuntary / `All part-time`) * 100, 2)) %>%
  select(Year,
         Geography,
         Gender,
         `Age group`,
         Value) %>%
  ungroup() %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = Value)


# Create the aggregate and non-aggregate table
total <-
  involuntary_work %>%
  filter(Geography == "Canada",
         Gender == "Total - Gender",
         `Age group` == "15 years and over") %>%
  mutate_at(2:(ncol(.) - 2), ~ "")


non_total <-
  involuntary_work %>%
  filter(!(
    Geography == "Canada" &
    Gender == "Total - Gender" &
    `Age group` == "15 years and over"
  ))


# Create the final table and export to csv
final_data <- bind_rows(total, non_total)


write.csv(final_data,
          "data/indicator_8-4-1.csv",
          na = "",
          row.names = FALSE,
          fileEncoding = "UTF-8")
