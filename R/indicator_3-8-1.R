# CIF 3.8.1 ---------------------------------------------------------------

# load libraries
library(dplyr)
library(cansim)


# load CODR table from stc api
Raw_data <- get_cansim("13-10-0905-01", factors = FALSE)


# load geocode
geocodes <- read.csv("geocodes.csv")


mental_health <-
  Raw_data %>%
  filter(
    Characteristics == "Percent",
    Indicators == "Perceived mental health, very good or excellent"
  ) %>%
  select(REF_DATE, GEO, `Age group`, Sex, VALUE) %>%
  rename(Year = REF_DATE,
         Geography = GEO,
         Value = VALUE) %>%
  mutate(Geography = recode(Geography,
                            "Canada (excluding territories)" = "Canada")) %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = Value)


total <-
  mental_health %>%
  filter(Geography == "Canada",
         `Age group` == "Total, 18 years and over",
         Sex == "Both sexes") %>%
  mutate_at(2:(ncol(.) - 2), ~ "")


non_total <-
  mental_health %>%
  filter(!(
    Geography == "Canada" &
      `Age group` == "Total, 18 years and over" &
      Sex == "Both sexes"
  ))

final_data <-
  bind_rows(total, non_total)


write.csv(final_data,
          "data/indicator_3-8-1.csv",
          na = "",
          row.names = FALSE,
          fileEncoding = "UTF-8")
