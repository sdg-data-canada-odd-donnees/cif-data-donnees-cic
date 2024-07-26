# CIF 3.15.1 ---------------------------------------------------------------

library(dplyr)
library(cansim)
library(stringr)
library(tidyr)

health <- get_cansim("13-10-0836-01", factors = FALSE)

geocodes <- read.csv("geocodes.csv")

age_group <- c(
  "Canada, 15 to 24 years",
  "Canada, 25 to 54 years",
  "Canada, 25 to 34 years",
  "Canada, 35 to 44 years",
  "Canada, 45 to 54 years",
  "Canada, 55 to 64 years",
  "Canada,  65 years and over"
)

health_filtered <-
  health %>% 
  filter(
    REF_DATE >= 2015,
    Statistics == "Percentage of persons with unmet health care needs"
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    Sex,
    Value = VALUE
  ) %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = Value)

filter_age <-
  health_filtered %>%
  filter(
    Geography %in% age_group
  ) %>%
  mutate(
    `Age group` = str_remove(Geography, "Canada,  "),
    Geography = "Canada"
  ) %>%
  mutate(
    `Age group` = str_remove(`Age group`, "Canada, ")
  ) %>%
  select(
    Year,
    Geography,
    `Age group`,
    Sex,
    GeoCode,
    Value
  )

total_line <-
  health_filtered %>%
  filter(
    Geography == "Canada",
    Sex == "Both sexes"
  ) %>% 
  mutate(Geography = "",
         Sex = "")

non_total <-
  health_filtered %>%
  filter(
    !(Geography %in% age_group)
  ) %>%
  filter(
    !(Geography == "Canada" & Sex == "Both sexes")
  )

data_final <-
  bind_rows(total_line,filter_age,non_total) %>%
  select(
    Year,
    Geography,
    `Age group`,
    Sex,
    GeoCode,
    Value
  )

write.csv(
  data_final,
  "data/indicator_3-15-1.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)