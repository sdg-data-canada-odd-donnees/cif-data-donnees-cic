#CIF 4.3.1

library(dplyr)
library(cansim)
library(stringr)
library(tidyr)

early_learning <- get_cansim("42-10-0004-01", factors = FALSE)

geocodes <- read.csv("geocodes.csv")

early_learning_filtered <-
  early_learning %>%
  filter(
    Statistics == "Percentage of children in child care"
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    Value = VALUE
  ) %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = Value)

total_line <-
  early_learning_filtered %>%
  filter(
    Geography == "Canada"
  ) %>%
  mutate(
    Geography = ""
  )

non_total <-
  early_learning_filtered %>%
  filter(
    !Geography == "Canada"
  )

data_final <-
  bind_rows(total_line,non_total)

write.csv(
  data_final,
  "data/indicator_4-3-1.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)