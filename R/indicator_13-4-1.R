# CIF 13.4.1 -------------------------------------------------------

library(dplyr)
library(cansim)

expenditures <- get_cansim("34-10-0063-01", factors = FALSE)
geocodes <- read.csv("geocodes.csv")

expenditures_filtered <-
  expenditures %>%
  filter(
    REF_DATE >= 2015,
    `Type of asset` == "Flood protection infrastructure"
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    Value = VALUE
  ) %>%
  na.omit()

total_line <-
  expenditures_filtered %>%
  filter(
    Geography == "Canada"
  ) %>%
  mutate(
    Geography = ""
  )

non_total <-
  expenditures_filtered %>%
  filter(
    !Geography == "Canada"
  )

data_final <-
  bind_rows(total_line,non_total) %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = Value)
  
write.csv(data_final,
          "data/indicator_13-4-1.csv",
          na = "",
          row.names = FALSE,
          fileEncoding = "UTF-8")