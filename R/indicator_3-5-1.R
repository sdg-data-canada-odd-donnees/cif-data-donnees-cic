#CIF 3.5.1

library(dplyr)
library(cansim)
library(stringr)
library(tidyr)

physical_activity <- get_cansim("13-10-0821-01", factors = FALSE)

data_final <-
  physical_activity %>%
  filter(
    REF_DATE >= 2015,
    Categories == "Meeting guidelines",
    Characteristics == "Estimate"
  ) %>% 
  select(
    Year = REF_DATE,
    Sex,
    `Age group`,
    Value = VALUE
  ) %>%
  na.omit()

write.csv(
  data_final,
  "data/indicator_3-5-1.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)