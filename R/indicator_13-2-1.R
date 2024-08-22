# CIF 13.2.1 -------------------------------------------------------

library(dplyr)
library(cansim)

connection <- get_cansim_sqlite("13-10-0156-01")

data <- connection %>%
  filter(`Cause of death (ICD-10)` == "Exposure to excessive natural heat [X30]") %>%
  collect_and_normalize()

disconnect_cansim_sqlite(connection)

exposure_deaths <- data %>%
  select(Year = REF_DATE, 
         Sex,
         `Age group`,
         Value = VALUE,
         ) %>%
  # blank out total line
  mutate(Sex = replace(Sex, 
                       Sex == "Both sexes" & `Age group` == "Total, all ages",
                       NA),
         `Age group` = replace(`Age group`,
                               is.na(Sex) & `Age group` == "Total, all ages",
                               NA)
         )

write.csv(exposure_deaths, "data/indicator_13-2-1.csv",
          na = "", row.names = FALSE, fileEncoding = "UTF-8")
