# CIF 16.1.1 -------------------------------------------------------------------

#load libraries
library(dplyr)
library(cansim)

raw_data <- get_cansim("43-10-0058-01", factors = FALSE)

safety_perception <- raw_data %>%
  filter(Indicators == "Feeling safe walking in their area alone after dark",
         Statistics == "Percentage of persons",
         ) %>%
  select(Year = REF_DATE,
         data.Geography = GEO,
         `data.Visible minority` = `Visible minority`,
         `data.Sociodemographic characteristics` = `Selected sociodemographic characteristics`,
         Value = VALUE
         ) %>%
  mutate(`data.Visible minority` = replace(`data.Visible minority`,
                                           `data.Visible minority` == "Total – Visible minority",
                                           "Total population")
         ) %>%
  mutate_at(2:(ncol(.) - 1), ~ paste0("data.", .x))

write.csv(safety_perception, "data/indicator_16-1-1.csv",
          na = "", row.names = FALSE, fileEncoding = "UTF-8")