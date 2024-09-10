# Indicator 4.1.1 ---------------------------------------------------------

library(cansim)
library(dplyr)

print(sessionInfo())

age_groups <- c(
  "Total - Age",
  "Total, persons aged 15 years and over",
  "15 to 24 years",
  "25 to 64 years",
  "65 to 74 years",
  "75 years and over"
)

select_characteristics <- c(
  "Non immigrants",
  "Immigrants",
  "Non-permanent residents",
  "Total visible minority population",
  "Not a visible minority",
  "Total Aboriginal identity",
  "Indigenous identity",
  "First Nations (North American Indian)",
  "Métis",
  "Inuk (Inuit)",
  "Inuk (Inuit)",
  "Non-Aboriginal identity",
  "Non-Indigenous identity",
  "Total, all persons",
  "Total - Indigenous identity"
)



# 2016 high school completion data from CODR table

raw_data_2016 <- get_cansim("37-10-0170-01", factors = FALSE)

hs_completion_2016 <- raw_data_2016 %>%
  filter(
    `Secondary (high) school diploma or equivalency certificate` == "High school completion rate",
    `Age group` %in% age_groups,
    `Selected demographic characteristics` %in% select_characteristics
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    Gender = Sex,
    `Age group`,
    `Selected demographic characteristics`,
    GeoCode = GeoUID,
    Value = VALUE
  ) %>%
  # edit variable names to match 2021 conventions
  mutate(
    Gender = replace(Gender, Gender == "Both sexes", "Total"),
    `Age group` = replace(`Age group`, `Age group` == "Total, persons aged 15 years and over", "Total"),
    `Selected demographic characteristics` = case_when(`Selected demographic characteristics` == "Total, all persons" ~ "Total",
                                                       `Selected demographic characteristics` == "Non-Aboriginal identity" ~ "Non-Indigenous identity",
                                                       `Selected demographic characteristics` == "Total Aboriginal identity" ~ "Indigenous identity",
                                                       .default = `Selected demographic characteristics`)
  ) %>%
  # remove geocode values for Canada
  mutate(GeoCode = replace(GeoCode, Geography == "Canada", NA))

hs_completion_2016_total <- hs_completion_2016 %>% 
  filter(
    Geography == "Canada",
    Gender == "Total",
    `Age group` == "Total",
    `Selected demographic characteristics` == "Total"
  ) %>% 
  mutate_at(2:5, ~ NA)

hs_completion_2016_all <- hs_completion_2016 %>% 
  filter(!(
    Geography == "Canada" &
    Gender == "Total" &
    `Age group` == "Total" &
    `Selected demographic characteristics` == "Total"
  ))



# 2021 high school completion data from CODR table

connection <- get_cansim_sqlite("98-10-0420-01")

raw_data_2021 <- connection %>%
  filter(`Age (15A)` %in% age_groups,
  `Labour force status (8)` == "Total - Labour force status",
  `Registered or Treaty Indian status (3)` == "Total - Registered or Treaty Indian status",
  `Residence by Indigenous geography (10)` == "Total - Residence on or off reserve",
  `Indigenous identity (9)` %in% select_characteristics,
  `Secondary (high) school diploma or equivalency certificate (15)` %in% c("No high school diploma or equivalency certificate", 
                                                                           "With high school diploma or equivalency certificate")
  )%>%
  collect_and_normalize()

disconnect_cansim_sqlite(connection)

hs_completion_2021 <- raw_data_2021 %>%
  select(Year = REF_DATE,
         Geography = GEO,
         Gender = `Gender (3)`,
         `Age group` = `Age (15A)`,
         `Selected demographic characteristics` = `Indigenous identity (9)`,
         `Secondary (high) school diploma or equivalency certificate` = `Secondary (high) school diploma or equivalency certificate (15)`,
         GeoCode = GeoUID,
         Value = VALUE,
  ) %>%
  # calculate percentage
  group_by(Year, Geography, Gender, `Age group`, `Selected demographic characteristics`) %>%
  mutate(Value = Value / sum(Value) * 100) %>%
  # keep only high school completion rate
  filter(`Secondary (high) school diploma or equivalency certificate` == "With high school diploma or equivalency certificate") %>%
  select(!(`Secondary (high) school diploma or equivalency certificate`)) %>%
  # rename select sub-categories for clarity
  mutate(`Age group` = recode(`Age group`, "Total - Age" = "Total"),
         Gender = recode(Gender, "Total - Gender" = "Total"),
         `Selected demographic characteristics` = case_match(`Selected demographic characteristics`, 
                                                             "Total - Indigenous identity" ~ "Total",
                                                             "Inuk (Inuit)" ~ "Inuk (Inuit)",
                                                             .default = `Selected demographic characteristics`)
  ) %>%
  # remove geocode values for Canada
  mutate(GeoCode = replace(GeoCode, Geography == "Canada", NA))

hs_completion_2021_total <- hs_completion_2021 %>%
  filter(Geography == "Canada",
         `Age group` == "Total",
         Gender == "Total",
         `Selected demographic characteristics` == "Total") %>%
  mutate_at(2:5, ~ NA)

hs_completion_2021_all <- hs_completion_2021 %>%
  filter(!(Geography == "Canada" 
           & `Age group` == "Total"
           & Gender == "Total"
           & `Selected demographic characteristics` == "Total")
         )


# Combine final data

data_final <- bind_rows(hs_completion_2016_total,
                        hs_completion_2021_total,
                        hs_completion_2016_all,
                        hs_completion_2021_all)

write.csv(data_final, "data/indicator_4-1-1.csv",
  row.names = FALSE, na = "", fileEncoding = "UTF-8")
