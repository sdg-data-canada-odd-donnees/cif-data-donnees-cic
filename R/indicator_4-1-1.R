# Indicator 4.1.1 ---------------------------------------------------------

library(cansim)
library(dplyr)

hs_completion <- get_cansim("37-10-0170-01", factors = FALSE)
geocodes <- read.csv("geocodes.csv")

select_characteristics <- c(
  "Non immigrants",
  "Immigrants",
  "Non-permanent residents",
  "Total visible minority population",
  "Not a visible minority",
  "Total Aboriginal identity",
  "First Nations (North American Indian)",
  "Métis",
  "Inuk (Inuit)",
  "Non-Aboriginal identity",
  "Total, all persons"
)

hs_completion_all <-
  hs_completion %>%
  filter(
    `Secondary (high) school diploma or equivalency certificate` == "High school completion rate",
    `Age group` %in% c(
      "Total, persons aged 15 years and over",
      "15 to 24 years",
      "25 to 64 years",
      "65 to 74 years",
      "75 years and over"
    ),
    `Selected demographic characteristics` %in% select_characteristics
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    Sex,
    `Age group`,
    `Selected demographic characteristics`,
    Value = VALUE
  )

total_line <- 
  hs_completion_all %>% 
  filter(
    Geography == "Canada",
    Sex == "Both sexes",
    `Age group` == "Total, persons aged 15 years and over",
    `Selected demographic characteristics` == "Total, all persons"
  ) %>% 
  mutate_at(2:5, ~ "")

hs_completion_all <- 
  hs_completion_all %>% 
  filter(!(
    Geography == "Canada" &
    Sex == "Both sexes" &
    `Age group` == "Total, persons aged 15 years and over" &
    `Selected demographic characteristics` == "Total, all persons"
  )) %>% 
  mutate_at(2:5, ~ paste0("data.", .x))

data_final <- 
  bind_rows(total_line, hs_completion_all) %>% 
  left_join(geocodes, by = "Geography") %>% 
  relocate(GeoCode, .before = "Value") %>% 
  rename_at(2:5, ~ paste0("data.", .x))

write.csv(
  data_final, 
  "data/indicator_4-1-1.csv",
  row.names = FALSE,
  na = ""
)

