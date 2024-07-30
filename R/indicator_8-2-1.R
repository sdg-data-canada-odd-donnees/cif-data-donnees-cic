#CIF 8.2.1

library(dplyr)
library(cansim)
library(stringr)
library(tidyr)

age <- get_cansim("14-10-0327-01", factors = FALSE)

# disability <- get_cansim("13-10-0377-01", factors = FALSE)

indigenous <- get_cansim("14-10-0359-01", factors = FALSE)

visible_minority <- get_cansim("14-10-0440-01", factors = FALSE)

immigrant <- get_cansim("14-10-0083-01", factors = FALSE)

labour <- get_cansim("14-10-0393-01", factors = FALSE)

geocodes <- read.csv("geocodes.csv")

geography <- c(
  "Canada",
  "Newfoundland and Labrador",
  "Prince Edward Island",
  "Nova Scotia",
  "New Brunswick",
  "Quebec",
  "Ontario",
  "Manitoba",
  "Saskatchewan",
  "Alberta",
  "British Columbia",
  "Yukon",
  "Northwest Territories",
  "Nunavut"
)

## Labour data

labour_filtered <-
  labour %>%
  filter(
    REF_DATE >= 2015,
    `Labour force characteristics` == "Employment rate"
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    Value = VALUE
  )

reorder_geo_labour <-
  labour_filtered %>%
  filter(
    Geography %in% geography
  )

remove_geo_labour <-
  labour_filtered %>%
  filter(
    !Geography %in% geography
  )

labour_combined <-
  bind_rows(reorder_geo_labour,remove_geo_labour)

## Age data

age_filtered <-
  age %>%
  filter(
    !(Sex == "Both sexes" & `Age group` == "15 years and over"),
    REF_DATE >= 2015,
    `Labour force characteristics` == "Employment rate"
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    Sex,
    `Age group`,
    Value = VALUE
  )


rename_age <-
  age_filtered %>%
  filter(
    `Age group` == "15 years and over"
  ) %>%
  mutate(
    `Age group` = "Total, 15 years and over"
  )

remove_age <-
  age_filtered %>%
  filter(
    !`Age group` == "15 years and over"
  )

age_combined <-
  bind_rows(rename_age, remove_age)

## Disability data

# disability_filtered <-
#   disability %>%
#   filter(
#     !Disability == "Total population, with and without disabilities",
#     `Labour force status` == "Employed",
#     Estimates == "Percentage of persons"
#   ) %>%
#   select(
#     Year = REF_DATE,
#     Geography = GEO,
#     Disability,
#     Gender,
#     `Age group`,
#     Value = VALUE
#   )
# 
# rename_age_disability <-
#   disability_filtered %>%
#   filter(
#     `Age group` == "65 years and older"
#   ) %>%
#   mutate(
#     `Age group` =  "65 years and over"
#   )
# 
# remove_age_disability <-
#   disability_filtered %>%
#   filter(
#     !`Age group` == "65 years and older"
#   )
# 
# disability_combined <-
#   bind_rows(rename_age_disability,remove_age_disability)
# 
# rename_gender_disability <-
#   disability_combined %>%
#   filter(
#     Gender == "Total, gender"
#   ) %>%
#   mutate(
#     Gender = "Total - Gender"
#   )
# 
# remove_gender_disability <-
#   disability_combined %>%
#   filter(
#     !Gender == "Total, gender"
#   )
# 
# disability_recombined <-
#   bind_rows(rename_gender_disability,remove_gender_disability)

## Indigenous data

indigenous_filtered <-
  indigenous %>%
  filter(
    !(`Indigenous group` == "Total population" & `Educational attainment` == "Total, all education levels"),
    REF_DATE >= 2015,
    `Labour force characteristics` == "Employment rate"
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Indigenous group`,
    `Educational attainment`,
    `Age group`,
    Value = VALUE
  )

rename_age_indigenous <-
  indigenous_filtered %>%
  filter(
    `Age group` == "15 years and over"
  ) %>%
  mutate(
    `Age group` = "Total, 15 years and over"
  )

remove_age_indigenous <-
  indigenous_filtered %>%
  filter(
    !`Age group` == "15 years and over"
  )

indigenous_combined <-
  bind_rows(rename_age_indigenous, remove_age_indigenous)

## Visible minority data

visible_minority_filtered <-
  visible_minority %>%
  filter(
    !`Population group` == "Total population",
    REF_DATE >= 2015,
    `Labour force characteristics` == "Employment rate"
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Visible minority` = `Population group`,
    Sex,
    `Age group` = Age,
    Value = VALUE
  )

rename_age_vismin <-
  visible_minority_filtered %>%
  filter(
    `Age group` == "15 years and over"
  ) %>%
  mutate(
    `Age group` = "Total, 15 years and over"
  )

remove_age_vismin <-
  visible_minority_filtered %>%
  filter(
    !`Age group` == "15 years and over"
  )

visible_minority_combined <-
  bind_rows(rename_age_vismin,remove_age_vismin)

## Immigrant data

immigrant_filtered <-
  immigrant %>%
  filter(
    !`Immigrant status` == "Total population",
    REF_DATE >= 2015,
    `Labour force characteristics` == "Employment rate"
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Immigrant status`,
    `Age group`,
    Value = VALUE
  )

rename_age_immigrant <-
  immigrant_filtered %>%
  filter(
    `Age group` == "15 years and over"
  ) %>%
  mutate(
    `Age group` = "Total, 15 years and over"
  )

remove_age_immigrant <-
  immigrant_filtered %>%
  filter(
    !`Age group` == "15 years and over"
  )

immigrant_combined <-
  bind_rows(rename_age_immigrant,remove_age_immigrant)

## Total and non total lines

total_line <-
  labour_combined %>%
  filter(
    Geography == "Canada"
  ) %>%
  mutate(
    Geography = ""
  )

non_total <-
  labour_combined %>%
  filter(
    !Geography == "Canada"
  )

data_final <- 
  bind_rows(total_line, non_total, age_combined, indigenous_combined, visible_minority_combined, immigrant_combined) %>%
  select(
    Year,
    Geography,
    `Age group`,
    Sex,
    `Indigenous group`,
    `Educational attainment`,
    `Visible minority`,
    `Immigrant status`,
    Value
  ) %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = Value)

write.csv(
  data_final,
  "data/indicator_8-2-1.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)