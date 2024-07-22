#CIF 8.2.1

library(dplyr)
library(cansim)
library(stringr)
library(tidyr)

age <- get_cansim("14-10-0327-01", factors = FALSE)

disability <- get_cansim("13-10-0377-01", factors = FALSE)

indigenous <- get_cansim("14-10-0359-01", factors = FALSE)

visible_minority <- get_cansim("14-10-0440-01", factors = FALSE)

immigrant <- get_cansim("14-10-0083-01", factors = FALSE)

labour <- get_cansim("14-10-0393-01", factors = FALSE)

geocodes <- read.csv("geocodes.csv")

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

disability_filtered <-
  disability %>%
  filter(
    !Disability == "Total population, with and without disabilities",
    `Labour force status` == "Employed",
    Estimates == "Percentage of persons"
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    Disability,
    Gender,
    `Age group`,
    Value = VALUE
  )

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

total_line <-
  labour_filtered %>%
  filter(
    Geography == "Canada"
  ) %>%
  mutate(
    Geography = ""
  )

non_total <-
  labour_filtered %>%
  filter(
    !Geography == "Canada"
  )

data_final <- 
  bind_rows(total_line, non_total, age_filtered, disability_filtered, indigenous_filtered, visible_minority_filtered, immigrant_filtered) %>%
  select(
    Year,
    Geography,
    `Age group`,
    Sex,
    Gender,
    Disability,
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