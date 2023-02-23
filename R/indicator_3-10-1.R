# CIF 3.10.1 ---------------------------------------------------------------


# load libraries
library(dplyr)
library(cansim)
library(stringr)


# load CODR table from stc api
Raw_data <- get_cansim("13-10-0394-01", factors = FALSE)


# load geocode
geocodes <- read.csv("geocodes.csv")


selected_ages <- c(
  "Age at time of death, all ages",
  "Age at time of death, 1 to 14 years",
  "Age at time of death, 15 to 24",
  "Age at time of death, 25 to 34 years",
  "Age at time of death, 35 to 44 years",
  "Age at time of death, 45 to 54 years",
  "Age at time of death, 55 to 64 years",
  "Age at time of death, 65 to 74 years",
  "Age at time of death, 75 to 84 years",
  "Age at time of death, 85 and over"
)


selected_diseases <- c(
  "Malignant neoplasms [C00-C97]",
  "Diabetes mellitus [E10-E14]",
  "Alzheimer's disease [G30]",
  "Diseases of heart [I00-I09, I11, I13, I20-I51]",
  "Cerebrovascular diseases [I60-I69]",
  "Influenza and pneumonia [J09-J18]",
  "Chronic lower respiratory diseases [J40-J47]",
  "Chronic liver disease and cirrhosis [K70, K73-K74]",
  "Accidents (unintentional injuries) [V01-X59, Y85-Y86]",
  "Intentional self-harm (suicide) [X60-X84, Y87.0]"
)


causes_of_death <-
  Raw_data %>%
  filter(
    Characteristics == "Age-specific mortality rate per 100,000 population",
    REF_DATE >= 2015,
    `Age at time of death` %in% selected_ages,
    `Leading causes of death (ICD-10)` %in% selected_diseases
  ) %>%
  select(REF_DATE,
         GEO,
         `Age at time of death`,
         Sex,
         `Leading causes of death (ICD-10)`,
         VALUE) %>%
  rename(Year = REF_DATE,
         Geography = GEO,
         Value = VALUE) %>%
  mutate(
    Geography = gsub(", place of residence", "", Geography),
    `Leading causes of death (ICD-10)` = str_remove(
      `Leading causes of death (ICD-10)`, 
      " \\[.*\\]"
    ),
    `Age at time of death` = gsub(
      "Age at time of death, ", 
      "", 
      `Age at time of death`
    ),
    `Age at time of death` = recode(
      `Age at time of death`,
      "all ages" = "All ages"
    )
  ) %>% 
  mutate_at(2:(ncol(.) - 1), ~ paste0("data.", .x)) %>% 
  rename_at(2:(ncol(.) - 1), ~ paste0("data.", .x))


write.csv(causes_of_death, "data/indicator_3-10-1.csv", na = "", row.names = FALSE)
