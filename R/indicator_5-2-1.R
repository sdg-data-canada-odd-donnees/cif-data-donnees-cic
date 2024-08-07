# CIF 5.2.1

# Load required libraries
library(dplyr)
library(stringr)
library(tidyr)
library(cansim)

# Load cansim table
intimate_partner_violence <- get_cansim("35-10-0205-01", factors = FALSE)

# Load geocodes from a CSV file
geocodes <- read.csv("geocodes.csv")

age_group <- c(
  "Total, all persons",
  "Age 15 to 24",
  "Age 25 to 34",
  "Age 35 to 44",
  "Age 45 to 54",
  "Age 55 to 64",
  "Age 65 to 74",
  "Age 75 and older"
)

indigenous <- c(
  "Indigenous person",
  "First Nations (North American Indian)",
  "Métis",
  "Inuk (Inuit)",
  "Non-Indigenous person"
)

LGBTQ2 <- c(
  "LGBTQ2 person",
  "Non-LGBTQ2 person"
)

visible_minority <- c(
  "Visible minority",
  "Arab",
  "Black",
  "Chinese",
  "Filipino",
  "Japanese",
  "Korean",
  "Latin American",
  "South Asian",
  "Southeast Asian",
  "West Asian",
  "Group not indicated elsewhere"
)

immigrant <- c(
  "Immigrant",
  "Non-immigrant"
)

disability <- c(
  "Person with disability",
  "Person without disability"
)

education <- c(
  "Highest degree earned, less than high school",
  "Highest degree earned, high school diploma",
  "Highest degree earned, college or trade school diploma",
  "Highest degree earned, university degree"
)

household_income <- c(
  "Household income, less than $20,000",
  "Household income, $20,000 to $59,999",
  "Household income, $60,000 to $99,999",
  "Household income, $100,000 to $149,999",
  "Household income, $150,000 or more"
)

residence <- c(
  "Location of residence, rural",
  "Location of residence, urban"
)

marital_status <- c(
  "Marital status, married or common-law",
  "Marital status, separated or divorced",
  "Marital status, widowed",
  "Marital status, single, never married"
)

intimate_partner_violence_filtered <-
  intimate_partner_violence %>%
  filter(
    Statistics == "Percentage",
    Gender == "Women",
    Timeframe == "Experienced intimate partner violence in the past 12 months"
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Type of intimate partner violence`,
    `Population characteristics`,
    Value = VALUE
  ) %>%
  na.omit() %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = "Value")

age <-
  intimate_partner_violence_filtered %>%
  filter(
    `Population characteristics` %in% age_group
  ) %>%
  mutate(
    `Age group` = `Population characteristics`
  )

indigenous_person <-
  intimate_partner_violence_filtered %>%
  filter(
    `Population characteristics` %in% indigenous
  ) %>%
  mutate(
    `Indigenous group` = `Population characteristics`
  )

visible_minority_population <-
  intimate_partner_violence_filtered %>%
  filter(
    `Population characteristics` %in% visible_minority
  ) %>%
  mutate(
    `Visible minority` = `Population characteristics`,
    `Visible minority` = case_when(
      `Visible minority` == "Visible minority" ~ "Visible minority population",
      TRUE ~ `Visible minority`
    )
  )

immigrant_status <-
  intimate_partner_violence_filtered %>%
  filter(
    `Population characteristics` %in% immigrant
  ) %>%
  mutate(
    `Immigrant status` = `Population characteristics`
  )

disability_status <-
  intimate_partner_violence_filtered %>%
  filter(
    `Population characteristics` %in% disability
  ) %>%
  mutate(
    `Disability` = `Population characteristics`
  )

education_level <- 
  intimate_partner_violence_filtered %>%
  filter(
    `Population characteristics` %in% education
  ) %>%
  mutate(
    `Level of education` = `Population characteristics`,
    `Level of education` = str_remove(`Level of education`, "Highest degree earned, "),
    `Level of education` = str_to_sentence(`Level of education`)
  )

income <-
  intimate_partner_violence_filtered %>%
  filter(
    `Population characteristics` %in% household_income
  ) %>%
  mutate(
    `Household income` = `Population characteristics`,
    `Household income` = str_remove(`Household income`, "Household income, "),
    `Household income` = str_to_sentence(`Household income`)
  )

location_of_residence <-
  intimate_partner_violence_filtered %>%
  filter(
    `Population characteristics` %in% residence
  ) %>%
  mutate(
    `Location of residence` = `Population characteristics`,
    `Location of residence` = str_remove(`Location of residence`, "Location of residence, "),
    `Location of residence` = str_to_sentence(`Location of residence`)
  )

marital <-
  intimate_partner_violence_filtered %>%
  filter(
    `Population characteristics` %in% marital_status
  ) %>%
  mutate(
    `Marital status` = `Population characteristics`,
    `Marital status` = str_remove(`Marital status`, "Marital status, "),
    `Marital status` = str_to_sentence(`Marital status`)
  )

combined <-
  bind_rows(age, indigenous_person, visible_minority_population, immigrant_status, disability_status, education_level, income, location_of_residence, marital) %>%
  select(
    Year,
    Geography,
    `Type of intimate partner violence`,
    `Age group`,
    `Indigenous group`,
    `Visible minority`,
    `Immigrant status`,
    `Disability`,
    `Level of education`,
    `Household income`,
    `Location of residence`,
    `Marital status`,
    GeoCode,
    Value
  )

total_line <-
  combined %>%
  filter(
    Geography == "Canada", 
    `Age group` == "Total, all persons",
    `Type of intimate partner violence` == "Total, any type of intimate partner violence"
  ) %>% 
  mutate_at(3:12, ~ "")

non_total <-
  combined %>%
  filter(
    !(
      Geography == "Canada" & `Age group` == "Total, all persons" & `Type of intimate partner violence` == "Total, any type of intimate partner violence"
    )
  )

data_final <-
  bind_rows(total_line, non_total)

write.csv(data_final,
          "data/indicator_5-2-1.csv",
          na = "",
          row.names = FALSE,
          fileEncoding = "UTF-8")