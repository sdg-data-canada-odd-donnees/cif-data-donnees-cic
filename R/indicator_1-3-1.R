# CIF 1.3.1 -------------------------------------------------------

library(dplyr)
library(cansim)
library(stringr)
library(tidyr)

debt_ratio <- get_cansim("36-10-0664-01", factors = FALSE)

Q4 <- str_subset(debt_ratio$REF_DATE, "-10")

age <- c(
  "Less than 35 years",
  "35 to 44 years",
  "45 to 54 years",
  "55 to 64 years",
  "65 years and over"
)

household <- c(
  "One-person households",
  "Multiple-person households"
)

quintile <- c(
  "Lowest income quintile",
  "Second income quintile",
  "Third income quintile",
  "Fourth income quintile",
  "Highest income quintile",
  "Lowest wealth quintile",
  "Second wealth quintile",
  "Third wealth quintile",
  "Fourth wealth quintile",
  "Highest wealth quintile",
  "Lowest income quintile, less than 35 years",
  "Lowest income quintile, 35 to 44 years",
  "Lowest income quintile, 45 to 54 years",
  "Lowest income quintile, 55 to 64 years",
  "Lowest income quintile, 65 years and over",
  "Second income quintile, less than 35 years",
  "Second income quintile, 35 to 44 years",
  "Second income quintile, 45 to 54 years",
  "Second income quintile, 55 to 64 years",
  "Second income quintile, 65 years and over",
  "Third income quintile, less than 35 years",
  "Third income quintile, 35 to 44 years",
  "Third income quintile, 45 to 54 years",
  "Third income quintile, 55 to 64 years",
  "Third income quintile, 65 years and over",
  "Fourth income quintile, less than 35 years",
  "Fourth income quintile, 35 to 44 years",
  "Fourth income quintile, 45 to 54 years",
  "Fourth income quintile, 55 to 64 years",
  "Fourth income quintile, 65 years and over",
  "Highest income quintile, less than 35 years",
  "Highest income quintile, 35 to 44 years",
  "Highest income quintile, 45 to 54 years",
  "Highest income quintile, 55 to 64 years",
  "Highest income quintile, 65 years and over"
)

source_of_income <- c(
  "Main source of household income: wages and salaries",
  "Main source of household income: self-employment income",
  "Main source of household income: net property income",
  "Main source of household income: current transfers received - pension benefits",
  "Main source of household income: current transfers received - others"
)

ownership_status <- c(
  "Owner",
  "Owner with mortgage",
  "Owner without mortgage",
  "Renter"
)

generation <- c(
  "Pre-1946",
  "Baby boom",
  "Generation X",
  "Millennials"
)

debt_ratio_filtered <-
  debt_ratio %>%
  filter(
    `Net worth indicators (wealth)` == "Debt to disposable income ratio",
    REF_DATE >= "2015-10",
    REF_DATE %in% Q4
  ) %>%
  select(
    Year = REF_DATE,
    Characteristics,
    Value = VALUE
  )%>%
  mutate(
    Year = str_replace_all(Year, "-10", " Q4")
  )

filter_age <-
  debt_ratio_filtered %>%
  filter(
    Characteristics %in% age
  ) %>%
  select(
    Year,
    Age = Characteristics,
    Value
  )

filter_household <-
  debt_ratio_filtered %>%
  filter(
    Characteristics %in% household
  ) %>%
  select(
    Year,
    `Type of household` = Characteristics,
    Value
  )

filter_quintile <-
  debt_ratio_filtered %>%
  filter(
    Characteristics %in% quintile
  ) %>%
  select(
    Year,
    `Income quintile` = Characteristics,
    Value
  )

filter_source_of_income <-
  debt_ratio_filtered %>%
  filter(
    Characteristics %in% source_of_income
  ) %>%
  mutate(
    Characteristics = str_remove(Characteristics, "Main source of household income: "),
    Characteristics = str_to_sentence(Characteristics)
  ) %>%
  select(
    Year,
    `Main source of household income` = Characteristics,
    Value
  )

filter_ownership_status <-
  debt_ratio_filtered %>%
  filter(
    Characteristics %in% ownership_status
  ) %>%
  select(
    Year,
    `Ownership status` = Characteristics,
    Value
  )

filter_generation <-
  debt_ratio_filtered %>%
  filter(
    Characteristics %in% generation
  ) %>%
  select(
    Year,
    `Generation` = Characteristics,
    Value
  )

total_line <-
  debt_ratio_filtered %>% 
  filter(
    Characteristics == "All households"
  ) %>% 
  mutate(Characteristics = "")

data_final <-
  bind_rows(total_line,filter_age,filter_generation,filter_household,filter_ownership_status,filter_quintile,filter_source_of_income) %>%
  select(
    Year,
    Age,
    `Generation`,
    `Type of household`,
    `Ownership status`,
    `Income quintile`,
    `Main source of household income`,
    Value
  ) %>%
  drop_na(Value)

write.csv(
  data_final,
  "data/indicator_1-3-1.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)