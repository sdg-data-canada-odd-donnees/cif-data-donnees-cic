# CIF 3.8.1 ---------------------------------------------------------------

# load libraries
library(dplyr)
library(cansim)
library(tidyr)

# load CODR table from stc api
Raw_data <- get_cansim("13-10-0905-01", factors = FALSE)
disag_income_raw <- get_cansim("13-10-0906-01", factors = FALSE)
disag_vismin_raw <- get_cansim("13-10-0880-01", factors = FALSE)

age_group <- c(
  "Total, 18 years and over",
  "18 to 34 years",
  "35 to 49 years",
  "50 to 64 years",
  "65 years and over"
)

immigration <- c(
    "Immigrants",
    "Admitted to Canada in the last 10 years",
    "Admitted to Canada more than 10 years ago",
    "Non-immigrants"
  )

sex <- c(
  "Males",
  "Females"
)

# load geocode
geocodes <- read.csv("geocodes.csv")

disag_income <-
  disag_income_raw %>%
  filter(
    Characteristics == "Percent",
    Indicators == "Perceived mental health, very good or excellent"
  ) %>%
  mutate(
    `Age group` = "Total, 18 years and over",
    `Household income` = case_when(
      `Selected characteristic` == "Household income, first quintile" ~ "First quintile",
      `Selected characteristic` == "Household income, second quintile" ~ "Second quintile",
      `Selected characteristic` == "Household income, third quintile" ~ "Third quintile",
      `Selected characteristic` == "Household income, fourth quintile" ~ "Fourth quintile",
      `Selected characteristic` == "Household income, fifth quintile" ~ "Fifth quintile"
    ),
    `Highest level of education` = case_when(
      `Selected characteristic` == "Highest level of education, less than secondary school graduation" ~ "Less than secondary school graduation",
      `Selected characteristic` == "Highest level of education, secondary school graduation, no post-secondary education" ~ "Secondary school graduation, no post-secondary education",
      `Selected characteristic` == "Highest level of education, post-secondary certificate/diploma or university degree" ~ "Post-secondary certificate/diploma or university degree"
    )
  ) %>%
  select(Year = REF_DATE,
         Geography = GEO,
         `Age group`,
         `Household income`,
         `Highest level of education`,
         Value = VALUE) %>%
  drop_na(Value) %>%
  mutate(Geography = recode(Geography,
                            "Canada (excluding territories)" = "Canada")) %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = Value)

disag_vismin <-
  disag_vismin_raw %>%
  filter(
    Characteristics == "Percentage of persons",
    Indicators == "Perceived mental health, very good or excellent"
  ) %>%
  mutate(
    `Age group` = case_when(
      `Selected sociodemographic characteristics` %in% age_group ~ `Selected sociodemographic characteristics`
    ),
    `Immigration status` = case_when(
      `Selected sociodemographic characteristics` %in% immigration ~ `Selected sociodemographic characteristics`
    ),
    Sex = case_when(
      `Selected sociodemographic characteristics` == "Men+" ~ "Males",
      `Selected sociodemographic characteristics` == "Women+" ~ "Females"
    ),
    `Visible minority` = case_when(
      `Visible minority` == "Total – Visible minority" ~ "All persons",
      `Visible minority` == "Total visible minority population" ~ "Visible minority",
      TRUE ~ `Visible minority`
    )
  ) %>%
  select(Year = REF_DATE,
         Geography = GEO,
         `Age group`,
         Sex,
         `Immigration status`,
         `Visible minority`,
         Value = VALUE) %>%
  drop_na(Value) %>%
  mutate(Geography = recode(Geography,
                            "Canada (excluding territories)" = "Canada")) %>%
  filter(
    !(Geography == "Canada" & `Visible minority` == "All persons" & `Age group` %in% age_group)
  ) %>%
  filter(
    !(Geography == "Canada" & `Visible minority` == "All persons" & Sex %in% sex)
  ) %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = Value)

mental_health <-
  Raw_data %>%
  filter(
    Characteristics == "Percent",
    Indicators == "Perceived mental health, very good or excellent"
  ) %>%
  select(REF_DATE, GEO, `Age group`, Sex, VALUE) %>%
  rename(Year = REF_DATE,
         Geography = GEO,
         Value = VALUE) %>%
  drop_na(Value) %>%
  mutate(Geography = recode(Geography,
                            "Canada (excluding territories)" = "Canada")) %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = Value)


total <-
  mental_health %>%
  filter(Geography == "Canada",
         `Age group` == "Total, 18 years and over",
         Sex == "Both sexes") %>%
  mutate_at(2:(ncol(.) - 2), ~ "")


non_total <-
  mental_health %>%
  filter(!(
    Geography == "Canada" &
      `Age group` == "Total, 18 years and over" &
      Sex == "Both sexes"
  ))

final_data <-
  bind_rows(total, non_total, disag_income, disag_vismin) %>%
  select(
    Year,
    Geography,
    `Age group`,
    Sex,
    `Household income`,
    `Highest level of education`,
    `Visible minority`,
    `Immigration status`,
    GeoCode,
    Value
  )


write.csv(final_data,
          "data/indicator_3-8-1.csv",
          na = "",
          row.names = FALSE,
          fileEncoding = "UTF-8")
