# CIF 3.3.1 ---------------------------------------------------------------

# load libraries
library(dplyr)
library(cansim)


# load CODR table from stc api
Raw_data <- get_cansim("13-10-0373-01", factors = FALSE)


body_mass <- 
  Raw_data %>%
  filter(Characteristics == "Percent", Measures == "Obese") %>%
  select(REF_DATE, Sex, `Age group`, VALUE) %>%
  rename(Year = REF_DATE, Value = VALUE)

total_line <-
  body_mass %>%
  filter(
    Sex == "Both sexes",
    `Age group` == "Ages 5 to 79"
  ) %>%
  mutate(
    Sex = "",
    `Age group` = ""
  )

non_total <-
  body_mass %>%
  filter(
    !(Sex == "Both sexes" & `Age group` == "Ages 5 to 79")
  )

data_final <-
  bind_rows(total_line,non_total)

write.csv(data_final,
          "data/indicator_3-3-1.csv",
          na = "",
          row.names = FALSE,
          fileEncoding = "UTF-8")

