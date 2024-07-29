# CIF 5.5.1 ---------------------------------------------------------------

# load libraries
library(dplyr)
library(cansim)
library(stringr)


# load CODR table from stc api
Raw_data <- get_cansim("14-10-0417-01", factors = FALSE)


# load geocode
geocodes <- read.csv("geocodes.csv")


# Format table
selected_occupations <- c(
  "Total employees, all occupations [00-95]",
  "Management occupations [00, 10, 20, 30, 40, 50, 60, 70, 80, 90]",
  "Business, finance and administration occupations, except management [11-14]",
  "Natural and applied sciences and related occupations, except management [21-22]",
  "Health occupations, except management [31-33]",
  "Occupations in education, law and social, community and government services, except management [41-45]",
  "Occupations in art, culture, recreation and sport, except management [51-55]",
  "Sales and service occupations, except management [62-65]",
  "Trades, transport and equipment operators and related occupations, except management [72-75]",
  "Natural resources, agriculture and related production occupations, except management [82-85]",
  "Occupations in manufacturing and utilities, except management [92-95]"
)

wage_ratio <-
  Raw_data %>%
  filter(
    REF_DATE >= 2015,
    Wages == "Median hourly gender wage ratio",
    `National Occupational Classification (NOC)` %in% selected_occupations,
    Sex == "Females"
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Type of work`,
    `National Occupational Classification (NOC)`,
    Age = `Age group`,
    Value = VALUE
  ) %>% 
  mutate(
    `National Occupational Classification (NOC)` = str_remove(
      `National Occupational Classification (NOC)`,
      " \\[.*\\]"
    ),
    `National Occupational Classification (NOC)` = str_remove(
      `National Occupational Classification (NOC)`,
      ", except management"
    )
  ) %>% 
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = Value)


# Create the total and non-total lines
total <-
  wage_ratio %>%
  filter(
    Geography == "Canada",
    `Type of work` == "Both full- and part-time employees",
    `National Occupational Classification (NOC)` == "Total employees, all occupations",
    Age == "15 years and over"
  ) %>%
  mutate_at(2:(ncol(.) - 2), ~ "")


non_total <-
  wage_ratio %>%
  filter(
    !(
      Geography == "Canada" &
        `Type of work` == "Both full- and part-time employees" &
        `National Occupational Classification (NOC)` == "Total employees, all occupations" &
        Age == "15 years and over"
    )
  ) %>%
  mutate_at(2:(ncol(.) - 2), ~ paste0("data.", .x))


# Format the final table
final_data <-
  bind_rows(total, non_total) %>%
  rename_at(2:(ncol(.) - 2), ~ paste0("data.", .x))


write.csv(
  final_data, 
  "data/indicator_5-5-1.csv", 
  na = "",
  row.names = FALSE
)
