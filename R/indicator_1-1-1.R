# CIF 1.1.1 -------------------------------------------------------

library(cansim)
library(dplyr)
library(readr)
library(rvest)

nunavut_url <- "https://www150.statcan.gc.ca/n1/pub/75f0002m/75f0002m2022003-eng.htm"
territories_url <- "https://www150.statcan.gc.ca/n1/pub/75f0002m/75f0002m2021007-eng.htm"

nunavut_data <- nunavut_url %>% 
  read_html() %>% 
  html_nodes("table") %>%
  .[[2]] %>%
  html_table()

nunavut_transposed <- data.frame(t(nunavut_data[-1])) %>%
  filter(
    X4 == "Estimate"
  ) %>%
  mutate(
    Geography = "Nunavut"
  ) %>%
  select(
    Year = X2,
    Geography,
    Nunavut_Val = X6,
    `Persons under 18 years` = X8,
    `Persons 18 to 64 years` = X9,
    `Persons 65 years and over` = X10
  )

nvt_value <-
  nunavut_transposed %>%
  mutate(
    `Persons in low income` = "All persons"
  ) %>%
  select(
    Year,
    Geography,
    `Persons in low income`,
    Value = `Nunavut_Val`
  )

nvt_under_18 <-
  nunavut_transposed %>%
  mutate(
    `Persons in low income` = "Persons under 18 years"
  ) %>%
  select(
    Year,
    Geography,
    `Persons in low income`,
    Value = `Persons under 18 years`
  )

nvt_18_to_64 <-
  nunavut_transposed %>%
  mutate(
    `Persons in low income` = "Persons 18 to 64 years"
  ) %>%
  select(
    Year,
    Geography,
    `Persons in low income`,
    Value = `Persons 18 to 64 years`
  )

nvt_over_65 <-
  nunavut_transposed %>%
  mutate(
    `Persons in low income` = "Persons 65 years and over"
  ) %>%
  select(
    Year,
    Geography,
    `Persons in low income`,
    Value = `Persons 65 years and over`
  )

nunavut_data_combined <-
  bind_rows(nvt_value,nvt_under_18,nvt_18_to_64,nvt_over_65) %>%
  mutate(
    Value = parse_number(Value, na = c("", "NA", "Note F: too unreliable to be published")),
    Year = parse_number(Year)
  )

territories_data <- territories_url %>% 
  read_html() %>% 
  html_nodes("table") %>%
  .[[2]] %>%
  html_table()

territories_transposed <- data.frame(t(territories_data[-1])) %>%
  filter(
    X3 == "Estimate"
  ) %>%
  select(
    Year = X2,
    `Yukon and Northwest Territories` = X6,
    Yukon = X7,
    `Northwest Territories` = X8, 
    `Persons under 18 years` = X10,
    `Persons 18 to 64 years` = X11,
    `Persons 65 years and over` = X12
  )

YK_NT <-
  territories_transposed %>%
  mutate(
    Geography = "Yukon and Northwest Territories",
    `Persons in low income` = "All persons"
  ) %>%
  select(
    Year,
    Geography,
    `Persons in low income`,
    Value = `Yukon and Northwest Territories`
  )

Yukon <-
  territories_transposed %>%
  mutate(
    Geography = "Yukon",
    `Persons in low income` = "All persons"
  ) %>%
  select(
    Year,
    Geography,
    `Persons in low income`,
    Value = `Yukon`
  )

Northwest <-
  territories_transposed %>%
  mutate(
    Geography = "Northwest Territories",
    `Persons in low income` = "All persons"
  ) %>%
  select(
    Year,
    Geography,
    `Persons in low income`,
    Value = `Northwest Territories`
  )

terr_under_18 <-
  territories_transposed %>%
  mutate(
    Geography = "Yukon and Northwest Territories",
    `Persons in low income` = "Persons under 18 years"
  ) %>%
  select(
    Year,
    Geography,
    `Persons in low income`,
    Value = `Persons under 18 years`
  )

terr_18_to_64 <-
  territories_transposed %>%
  mutate(
    Geography = "Yukon and Northwest Territories",
    `Persons in low income` = "Persons 18 to 64 years"
  ) %>%
  select(
    Year,
    Geography,
    `Persons in low income`,
    Value = `Persons 18 to 64 years`
  )

terr_over_65 <-
  territories_transposed %>%
  mutate(
    Geography = "Yukon and Northwest Territories",
    `Persons in low income` = "Persons 65 years and over"
  ) %>%
  select(
    Year,
    Geography,
    `Persons in low income`,
    Value = `Persons 65 years and over`
  )

territories_data_combined <-
  bind_rows(Yukon,Northwest,YK_NT,terr_under_18,terr_18_to_64,terr_over_65) %>%
  mutate(
    Value = parse_number(Value),
    Year = parse_number(Year)
  )

# get CODR table
raw_table <- get_cansim("11-10-0135-01", factors = FALSE)
geocodes <- read.csv("geocodes.csv")

demographics <- c(
  "All persons",
  "Females",
  "Females, 18 to 64 years",
  "Females, 65 years and over",
  "Females, under 18 years",
  "Males",
  "Males, 18 to 64 years",
  "Males, 65 years and over",
  "Males, under 18 years",
  "Persons 18 to 64 years",
  "Persons 65 years and over",
  "Persons under 18 years"
)

# filter for years past 2015, the proportion (rather than number), and remove "economic family" grouping
low_income <- 
  raw_table %>%
  rename(hier = `Hierarchy for Persons in low income`) %>%
  mutate(REF_DATE = as.numeric(REF_DATE)) %>%
  filter(
    REF_DATE >= 2015, 
    Statistics=="Percentage of persons in low income",
    `Persons in low income` %in% demographics,
    `Low income lines` == "Market basket measure, 2018 base"
  ) %>%
  select(
    Year = REF_DATE, 
    Geography = GEO, 
    `Persons in low income`, 
    Value = VALUE
  )

combined <-
  bind_rows(low_income, nunavut_data_combined, territories_data_combined) %>%
  na.omit()

# create total line for Open SDG format (making totals blank)
total_line <-
  combined %>%
  filter(
    Geography=="Canada", 
    `Persons in low income` == "All persons"
  ) %>%
  mutate(
    Geography = "", 
    `Persons in low income`= ""
  )

# combine total lines with all disaggregations
data_final <- 
  bind_rows(
    total_line,
    combined %>% 
      filter(!(Geography=="Canada" & `Persons in low income`=="All persons")) %>%
      arrange(Geography, `Persons in low income`, Year)
  ) %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = "Value")

# write data
write.csv(
  data_final,
  "data/indicator_1-1-1.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
