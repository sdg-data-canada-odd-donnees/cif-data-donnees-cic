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
    `Age group` = "Total",
    Sex = "Both sexes"
  ) %>%
  select(
    Year,
    Geography,
    `Age group`,
    `Sex`,
    Value = `Nunavut_Val`
  )

nvt_under_18 <-
  nunavut_transposed %>%
  mutate(
    `Age group` = "Under 18 years",
    Sex = "Both sexes"
  ) %>%
  select(
    Year,
    Geography,
    `Age group`,
    `Sex`,
    Value = `Persons under 18 years`
  )

nvt_18_to_64 <-
  nunavut_transposed %>%
  mutate(
    `Age group` = "18 to 64 years",
    Sex = "Both sexes"
  ) %>%
  select(
    Year,
    Geography,
    `Age group`,
    `Sex`,
    Value = `Persons 18 to 64 years`
  )

nvt_over_65 <-
  nunavut_transposed %>%
  mutate(
    `Age group` = "65 years and over",
    Sex = "Both sexes"
  ) %>%
  select(
    Year,
    Geography,
    `Age group`,
    `Sex`,
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
    `Age group` = "All persons",
    Sex = "Both sexes"
  ) %>%
  select(
    Year,
    Geography,
    `Age group`,
    `Sex`,
    Value = `Yukon and Northwest Territories`
  )

Yukon <-
  territories_transposed %>%
  mutate(
    Geography = "Yukon",
    `Age group` = "All persons",
    Sex = "Both sexes"
  ) %>%
  select(
    Year,
    Geography,
    `Age group`,
    `Sex`,
    Value = `Yukon`
  )

Northwest <-
  territories_transposed %>%
  mutate(
    Geography = "Northwest Territories",
    `Age group` = "All persons",
    Sex = "Both sexes"
  ) %>%
  select(
    Year,
    Geography,
    `Age group`,
    `Sex`,
    Value = `Northwest Territories`
  )

terr_under_18 <-
  territories_transposed %>%
  mutate(
    Geography = "Yukon and Northwest Territories",
    `Age group` = "Under 18 years",
    Sex = "Both sexes"
  ) %>%
  select(
    Year,
    Geography,
    `Age group`,
    `Sex`,
    Value = `Persons under 18 years`
  )

terr_18_to_64 <-
  territories_transposed %>%
  mutate(
    Geography = "Yukon and Northwest Territories",
    `Age group` = "18 to 64 years",
    Sex = "Both sexes"
  ) %>%
  select(
    Year,
    Geography,
    `Age group`,
    `Sex`,
    Value = `Persons 18 to 64 years`
  )

terr_over_65 <-
  territories_transposed %>%
  mutate(
    Geography = "Yukon and Northwest Territories",
    `Age group` = "65 years and over",
    Sex = "Both sexes"
  ) %>%
  select(
    Year,
    Geography,
    `Age group`,
    `Sex`,
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
  "Persons under 18 years",
  "Persons in economic families",
  "Males in economic families",
  "Females in economic families",
  "Seniors in economic families",
  "Senior males in economic families",
  "Senior females in economic families",
  "Persons under 18 years in economic families",
  "Persons under 18 years in couple families with children",
  "Persons under 18 years in female lone-parent families",
  "Persons under 18 years in all other economic families",
  "Persons 18 to 64 years in economic families",
  "Males 18 to 64 years in economic families",
  "Females 18 to 64 years in economic families",
  "Persons not in an economic family",
  "Males not in an economic family",
  "Females not in an economic family",
  "Seniors not in an economic family",
  "Senior males not in an economic family",
  "Senior females not in an economic family",
  "Non-seniors not in an economic family",
  "Non-senior males not in an economic family",
  "Non-senior females not in an economic family"
)

male <- c(
  "Males",
  "Males, 18 to 64 years",
  "Males, 65 years and over",
  "Males, under 18 years",
  "Males in economic families",
  "Senior males in economic families",
  "Males 18 to 64 years in economic families",
  "Males not in an economic family",
  "Senior males not in an economic family",
  "Non-senior males not in an economic family"
)

female <- c(
  "Females",
  "Females, 18 to 64 years",
  "Females, 65 years and over",
  "Females, under 18 years",
  "Females in economic families",
  "Senior females in economic families",
  "Females 18 to 64 years in economic families",
  "Females not in an economic family",
  "Senior females not in an economic family",
  "Non-senior females not in an economic family"
)

all_persons <- c(
  "All persons",
  "Persons 18 to 64 years",
  "Persons 65 years and over",
  "Persons under 18 years",
  "Persons in economic families",
  "Seniors in economic families",
  "Persons under 18 years in economic families",
  "Persons under 18 years in couple families with children",
  "Persons under 18 years in female lone-parent families",
  "Persons under 18 years in all other economic families",
  "Persons 18 to 64 years in economic families",
  "Persons not in an economic family",
  "Seniors not in an economic family",
  "Non-seniors not in an economic family"
)

all_age_group <- c(
  "All persons",
  "Females",
  "Males",
  "Persons in economic families",
  "Persons not in an economic family",
  "Males in economic families",
  "Females in economic families",
  "Males not in an economic family",
  "Females not in an economic family"
)

under_18 <- c(
  "Females, under 18 years",
  "Males, under 18 years",
  "Persons under 18 years",
  "Persons under 18 years in economic families",
  "Persons under 18 years in couple families with children",
  "Persons under 18 years in female lone-parent families",
  "Persons under 18 years in all other economic families"
)

age_18_64 <- c(
  "Females, 18 to 64 years",
  "Males, 18 to 64 years",
  "Persons 18 to 64 years",
  "Persons 18 to 64 years in economic families",
  "Males 18 to 64 years in economic families",
  "Females 18 to 64 years in economic families"
)

age_65_over <- c(
  "Females, 65 years and over",
  "Males, 65 years and over",
  "Persons 65 years and over",
  "Seniors in economic families",
  "Senior males in economic families",
  "Senior females in economic families",
  "Seniors not in an economic family",
  "Senior males not in an economic family",
  "Senior females not in an economic family"
)

economic_family <- c(
  "Persons in economic families",
  "Males in economic families",
  "Females in economic families",
  "Seniors in economic families",
  "Senior males in economic families",
  "Senior females in economic families",
  "Persons under 18 years in economic families",
  "Persons under 18 years in couple families with children",
  "Persons under 18 years in female lone-parent families",
  "Persons under 18 years in all other economic families",
  "Persons 18 to 64 years in economic families",
  "Males 18 to 64 years in economic families",
  "Females 18 to 64 years in economic families",
  "Persons not in an economic family",
  "Males not in an economic family",
  "Females not in an economic family",
  "Seniors not in an economic family",
  "Senior males not in an economic family",
  "Senior females not in an economic family",
  "Non-seniors not in an economic family",
  "Non-senior males not in an economic family",
  "Non-senior females not in an economic family"
)

all_family <- c(
  "All persons",
  "Persons 18 to 64 years",
  "Persons 65 years and over",
  "Persons under 18 years",
  "Males",
  "Males, 18 to 64 years",
  "Males, 65 years and over",
  "Males, under 18 years",
  "Females",
  "Females, 18 to 64 years",
  "Females, 65 years and over",
  "Females, under 18 years"
)

in_economic_families <- c(
  "Persons in economic families",
  "Males in economic families",
  "Females in economic families",
  "Persons under 18 years in economic families",
  "Persons 18 to 64 years in economic families",
  "Males 18 to 64 years in economic families",
  "Females 18 to 64 years in economic families"
)

senior_in_econ <- c(
  "Seniors in economic families",
  "Senior males in economic families",
  "Senior females in economic families"
)

not_in_economic_families <- c(
  "Persons not in an economic family",
  "Males not in an economic family",
  "Females not in an economic family"
)

seniors_not_in_econ <- c(
  "Seniors not in an economic family",
  "Senior males not in an economic family",
  "Senior females not in an economic family"
)

non_seniors_not_in_econ <- c(
  "Non-seniors not in an economic family",
  "Non-senior males not in an economic family",
  "Non-senior females not in an economic family"
)

# filter for years past 2015 and the proportion (rather than number)
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
  ) %>%
  mutate(
    `Age group` = case_when(
      `Persons in low income` %in% all_age_group ~ "Total",
      `Persons in low income` %in% under_18 ~ "Under 18 years",
      `Persons in low income` %in% age_18_64 ~ "18 to 64 years",
      `Persons in low income` %in% age_65_over ~ "65 years and over",
      TRUE ~ ""
    ),
    Sex = case_when(
      `Persons in low income` %in% all_persons ~ "Both sexes",
      `Persons in low income` %in% male ~ "Male",
      `Persons in low income` %in% female ~ "Female",
      TRUE ~ ""
    ),
    `Economic family type` = case_when(
      `Persons in low income` %in% all_family ~ "Total",
      `Persons in low income` %in% in_economic_families ~ "Persons in economic families",
      `Persons in low income` == "Persons under 18 years in couple families with children" ~ "Persons in couple families with children",
      `Persons in low income` == "Persons under 18 years in female lone-parent families" ~ "Persons in female lone-parent families",
      `Persons in low income` == "Persons under 18 years in all other economic families" ~ "Persons in all other economic families",
      `Persons in low income` %in% senior_in_econ ~ "Seniors in economic families",
      `Persons in low income` %in% not_in_economic_families ~ "Persons not in an economic family",
      `Persons in low income` %in% seniors_not_in_econ ~ "Seniors not in an economic family",
      `Persons in low income` %in% non_seniors_not_in_econ ~ "Non-seniors not in an economic family"
    )
  ) %>%
  select(
    Year, 
    Geography, 
    `Age group`,
    Sex,
    `Economic family type`,
    Value
  )

combined <-
  bind_rows(low_income, nunavut_data_combined, territories_data_combined) %>%
  na.omit()

# create total line for Open SDG format (making totals blank)
total_line <-
  combined %>%
  filter(
    Geography=="Canada", 
    `Age group` == "Total",
    Sex == "Both sexes",
    `Economic family type` == "Total"
  ) %>%
  mutate(
    Geography = "", 
    `Age group` ="",
    Sex = "",
    `Economic family type` = ""
  )

# combine total lines with all disaggregations
data_final <- 
  bind_rows(
    total_line,
    combined %>% 
      filter(!(Geography=="Canada" & `Age group` == "Total" & Sex == "Both sexes" & `Economic family type` == "Total")) %>%
      arrange(Geography, Year)
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
