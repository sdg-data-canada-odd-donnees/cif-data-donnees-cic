# CIF 11.2.1 --------------------------------------------------------------------

#load libraries 
library(dplyr)
library(tidyr)
library(cansim)

core_housing <- get_cansim("46-10-0065-01", factors = FALSE)

geocodes <- read.csv("geocodes.csv")

large_pop <- c(
  "Large urban population centres, Newfoundland and Labrador",
  "Large urban population centres, Nova Scotia",
  "Large urban population centres, New Brunswick",
  "Large urban population centres, Quebec",
  "Large urban population centres, Ontario",
  "Large urban population centres, Manitoba",
  "Large urban population centres, Saskatchewan",
  "Large urban population centres, Alberta",
  "Large urban population centres, British Columbia",
  "Total, large urban population centres"
)

med_pop <- c(
  "Medium population centres, Prince Edward Island",
  "Medium population centres, New Brunswick",
  "Medium population centres, Quebec",
  "Medium population centres, Ontario",
  "Medium population centres, Manitoba",
  "Medium population centres, Saskatchewan",
  "Medium population centres, Alberta",
  "Medium population centres, British Columbia",
  "Total, medium population centres"
)

small_pop <- c(
  "Small population centres, Prince Edward Island",
  "Small population centres, Newfoundland and Labrador",
  "Small population centres, Nova Scotia",
  "Small population centres, New Brunswick",
  "Small population centres, Quebec",
  "Small population centres, Ontario",
  "Small population centres, Manitoba",
  "Small population centres, Saskatchewan",
  "Small population centres, Alberta",
  "Small population centres, British Columbia",
  "Total, small population centres"
)

rural <- c(
  "Rural areas, Prince Edward Island",
  "Rural areas, Newfoundland and Labrador",
  "Rural areas, Nova Scotia",
  "Rural areas, New Brunswick",
  "Rural areas, Quebec",
  "Rural areas, Ontario",
  "Rural areas, Manitoba",
  "Rural areas, Saskatchewan",
  "Rural areas, Alberta",
  "Rural areas, British Columbia",
  "Total, rural areas"
)

cma <- c(
  "St. John's, Newfoundland and Labrador",
  "Halifax, Nova Scotia",
  "Moncton, New Brunswick",
  "Saint John, New Brunswick",
  "Québec, Quebec",
  "Montréal, Quebec",
  "Ottawa-Gatineau, Ontario/Quebec",
  "Toronto, Ontario",
  "Hamilton, Ontario",
  "Kitchener-Cambridge-Waterloo, Ontario",
  "Winnipeg, Manitoba",
  "Regina, Saskatchewan",
  "Saskatoon, Saskatchewan",
  "Lethbridge, Alberta",
  "Calgary, Alberta",
  "Edmonton, Alberta",
  "Vancouver, British Columbia",
  "Other census metropolitan areas, Quebec",
  "Other census metropolitan areas, Ontario",
  "Other census metropolitan areas, British Columbia",
  "Total, census metropolitan areas"
)

agglomerations <- c(
  "Census agglomerations, Newfoundland and Labrador",
  "Census agglomerations, Prince Edward Island",
  "Census agglomerations, Nova Scotia",
  "Census agglomerations, New Brunswick",
  "Census agglomerations, Quebec",
  "Census agglomerations, Ontario",
  "Census agglomerations, Manitoba",
  "Census agglomerations, Saskatchewan",
  "Census agglomerations, Alberta",
  "Census agglomerations, British Columbia",
  "Census agglomerations, Yukon",
  "Census agglomerations, Northwest Territories",
  "Total, census agglomerations"
)

outside <- c(
  "Outside census metropolitan areas and census agglomerations, Newfoundland and Labrador",
  "Outside census metropolitan areas and census agglomerations, Prince Edward Island",
  "Outside census metropolitan areas and census agglomerations, Nova Scotia",
  "Outside census metropolitan areas and census agglomerations, New Brunswick",
  "Outside census metropolitan areas and census agglomerations, Quebec",
  "Outside census metropolitan areas and census agglomerations, Ontario",
  "Outside census metropolitan areas and census agglomerations, Manitoba",
  "Outside census metropolitan areas and census agglomerations, Saskatchewan",
  "Outside census metropolitan areas and census agglomerations, Alberta",
  "Outside census metropolitan areas and census agglomerations, British Columbia",
  "Total, outside census metropolitan areas and census agglomerations"
)

province_territories <- c(
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

core_housing_filtered <-
  core_housing %>%
  filter(
    `Core housing need statistics` == "Percentage of households in core housing need"
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    Tenure = `Tenure including first-time homebuyer and social and affordable housing status`,
    Value = VALUE
  ) %>%
  na.omit()

rename_canada <-
  core_housing_filtered %>%
  filter(
    Geography == "Canada (provinces only)"
  ) %>%
  mutate(
    Geography = "Canada",
    `Geographic location` = "Canada"
  )

prov_terr <-
  core_housing_filtered %>%
  filter(
    Geography %in% province_territories
  ) %>%
  mutate(
    `Geographic location` = "Provinces and territories"
  )

census_metropolitan_areas <-
  core_housing_filtered %>%
  filter(
    Geography %in% cma
  ) %>%
  mutate(
    `Geographic location` = "Census metropolitan areas",
    `Select census metropolitan areas` = Geography,
    Geography = trimws(gsub(".*,",'', Geography)),
    Geography = case_when(
      Geography == "census metropolitan areas" ~ "Canada",
      TRUE ~ Geography
    )
  )

census_agglomerations <-
  core_housing_filtered %>%
  filter(
    Geography %in% agglomerations
  ) %>%
  mutate(
    `Geographic location` = "Census agglomerations",
    Geography = trimws(gsub(".*,",'', Geography)),
    Geography = case_when(
      Geography == "census agglomerations" ~ "Canada",
      TRUE ~ Geography
    )
  )

outside_csm_ca <-
  core_housing_filtered %>%
  filter(
    Geography %in% outside
  ) %>%
  mutate(
    `Geographic location` = "Outside census metropolitan areas and census agglomerations",
    Geography = trimws(gsub(".*,",'', Geography)),
    Geography = case_when(
      Geography == "outside census metropolitan areas and census agglomerations" ~ "Canada",
      TRUE ~ Geography
    )
  )

large_population <-
  core_housing_filtered %>%
  filter(
    Geography %in% large_pop
  ) %>%
  mutate(
    `Geographic location` = "Large urban population centres",
    Geography = trimws(gsub(".*,",'', Geography)),
    Geography = case_when(
      Geography == "large urban population centres" ~ "Canada",
      TRUE ~ Geography
    )
  )

medium_population <-
  core_housing_filtered %>%
  filter(
    Geography %in% med_pop
  ) %>%
  mutate(
    `Geographic location` = "Medium population centres",
    Geography = trimws(gsub(".*,",'', Geography)),
    Geography = case_when(
      Geography == "medium population centres" ~ "Canada",
      TRUE ~ Geography
    )
  )

small_population <-
  core_housing_filtered %>%
  filter(
    Geography %in% small_pop
  ) %>%
  mutate(
    `Geographic location` = "Small population centres",
    Geography = trimws(gsub(".*,",'', Geography)),
    Geography = case_when(
      Geography == "small population centres" ~ "Canada",
      TRUE ~ Geography
    )
  )

rural_areas <-
  core_housing_filtered %>%
  filter(
    Geography %in% rural
  ) %>%
  mutate(
    `Geographic location` = "Rural areas",
    Geography = trimws(gsub(".*,",'', Geography)),
    Geography = case_when(
      Geography == "rural areas" ~ "Canada",
      TRUE ~ Geography
    )
  )

combined <-
  bind_rows(rename_canada,prov_terr,census_metropolitan_areas,census_agglomerations,outside_csm_ca,large_population,medium_population,small_population,rural_areas) %>%
  select(
    Year,
    Geography,
    `Geographic location`,
    `Select census metropolitan areas`,
    Tenure,
    Value
  )

total_line <-
  combined %>%
  filter(
    `Geographic location` == "Canada",
    Tenure == "Total, tenure"
  ) %>% 
  mutate_at(2:5, ~ "")

non_total <-
  combined %>%
  filter(
    !(`Geographic location` == "Canada" & Tenure == "Total, tenure")
  )

data_final <-
  bind_rows(total_line,non_total) %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = "Value")

write.csv(data_final,
          "data/indicator_11-2-1.csv",
          na = "",
          row.names = FALSE,
          fileEncoding = "UTF-8")
  