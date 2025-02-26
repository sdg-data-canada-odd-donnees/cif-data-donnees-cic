# CIF 11.4.1 --------------------------------------------------------------------

#load libraries
library(dplyr)
library(cansim)
library(stringr)

# load geocode
geocodes <- read.csv("geocodes.csv")

canada_prov_terr <- c(
  "Canada",
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

# only choosing demographics given at the individual level - no household statistics
demographics <- c(
  "Total - Age groups of the population - 100% data",
  "0 to 14 years",
  "15 to 64 years",
  "15 to 19 years",
  "20 to 24 years",
  "65 years and over"
)

# Connect to large CODR table
connection <- get_cansim_sqlite("23-10-0313-01")
data <- connection %>%
  filter(
    `Demographic and socio-economic` %in% demographics,
    `Distance-capacity public transit service area` == "500 metres from all public transit stops",
    `Sustainable Development Goals (SDGs) 11.2.1 indicator` == "Proportion of population within service area"
  ) %>%
  collect_and_normalize()

disconnect_cansim_sqlite(connection)

# Create mapping of province/territory names to geo hierarchy codes
# ex: Ontario -> 1.469
prov_terr <- data %>%
  filter(GEO %in% canada_prov_terr[-1]) %>%
  select(GEO, `Hierarchy for GEO`) %>%
  distinct()
prov_terr_hierarchy_map <- setNames(as.character(prov_terr$GEO), prov_terr$`Hierarchy for GEO`)

data_filtered <- data %>%
  # Filter to keep only Canada, provinces/territories, and CMAs
  filter(
    (GEO %in% canada_prov_terr) | (grepl("Census metropolitan area", GEO)),
  ) %>%
  # Replace "Census metropolitan area (CMA)" in city name with appropriate province/territory
  # ex: Toronto, Census metropolitan area (CMA) -> Toronto, Ontario
  mutate(
    GEO = case_when(
      !(GEO %in% canada_prov_terr) ~ paste(str_remove(GEO, ",.+"), unlist(prov_terr_hierarchy_map[str_extract(`Hierarchy for GEO`, "^1\\.\\d+")]), sep = ", "),
      .default = GEO
    )
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    Location,
    Gender,
    `Demographic and socio-economic`,
    GeoCode = GeoUID,
    Value = VALUE
  ) %>%
  mutate(
    across(
      c(Geography, Location, Gender, `Demographic and socio-economic`, GeoCode),
      ~ replace(.,
                Geography == "Canada" &
                Location == "Total, location" &
                Gender == "Total, gender" &
                `Demographic and socio-economic` == "Total - Age groups of the population - 100% data",
                NA)
    )
  )

write.csv(data_filtered,
          "data/indicator_11-4-1.csv",
          na = "",
          row.names = FALSE,
          fileEncoding = "UTF-8")
