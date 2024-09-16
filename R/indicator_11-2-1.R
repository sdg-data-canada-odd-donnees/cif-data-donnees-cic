# CIF 11.2.1 --------------------------------------------------------------------

#load libraries 
library(dplyr)
library(tidyr)
library(cansim)

raw_data1 <- get_cansim("46-10-0067-01", factors = FALSE)
raw_data2 <- get_cansim("46-10-0085-01", factors = FALSE)


core_housing_by_vulnerable_population <- raw_data1 %>%
  filter(
    Statistics == "Percentage of households",
    `Living with housing problems` == "Living in core housing need",
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Select housing-vulnerable populations`,
    GeoCode = GeoUID,
    Value = VALUE,
  ) %>%
  # Blank headline data
  mutate(
    across(
      c("Geography", "Select housing-vulnerable populations"),
      ~ replace(., `Select housing-vulnerable populations` == "All households", NA)
    )
  )


provinces <- c(
  "Newfoundland and Labrador",
  "Prince Edward Island",
  "Nova Scotia",
  "New Brunswick",
  "Quebec",
  "Ontario",
  "Manitoba",
  "Saskatchewan",
  "Alberta",
  "British Columbia"
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
  "London, Ontario",
  "Winnipeg, Manitoba",
  "Regina, Saskatchewan",
  "Saskatoon, Saskatchewan",
  "Lethbridge, Alberta",
  "Calgary, Alberta",
  "Edmonton, Alberta",
  "Vancouver, British Columbia",
  "Total, census metropolitan areas"
  ) %>%
  append(paste("Other census metropolitan areas", provinces, sep = ", "))

agglomerations <- paste("Census agglomerations", provinces, sep = ", ") %>%
  append("Total, census agglomerations")

outside <- paste("Outside census metropolitan areas and census agglomerations", provinces, sep = ", ") %>%
  append("Total, outside census metropolitan areas and census agglomerations")

large_pop <- paste("Large urban population centres", provinces, sep = ", ") %>%
  append("Total, large urban population centres")

med_pop <- paste("Medium population centres", provinces, sep = ", ") %>%
  append("Total, medium population centres")

small_pop <- paste("Small population centres", provinces, sep = ", ") %>%
  append("Total, small population centres")

rural <- paste("Rural areas", provinces, sep = ", ") %>%
  append("Total, rural areas")


core_housing_by_tenure <- raw_data2 %>%
  filter(
    `Core housing need statistics` == "Percentage of households in core housing need"
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    Tenure = `Tenure including first-time homebuyer and social and affordable housing status`,
    GeoCode = GeoUID,
    Value = VALUE
  ) %>%
  # filter out data that duplicates headline data from first table
  filter(
    !(Geography == "Canada (provinces only)" & Tenure == "Total, tenure")
  ) %>%
  # Add column: Geographic location
  mutate(
    `Geographic location` = case_match(
      Geography,
      "Canada (provinces only)" ~ "Total",
      provinces ~ "Total",
      # cma ~ "Census metropolitan area",
      agglomerations ~ "Census agglomerations",
      outside ~ "Outside census metropolitan areas and census agglomerations",
      large_pop ~ "Large urban population centres",
      med_pop ~ "Medium population centres",
      small_pop ~ "Small population centres",
      rural ~ "Rural areas"
      ),
    `Selected census metropolitan areas` = case_when(
      Geography %in% cma ~ Geography
      ),
    Geography = case_when(
      Geography %in% cma ~ NA,
      startsWith(Geography, "Large urban population centres") ~ trimws(gsub(".*,", "", Geography)),
      startsWith(Geography, "Medium population centres") ~ trimws(gsub(".*,", "", Geography)),
      startsWith(Geography, "Small population centres") ~ trimws(gsub(".*,", "", Geography)),
      startsWith(Geography, "Rural areas") ~ trimws(gsub(".*,", "", Geography)),
      startsWith(Geography, "Census agglomerations") ~ trimws(gsub(".*,", "", Geography)),
      startsWith(Geography, "Outside") ~ trimws(gsub(".*,", "", Geography)),
      startsWith(Geography, "Total") ~ "Canada (provinces only)",
      .default = Geography
      )
  ) %>%
  relocate(`Geographic location`, .after = Geography) %>%
  relocate(`Selected census metropolitan areas`, .after = `Geographic location`)


data_final <- 
  bind_rows(
    core_housing_by_tenure,
    core_housing_by_vulnerable_population
  ) %>%
  relocate(`Select housing-vulnerable populations`, .before = GeoCode) %>%
  # Remove geocode for Canada
  mutate(GeoCode = replace(GeoCode, GeoCode == 11124, NA))

data_final$GeoCode <- as.numeric(data_final$GeoCode)


write.csv(data_final, "data/indicator_11-2-1.csv",
          na = "", row.names = FALSE, fileEncoding = "UTF-8")
  