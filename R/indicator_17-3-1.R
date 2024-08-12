# CIF 17.3.1 -------------------------------------------------------

# load libraries
library(cansim)
library(dplyr)
library(httr)
library(jsonlite)
library(dotenv)
library(hablar) # for sum_ function

raw_data <- get_cansim("36-10-0646-01", factors = FALSE)

# Get list of developing countries for each year.
# Developing countries have HDI < 0.8 by UN definition.
# Fetch HDI data from UN Human Development Data API 2.0.
# See https://hdr.undp.org/data-center/documentation-and-downloads
# Manual: https://hdr.undp.org/sites/default/files/2023-24_HDR/HDRO_data_api_manual.pdf

# API key for UN Human Development Data API 2.0
API_key <- Sys.getenv("UN_HUMAN_DEVELOPMENT_DATA_API_KEY") ### SET UP GITHUB SECRETS

# Prepare URL for GET request
base_url <- "https://hdrdata.org/api/CompositeIndices/query-detailed?"
query_params <- list(apikey = API_key, indicator = "HDI")

# Send the GET request to the API
response <- GET(base_url, query = query_params)

# Check if the response status code is 200 (OK)
if (status_code(response) == 200) {
  # If the response is successful, read the content
  resptext <- content(response, "text")
} else {
  # If the response is not successful, print the content and stop the execution with an error message
  print(content(response, "text"))
  stop("Failed to fetch data: ", status_code(response))
}

country_name_map <- list("Bolivia (Plurinational State of)" = "Bolivia",
                         "Congo " = "Democratic Republic of the Congo and Republic of the Congo",
                         "Congo (Democratic Republic of the)" = "Democratic Republic of the Congo and Republic of the Congo",
                         "Eswatini (Kingdom of)" = "Eswatini",
                         "Hong Kong, China (SAR)" = "Hong Kong",
                         "Iran (Islamic Republic of)" = "Iran",
                         "Korea (Republic of)" = "South Korea",
                         "Lao People's Democratic Republic" = "Laos",
                         "Moldova (Republic of)" = "Republic of Moldova",
                         "Syrian Arab Republic" = "Syria",
                         "Tanzania (United Republic of)" = "United Republic of Tanzania",
                         "Turkey" = "Turkiye",
                         "Venezuela (Bolivarian Republic of)" = "Venezuela")
# no HDI data for Kosovo, Monaco, DPR Korea (North Korea), Holy See (Vatican City), Taiwan
# Federated States of Micronesia, San Marino, Tuvalu do not appear in CODR table so SC spelling is unknown

developing_countries <- fromJSON(resptext) %>%
  filter(year >= 2015,
         value < 0.8, # HDI values < 0.8 represent developing countries
         !startsWith(countryIsoCode, "ZZ"), # country codes starting with ZZ represent larger regions, e.g. Sub-Saharan Africa, not relevant to this analysis
         ) %>%
  select(Year = year,
         country,
  )
# rename countries to match presentation in CODR table

for (c in developing_countries$country) {  ### still working on this :(
  developing_countries <- mutate(developing_countries, newname = replace(country, !is.null(country_name_map$c), country_name_map$c))
}

exports <- raw_data %>%
  filter(REF_DATE >= 2015,
         GEO == "Canada",
         Trade == "International exports",
         ) %>%
  select(Year = REF_DATE,
         `Trading partners`,
         `Goods and services (products)`,
         VALUE,
         )

exports_to_developing_countries <- 
  inner_join(exports, developing_countries, by = c("Year", "Trading partners" = "country")) %>%
  summarise(Value = sum_(VALUE), .by = c(Year, `Goods and services (products)`))
  

# Write to csv
write.csv(exports_to_developing_countries, "data/indicator_17-3-1.csv",
          na = "", row.names = FALSE, fileEncoding = "UTF-8")
