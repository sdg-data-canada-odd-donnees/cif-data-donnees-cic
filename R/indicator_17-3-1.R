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
# load_dot_env(".env") # un-comment to run on local system with .env file
API_key <- Sys.getenv("UN_HUMAN_DEVELOPMENT_DATA_API_KEY")

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

# Create mapping between countries with different names in UN database and CODR table
country_name_map <- list("Bolivia (Plurinational State of)" = "Bolivia",
                         "Congo" = "Democratic Republic of the Congo and Republic of the Congo",
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
# Countries appearing in CODR table but not in UN HDI database: DPR Korea (North Korea), Holy See (Vatican City), Kosovo, Monaco, Taiwan
# Countries appearing in UN HDI database but not in CODR table: Federated States of Micronesia, San Marino, Tuvalu

developing_countries <- fromJSON(resptext) %>%
  filter(year >= 2015,
         value < 0.8, # HDI values < 0.8 represent developing countries
         !startsWith(countryIsoCode, "ZZ"), # country codes starting with ZZ represent larger regions, e.g. Sub-Saharan Africa, not relevant to this analysis
         ) %>%
  select(Year = year,
         Country = country,
  ) %>%
  # rename countries in UN database to match presentation in CODR table
  mutate(Country = replace(Country, Country %in% names(country_name_map), unlist(country_name_map[Country]))) %>%
  # the mapping allows for duplicate rows because both Congos are combined under a single name in the CODR table
  # remove duplicate rows --> if at least one of the Congos is "developing", the exports to both will be considered in the final output
  distinct()

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
  inner_join(exports, developing_countries, by = c("Year", "Trading partners" = "Country")) %>%
  summarise(Value = sum_(VALUE), .by = c(Year, `Goods and services (products)`))

# Write to csv
write.csv(exports_to_developing_countries, "data/indicator_17-3-1.csv",
          na = "", row.names = FALSE, fileEncoding = "UTF-8")
