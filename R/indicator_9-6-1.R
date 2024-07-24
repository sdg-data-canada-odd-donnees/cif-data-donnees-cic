#CIF 9.6.1

library(dplyr)
library(stringr)
library(tidyr)
library(httr)
library(jsonlite)
library(dotenv)

url <- "https://developer.nrel.gov/api/alt-fuel-stations/v1.csv?access=public%2Cprivate&api_key=DEMO_KEY&cards_accepted=all&cng_fill_type=all&cng_has_rng=all&cng_psi=all&country=CA&download=true&e85_has_blender_pump=false&ev_charging_level=2%2Cdc_fast&ev_connector_type=all&ev_network=all&federal_funding_types=all&fuel_type=all&hy_is_retail=true&limit=all&lng_has_rng=all&lpg_include_secondary=false&maximum_vehicle_class=all&offset=0&owner_type=all&state=all&status=E&utf8_bom=true"

dotenv::load_dot_env()

ELECTRIC_CHARGING_STATIONS <- Sys.getenv("ELECTRIC_CHARGING_STATIONS")

URL <- "https://developer.nrel.gov/api/alt-fuel-stations/v1.csv?"

query_params <- list(access = "all", api_key = ELECTRIC_CHARGING_STATIONS, cards_accepted = "all", cng_fill_type = "all", cng_has_rng = "all", cng_psi = "all", country = "CA", )

response <- GET(URL, query = query_params)
