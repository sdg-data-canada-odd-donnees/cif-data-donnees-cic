# CIF 9.6.1

# Load required libraries
library(dplyr)
library(stringr)
library(tidyr)
library(httr)
library(jsonlite)
# library(dotenv) # un-comment to run on local system with .env file (1/2)
library(readr)

# Retrieve the API key for electric charging stations from environment variables
# load_dot_env(".env") # un-comment to run on local system with .env file (2/2)
ELECTRIC_CHARGING_STATIONS <- Sys.getenv("ELECTRIC_CHARGING_STATIONS")

# Define the URL for the API endpoint
URL <- "https://developer.nrel.gov/api/alt-fuel-stations/v1.csv?"

# Load geocodes from a CSV file
geocodes <- read.csv("geocodes.csv")

# Define the query parameters for the API request
query_params <- list(
  access = "all", 
  api_key = ELECTRIC_CHARGING_STATIONS, 
  cards_accepted = "all", 
  cng_fill_type = "all", 
  cng_has_rng = "all", 
  cng_psi = "all", 
  country = "CA"
)

# Send the GET request to the API
response <- GET(URL, query = query_params)

# Check if the response status code is 200 (OK)
if (status_code(response) == 200) {
  # If the response is successful, read the content and convert it to a data frame
  content <- content(response, "text")
  data <- read_csv(content, col_types = cols(.default = "?",
                                             `BD Blends` = "c",
                                             `BD Blends (French)` = "c",
                                             `Hydrogen Pressures` = "c",
                                             `Hydrogen Standards` = "c",
                                             `Federal Agency ID` = "c",
                                             `Federal Agency Name` = "c",
                                             `Federal Agency Code` = "c",
                                             `E85 Other Ethanol Blends` = "c",
                                             `NG PSI` = "c",
                                             `CNG PSI` = "c"
                                             )
                   )
  print(head(data))
} else {
  # If the response is not successful, print the content and stop the execution with an error message
  print(content(response, "text"))
  stop("Failed to fetch data: ", status_code(response))
}

# Filter, transform, and select relevant columns from the data
filter_data <-
  data %>%
  filter(
    `Access Code` == "public" # Filter for public access stations
  ) %>%
  select(
    `Open Date`,
    `Date Last Confirmed`,
    `Updated At`,
    Geography = State, # Rename State column to Geography
    `Type of charging or fuelling stations` = `Fuel Type Code` # Rename Fuel Type Code column to Type of charging or fuelling stations
  ) %>%
  mutate( # Rename abbreviations
    `Type of charging or fuelling stations` = case_when(
      `Type of charging or fuelling stations` == "LPG" ~ "Propane",
      `Type of charging or fuelling stations` == "ELEC" ~ "Electric",
      `Type of charging or fuelling stations` == "CNG" ~ "Compressed natural gas",
      `Type of charging or fuelling stations` == "BD" ~ "Biodiesel",
      `Type of charging or fuelling stations` == "HY" ~ "Hydrogen",
      `Type of charging or fuelling stations` == "LNG" ~ "Liquefied natural gas",
      `Type of charging or fuelling stations` == "LPG" ~ "Propane",
      `Type of charging or fuelling stations` == "E85" ~ "Ethanol",
      `Type of charging or fuelling stations` == "RD" ~ "	Renewable diesel",
      TRUE ~ `Type of charging or fuelling stations`
    ),
    `Open Date` = as.Date(`Open Date`, format = "%m/%d/%Y"), # Convert Open Date to Date format
    `Date Last Confirmed` = as.Date(`Date Last Confirmed`, format = "%m/%d/%Y"), # Convert Date Last Confirmed to Date format
    `Updated At` = as.Date(str_sub(`Updated At`, 1, 10), format = "%Y-%m-%d") # Convert Updated At to Date format
  ) %>%
  mutate(
    Year = coalesce(`Open Date`, `Date Last Confirmed`, `Updated At`), # Choose the first non-NA date
    Year = as.numeric(format(Year, "%Y")) # Extract the year from the date
  ) %>%
  mutate( # Rename abbreviations for geography
    Geography = case_when( 
      Geography == "AB" ~ "Alberta",
      Geography == "BC" ~ "British Columbia",
      Geography == "MB" ~ "Manitoba",
      Geography == "NB" ~ "New Brunswick",
      Geography == "NL" ~ "Newfoundland and Labrador",
      Geography == "NS" ~ "Nova Scotia",
      Geography == "NT" ~ "Northwest Territories",
      Geography == "NU" ~ "Nunavut",
      Geography == "ON" ~ "Ontario",
      Geography == "PE" ~ "Prince Edward Island",
      Geography == "QC" ~ "Quebec",
      Geography == "SK" ~ "Saskatchewan",
      Geography == "YT" ~ "Yukon",
      TRUE ~ Geography
    )
  ) %>%
  select(
    Year,
    Geography,
    `Type of charging or fuelling stations`
  ) %>%
  arrange(Year, Geography) # Sort data by Year and Geography

# Count the frequency of each Type of charging or fuelling stations grouped by Year and Geography
frequency_data <- 
  filter_data %>%
  count(Year, Geography, `Type of charging or fuelling stations`, name = "Value")

# Create a summary of Type of charging or fuelling stations frequencies for Canada
canada_frequency <- 
  frequency_data %>%
  group_by(Year, `Type of charging or fuelling stations`) %>%
  summarise(Value = sum(Value), .groups = 'drop') %>%
  mutate(Geography = "Canada")

# Create a summary of total stations for each geography and year
geo_summary <- 
  frequency_data %>%
  group_by(Year, Geography) %>%
  summarise(Value = sum(Value), .groups = 'drop') %>%
  mutate(`Type of charging or fuelling stations` = "Total electric charging and alternative fuelling stations")

# Create a summary of total stations for Canada
canada_total <-
  canada_frequency %>%
  group_by(Year, Geography) %>%
  summarise(Value = sum(Value), .groups = 'drop') %>%
  mutate(
    `Type of charging or fuelling stations` = "",
    Geography = ""
  )

# Combine all data into a final dataset
data_combined <- 
  bind_rows(canada_total, geo_summary, canada_frequency, frequency_data) %>%
  relocate(`Type of charging or fuelling stations`, .before = "Value") %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = "Value")

# Compute the cumulative sum of Value by Year, Geography, and Type of charging or fuelling stations
data_final <- 
  data_combined %>%
  group_by(Geography, `Type of charging or fuelling stations`) %>%
  arrange(Year) %>%
  mutate(Value = cumsum(Value)) %>%
  ungroup() %>%
  filter(
    Year >= 2010 & Year <= format(Sys.Date(), "%Y") # Filter for data from the year 2010 onward until the current year
  ) %>%
  arrange(Geography, Year) 

# Write the final dataset to a CSV file
write.csv(data_final,
          "data/indicator_9-6-1.csv",
          na = "",
          row.names = FALSE,
          fileEncoding = "UTF-8")
