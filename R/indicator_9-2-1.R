# CIF 9.2.1 ---------------------------------------------------------------

# load libraries
library(dplyr)
library(cansim)

# load CODR table from stc api
Raw_data <- get_cansim("27-10-0359-01", factors = FALSE)


# load geocode
geocodes <- read.csv("geocodes.csv")


# Extract geographies
selected_geographies <- c(
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
  "British Columbia"
) 


# Format Table
expenditure <-
  Raw_data %>%
  filter(GEO %in% selected_geographies) %>%
  select(Year = REF_DATE,
         Geography = GEO,
         Value = VALUE) %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = Value) %>%
  rename(data.Geography = Geography)


# Create the total line and non-total line
total <-
  expenditure %>%
  filter(data.Geography == "Canada") %>%
  mutate(data.Geography = "")


non_total <-
  expenditure %>%
  filter(!(data.Geography == "Canada")) %>%
  mutate(data.Geography = paste0("data.", data.Geography))


# Add rows and format
final_data <-
  bind_rows(total, non_total)


write.csv(
  final_data, 
  "data/indicator_9-2-1.csv", 
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
  
  
  
  
  
  
  
