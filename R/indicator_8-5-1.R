# CIF 8.5.1 ---------------------------------------------------------------

# load libraries
library(dplyr)
library(cansim)


# load CODR table from stc api
Raw_data <- get_cansim("36-10-0222-01", factors = FALSE)
Raw_data2 <- get_cansim("17-10-0005-01", factors = FALSE)


# load geocode
geocodes <- read.csv("geocodes.csv")

# Format the population table
population <-
  Raw_data2 %>%
  filter(REF_DATE >= 2015,
         `Age group` == "All ages",
         Gender == "Total - gender") %>%
  select(Year = REF_DATE,
         Geography = GEO,
         Population = VALUE)


# Format GDP table and merge population table
GDP <-
  Raw_data %>%
  filter(
    REF_DATE >= 2015,
    GEO != "Outside Canada",
    GEO != "Northwest Territories including Nunavut",
    Prices == "Current prices",
    Estimates == "Gross domestic product at market prices",
  ) %>%
  select(Year = REF_DATE,
         Geography = GEO,
         TotalGDP = VALUE) %>%
  inner_join(population, by = c("Year", "Geography")) %>% 
  mutate(
    Value = round(((TotalGDP / Population) * 1000000), digits = 0)
  ) %>%
  select(Year,
         Geography,
         Value) %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = Value)


# Create the aggregate and non aggregate
total <-
  GDP %>%
  filter(Geography == "Canada") %>%
  mutate_at(2:(ncol(.) - 2), ~ "")


non_total <-
  GDP %>%
  filter(!(Geography == "Canada")) %>%
  mutate_at(2:(ncol(.) - 2), ~ paste0("data.", .x))


# Bind the rows together and format and export the final table
final_data <-
  bind_rows(total, non_total) %>%
  rename(data.Geography = Geography)


write.csv(
    final_data, 
    "data/indicator_8-5-1.csv", 
    na = "",
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
