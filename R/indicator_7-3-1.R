
# CIF 7.3.1 ---------------------------------------------------------------

# load libraries
# install.packages("cansim")
library(cansim)
library(dplyr)

# load CODR data
raw_data <- get_cansim("25-10-0029-01", factors = FALSE)
population_ests <- get_cansim("17-10-0005-01", factors = FALSE)

# load geocode file
geocodes <- read.csv("geocodes.csv")


# get the terajoule values of energy consumption
terajoules <- 
  raw_data %>%
  filter(
    REF_DATE >= 2015,
    `Fuel type` == "Total primary and secondary energy",
    `Supply and demand characteristics` == "Energy use, final demand"
  ) %>% 
  select(
    Year = REF_DATE,
    Geography = GEO,
    Terajoules = VALUE
  )

# get the total population estimates
population_ests <- 
  population_ests %>% 
  filter(
    REF_DATE >= 2015,
    Gender == "Total - gender",
    `Age group` == "All ages"
  ) %>% 
  select(
    Year = REF_DATE,
    Geography = GEO,
    Population = VALUE
  )

# calculate terajoules per capita
tj_per_capita <- 
  terajoules %>% 
  inner_join(population_ests) %>% 
  mutate(
    Value = round(Terajoules / Population, 2)
  ) %>% 
  select(-c("Terajoules", "Population")) %>% 
  left_join(geocodes) %>% 
  relocate(GeoCode, .before = Value)

# create the aggregate variable
total_line <- 
  tj_per_capita %>% 
  filter(
    Geography == "Canada"
  ) %>% 
  mutate(
    Geography = ""
  )

# the disaggrete values
not_total_line <- 
  tj_per_capita %>% 
  filter(
    Geography != "Canada"
  ) %>% 
  mutate(
    Geography = paste0("data.", Geography)
  )

# append the total line to the disaggregate rows
data_final <- 
  bind_rows(total_line, not_total_line) %>% 
  rename(data.Geography = Geography)

# write to csv
write.csv(
  data_final,
  "data/indicator_7-3-1.csv",
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)
