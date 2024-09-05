# CIF 2.1.1 ---------------------------------------------------------------

# load libraries
library(dplyr)
library(cansim)

# load CODR table from stc api
economic_families <- get_cansim("13-10-0834-01", factors = FALSE)
demographic_characteristics <- get_cansim("13-10-0835-01", factors = FALSE)

# load geocode
geocodes <- read.csv("geocodes.csv")


# Manual input data for territories
# Sources
# 2020: https://www150.statcan.gc.ca/n1/en/daily-quotidien/221103/dq221103d-eng.pdf
# 2021: https://www150.statcan.gc.ca/n1/daily-quotidien/230621/dq230621c-eng.htm
# 2022: https://www150.statcan.gc.ca/n1/daily-quotidien/240619/dq240619d-eng.htm
territories <- c("Yukon", "Northwest Territories", "Nunavut")
nterritories <- length(territories)
years <- c("2020", "2021", "2022")
nyears <- length(years)
# food_insecurity_status <- c("food insecure", "food secure")
# nstatus <- length(food_insecurity_status)

# Sources give % of food insecure households (marginal, moderate or severe)
values_insecure <- c(21.2, # 2020 YT
                     20.4, #      NT
                     49.5, #      NU
                     12.8, # 2021 YT
                     22.2, #      NT
                     46.1, #      NU
                     21.4, # 2022 YT
                     27.6, #      NT
                     62.6) #      NU
# % of food secure = 100 - % of food insecure
values_secure <- 100 - values_insecure

# Build data frame for manually input data for territories
df_territories <- tibble(
    Year = rep(years, each = nterritories),
    Geography = rep(territories, nyears),
    `Household food security status` = "Food insecure",
    Value = values_insecure,
  ) %>%
  add_row(
    Year = rep(years, each = nterritories),
    Geography = rep(territories, nyears),
    `Household food security status` = "Food secure",
    Value = values_secure,
  ) %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = Value)


# Format the table 
filter_economic_families <- 
  economic_families %>%
  filter(Statistics == "Percentage of persons") %>%
  select(Year = REF_DATE, 
         Geography = GEO, 
         `Economic family type`, 
         `Household food security status`, 
         Value = VALUE)

filter_demographic_characteristics <-
  demographic_characteristics %>%
  filter(Statistics == "Percentage of persons") %>%
  filter(!(GEO == "Canada" & `Demographic characteristics` == "All persons")) %>%
  select(Year = REF_DATE, 
         Geography = GEO, 
         `Demographic characteristics`, 
         `Household food security status`, 
         Value = VALUE)
  
food_insecurity <- bind_rows(filter_economic_families,
                             filter_demographic_characteristics) %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = Value) %>%
  relocate(`Demographic characteristics`, .before = `Household food security status`)

# Create the aggregate line
total_line <- 
  food_insecurity %>%
  filter(Geography == "Canada", `Economic family type` == "All persons",
         `Household food security status` == "Food insecure, moderate or severe") %>%
  mutate_at(2:(ncol(.)-2), ~ NA)

# Create the non - aggregate data 
food_insecurity <-
  food_insecurity %>%
  filter(!(Geography == "Canada" & `Economic family type` == "All persons" & 
             `Household food security status` == "Food insecure, moderate or severe"))

# Add the aggregate and non - aggregate data
data_final <- bind_rows(total_line, 
                        df_territories, 
                        food_insecurity)

# Write the csv file
write.csv(data_final, "data/indicator_2-1-1.csv",
          na = "", row.names = FALSE, fileEncoding = "UTF-8")
