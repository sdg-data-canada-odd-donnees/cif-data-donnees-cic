#13-10-0834-01

# CIF 2.1.1 ---------------------------------------------------------------

# load libraries
library(dplyr)
library(tidyr)
library(cansim)
library(readr)

# load CODR table from stc api
raw_data <- get_cansim("13-10-0834-01", factors = FALSE)

# load geocode
geocodes <- read_csv("geocodes.csv")



#Format the table 


food_insecurity <- 
  raw_data %>%
  filter(Statistics == "Percentage of persons") %>%
  select(REF_DATE, GEO, `Economic family type`, `Household food security status`, 
         VALUE) %>%
  rename(Year = REF_DATE, Geography = GEO, Value = VALUE) %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = Value)



#Create the aggregate line

total_line <- 
  food_insecurity %>%
  filter(Geography == "Canada", `Economic family type` == "All persons",
         `Household food security status` == "Food insecure, moderate or severe") %>%
  mutate_at(2:(ncol(.)-2), ~ "")



#Create the non - aggregate data 

food_insecurity <- 
  food_insecurity %>%
  filter(!(Geography == "Canada" & `Economic family type` == "All persons" & 
             `Household food security status` == "Food insecure, moderate or 
          severe")) %>%
  mutate_at(2:(ncol(.)-2), ~ paste0("data.", .x))


#Add the aggregate and non - aggregate data

data_final <- 
  bind_rows(total_line, food_insecurity)

#rename columns

names(data_final)[2:(ncol(data_final)-2)] <- paste0("data.", names(data_final)[2:(ncol(data_final)-2)])

#Write the csv file

write_csv(data_final, "CIF/data/indicator_2-1-1.csv", na = "")










