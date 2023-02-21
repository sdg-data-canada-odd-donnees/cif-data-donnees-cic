
#37-10-0196-01

# CIF 8.3.1 ---------------------------------------------------------------

# load libraries
library(dplyr)
library(tidyr)
library(cansim)
library(readr)


# load CODR table from stc api
Raw_data <- get_cansim("37-10-0196-01", factors = FALSE)


# load geocode
geocodes <- read_csv("geocodes.csv")


#Format Table 


prop_youth <- 
  Raw_data %>% 
  filter(REF_DATE >= 2015,
         GEO != "Organisation for Economic Co-operation and Development (OECD) - average",
         `Educational attainment level` == "Total, all education levels",
          `Labour force and education status` == "Sub-total, not in employed, education or training (NEET)",
         Statistics == "Proportion") %>% 
  select(Year = REF_DATE, 
         Geography = GEO,
         `Age group`,
         Sex,
         Value = VALUE) %>% 
  left_join(geocodes, by = "Geography") %>% 
  relocate(GeoCode, .before = Value)




#Create the aggregate table

total <- 
  prop_youth %>% 
  filter(Geography == "Canada",
         `Age group` == "Total, 15 to 29 years",
         Sex == "Both sexes") %>% 
  mutate_at(2:(ncol(.)-2), ~ "")


#Create the non-aggregate

non_total <- 
  prop_youth %>% 
  filter(!(Geography == "Canada" & 
             `Age group` == "Total, 15 to 29 years" &
             Sex == "Both sexes")) %>% 
  mutate_at(2:(ncol(.)-2), ~ paste0("data.", .x))


#Add the rows together to create the final

final_data <- 
  bind_rows(total, non_total)

names(final_data)[2:(ncol(final_data)-2)] <- 
  paste0("data.", names(final_data)[2:(ncol(final_data)-2)])  

write_csv(final_data, "CIF/data/indicator_8-3-1.csv", na = "")  
  
  
  
  
  
  
  



