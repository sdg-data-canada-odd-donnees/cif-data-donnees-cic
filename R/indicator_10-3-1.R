

#14-10-0340-01

# CIF 10.3.1 ---------------------------------------------------------------

# load libraries
library(dplyr)
library(tidyr)
library(cansim)
library(readr)
library(stringr)



# load CODR table from stc api
Raw_data <- get_cansim("14-10-0340-01", factors = FALSE)



# load geocode
geocodes <- read_csv("geocodes.csv")


#Format table

selected_occupations <- c("Total employees, all occupations",
                          "Management occupations [0]",
                          "Business, finance and administration occupations [1]",
                          "Natural and applied sciences and related occupations [2]",
                          "Health occupations [3]",
                          "Occupations in education, law and social, community and government services [4]",
                          "Occupations in art, culture, recreation and sport [5]",
                          "Sales and service occupations [6]",
                          "Trades, transport and equipment operators and related occupations [7]",
                          "Natural resources, agriculture and related production occupations [8]",
                          "Occupations in manufacturing and utilities [9]")


wage_ratio <- 
  Raw_data %>% 
  filter(REF_DATE >= 2015,
         Wages == "Median hourly gender wage ratio",
         `National Occupational Classification (NOC)` %in% selected_occupations,
         Sex == "Females") %>% 
  select(Year = REF_DATE,
         Geography = GEO,
         `Type of work`,
         `National Occupational Classification (NOC)`,
         Age = `Age group`,
         Value = VALUE) %>% 
  mutate(`National Occupational Classification (NOC)` = 
           str_remove(`National Occupational Classification (NOC)`, "\\[.*\\]")) %>% 
  left_join(geocodes, by = "Geography") %>% 
  relocate(GeoCode, .before = Value)



#Create the total and non-total lines 

total <- 
  wage_ratio %>% 
  filter(Geography == "Canada",
         `Type of work` == "Both full- and part-time employees",
         `National Occupational Classification (NOC)` == "Total employees, all occupations",
         Age == "15 years and over") %>% 
  mutate_at(2:(ncol(.)-2), ~ "")


non_total <- 
  wage_ratio %>% 
  filter(!(Geography == "Canada" &
                  `Type of work` == "Both full- and part-time employees" &
                  `National Occupational Classification (NOC)` == "Total employees, all occupations" &
                  Age == "15 years and over")) %>% 
             mutate_at(2:(ncol(.)-2), ~ paste0("data.", .x))



#Format the finla table 

final_data <- 
  bind_rows(total, non_total)


write_csv(final_data, "CIF/data/indicator_10-3-1.csv", na = "")











