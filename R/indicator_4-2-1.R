#13-10-0096-01

# CIF 4.2.1 ---------------------------------------------------------------

# load libraries
library(dplyr)
library(tidyr)
library(cansim)
library(readr)
library(stringr)

# load CODR table from stc api
Raw_data <- get_cansim("37-10-0130-01", factors = FALSE)
Raw_data2 <- get_cansim("37-10-0117-01", factors = FALSE) 


# load geocode
geocodes <- read_csv("geocodes.csv")


#load the total population data set 

selected_education <- c(
  "Tertiary education",
  "Short-cycle tertiary",
  "Bachelor's level",
  "Master's or Doctoral level",
  "University"
  
)

Total_pop <- 
  Raw_data %>% 
  filter(REF_DATE >= 2010,
         `Educational attainment level` %in% selected_education ,
  ) %>% 
  filter(!(GEO == "Organisation for Economic Co-operation and Development (OECD) - average")) %>% 
  select(Year = REF_DATE, Geography = GEO, `Educational attainment level`,
         `Age group`, Sex, Value = VALUE)

Total_pop$`Population characteristics` <- "Total population"
Total_pop <- 
  Total_pop %>% relocate(`Population characteristics`, .after = Geography)






#Load the indigenous table for Tertiary education education - College + University 



indg_pop1 <- 
  Raw_data2 %>% 
  mutate(across(`Educational attainment level`, str_replace, "College","Short-cycle tertiary")) %>% 
  filter(REF_DATE >= 2010, 
         `Population characteristics` == "Off-reserve Indigenous  population", 
         `Educational attainment level` %in% c("Short-cycle tertiary", "University")) %>% 
  select(Year = REF_DATE, Geography = GEO, 
         `Population characteristics`, `Educational attainment level`,
         Value = VALUE) %>% 
  group_by(Year, Geography, `Population characteristics`, `Educational attainment level`) %>% 
  summarise(Value = sum(Value, na.rm = TRUE)) %>% 
  pivot_wider(names_from = `Educational attainment level`, values_from = Value) %>% 
  mutate(Value = `Short-cycle tertiary` + University) %>% 
  select(Year, Geography, `Population characteristics`, Value) %>% 
  mutate(`Educational attainment level` = "Tertiary education") %>% 
  relocate(`Educational attainment level`, .before = Value)






#load the indigenous table for short cycle and University stats 

indg_pop2 <- 
  Raw_data2 %>% 
  mutate(across(`Educational attainment level`, str_replace, 
                "College", "Short-cycle tertiary")) %>% 
  filter(REF_DATE >= 2010, 
         `Population characteristics` == "Off-reserve Indigenous  population",
         `Educational attainment level`
         %in% c("Short-cycle tertiary", "University")) %>% 
  select(Year = REF_DATE, Geography = GEO, 
         `Population characteristics`, 
         `Educational attainment level`,
         Value = VALUE)





#Bind the two data sets together 

final_indg <- 
  bind_rows(indg_pop2, indg_pop1)




#Create the Age group and sex column in the indigenous table 

final_indg$`Age group` <- "Total, 25 to 64 years"
final_indg$Sex <- "Both sexes"

final_indg <- 
  final_indg %>% relocate(Sex, .before = Value) %>% 
  relocate(`Age group`, .after = `Educational attainment level`)






#Bind the total population with the indigenous population

all_characteristics <- 
  bind_rows(Total_pop, final_indg) %>% 
  left_join(geocodes, by = "Geography") %>% 
  relocate(GeoCode, .before = Value)



#Create the aggregate and non-aggregate lines for the graphs

total_line <-
  all_characteristics %>% 
  filter(Geography == "Canada", 
         `Population characteristics` == "Total population",
         `Educational attainment level` == "Tertiary education",
         `Age group` == "Total, 25 to 64 years",
         Sex == "Both sexes") %>% 
  mutate_at(2:(ncol(.)-2), ~ "")

non_total_line <- 
  all_characteristics %>% 
  filter(!(Geography == "Canada" &
             `Population characteristics` == "Total population" &
             `Educational attainment level` == "Tertiary education" &
             `Age group` == "Total, 25 to 64 years" &
             Sex == "Both sexes")) %>% 
  mutate_at(2:(ncol(.)-2), ~ paste0("data.", .x)) 



#Bind and format all the total and non-total together 

final_data <- 
  bind_rows(total_line, non_total_line)

names(final_data)[2:(ncol(final_data)-2)] <-
  paste0("data.", names(final_data)[2:(ncol(final_data)-2)])

write_csv(final_data, "CIF/data/indicator_4-2-1.csv", na = "")







