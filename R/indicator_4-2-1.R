# CIF 4.2.1 ---------------------------------------------------------------

library(dplyr)
library(cansim)

# load CODR table from stc api
Raw_data <- get_cansim("37-10-0130-01", factors = FALSE)
Raw_data2 <- get_cansim("37-10-0117-01", factors = FALSE) 


# load geocode
geocodes <- read.csv("geocodes.csv")


selected_education <- c(
  "Tertiary education",
  "Short-cycle tertiary",
  "Bachelor's level",
  "Master's or Doctoral level",
  "University"
)


Total_pop <- 
  Raw_data %>% 
  filter(
    REF_DATE >= 2010,
    `Education attainment level` %in% selected_education,
    !(GEO == "Organisation for Economic Co-operation and Development (OECD) - average")
  ) %>% 
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Education attainment level`,
    `Age group`,
    Gender,
    Value = VALUE
  ) %>% 
  mutate(`Population characteristics` = "Total population") %>% 
  relocate(`Population characteristics`, .after = Geography)


# Load the indigenous dataframe  
indg_pop1 <-
  Raw_data2 %>%
  mutate(
    `Educational attainment level` = ifelse(
      `Educational attainment` == "College",
      "Short-cycle tertiary",
      `Educational attainment`
    )
  ) %>%
  filter(
    REF_DATE >= 2010,
    `Characteristics of the Indigenous group aged 25 to 64` == "Indigenous peoples",
    `Educational attainment` %in% c("Short-cycle tertiary", "University")
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Population characteristics` = `Characteristics of the Indigenous group aged 25 to 64`,
    `Education attainment level` = `Educational attainment`,
    Value = VALUE
  ) %>% 
  mutate(
    `Population characteristics` = "Off-reserve Indigenous population"
  )


# Calculate total for Tertiary education education = College + University
total_indg_pop <-
  indg_pop1 %>%
  group_by(
    Year,
    Geography,
    `Population characteristics`
  ) %>% 
  summarise(Value = sum(Value, na.rm = TRUE)) %>%
  mutate(`Education attainment level` = "Tertiary education")


# Bind the total to the total population dataframe 
final_indg <- 
  bind_rows(indg_pop1, total_indg_pop) %>% 
  mutate(
    `Age group` = "Total, 25 to 64 years",
    Gender = "Total - Gender"
  )


# Bind the total population with the indigenous population
all_characteristics <- 
  bind_rows(Total_pop, final_indg) %>% 
  left_join(geocodes, by = "Geography") %>% 
  relocate(GeoCode, .before = Value)


#Create the aggregate and non-aggregate lines for the graphs
total_line <-
  all_characteristics %>%
  filter(
    Geography == "Canada",
    `Population characteristics` == "Total population",
    `Education attainment level` == "Tertiary education",
    `Age group` == "Total, 25 to 64 years",
    Gender == "Total - Gender"
  ) %>%
  mutate_at(2:6, ~ "")


non_total_line <- 
  all_characteristics %>%
  filter(
    !(
      Geography == "Canada" &
        `Population characteristics` == "Total population" &
        `Education attainment level` == "Tertiary education" &
        `Age group` == "Total, 25 to 64 years" &
        Gender == "Total - Gender"
    )
  )


# Bind and format all the total and non-total together 
final_data <- 
  bind_rows(total_line, non_total_line)


write.csv(
  final_data, 
  "data/indicator_4-2-1.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
