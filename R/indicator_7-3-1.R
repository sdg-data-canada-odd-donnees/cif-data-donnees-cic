# CIF 7.3.1 ---------------------------------------------------------------

# load libraries
library(dplyr)
library(cansim)

# load CODR table from stc api
Raw_data <- 
  get_cansim("25-10-0015-01", factors = FALSE) %>% 
  filter(
    REF_DATE >= 2015 & REF_DATE < substr(Sys.Date(), 1, 4),
    `Class of electricity producer` == "Total all classes of electricity producer"
  )

# load geocode
geocodes <- read.csv("geocodes.csv")

generation_types <- c(
  # "Total all types of electricity generation",
  "Hydraulic turbine",
  "Nuclear steam turbine",
  "Total electricity production from biomass",
  "Tidal power turbine",
  "Wind power turbine",
  "Solar"
)

total_electricty <-
  Raw_data %>%
  filter(
    `Type of electricity generation` == "Total all types of electricity generation"
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    # `Type of electricity generation`,
    Total = VALUE
  )

renewable_electricity <- 
  Raw_data %>% 
    filter(
      `Type of electricity generation` %in% generation_types
    ) %>% 
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Type of electricity generation`,
    Renewable = VALUE
  )

total_renewable_electricity <-
  renewable_electricity %>% 
    group_by(Year, Geography) %>% 
    summarise(Renewable = sum(Renewable), .groups = "drop") %>% 
    mutate(`Type of electricity generation` = "Total renewable and non-greenhouse gas emitting sources")


renewable <- 
  renewable_electricity %>% 
  bind_rows(total_renewable_electricity) %>% 
  left_join(total_electricty) %>% 
  mutate(Value = round((Renewable / Total)*100, digits = 3)) %>% 
  mutate(
    Year = substr(Year, 1, 4)
  ) %>% 
  group_by(Year, Geography, `Type of electricity generation`) %>% 
  summarise(Value = mean(Value), .groups = "drop") %>% 
  left_join(geocodes, by = "Geography") %>% 
  relocate(GeoCode, .before = Value)


total_line <-
  renewable %>%
  filter(
    Geography == "Canada",
    `Type of electricity generation` == "Total renewable and non-greenhouse gas emitting sources",
  ) %>%
  mutate_at(2:(ncol(.) - 2), ~ "")

non_total_line <-
  renewable %>%
  filter(
    !(
      Geography == "Canada" &
        `Type of electricity generation` == "Total renewable and non-greenhouse gas emitting sources"
    )
  ) %>%
  mutate_at(2:(ncol(.) - 2), ~ paste0("data.", .x))


final_data <-
  bind_rows(total_line, non_total_line) %>%
  rename_at(2:(ncol(.) - 2), ~ paste0("data.", .x))


write.csv(final_data,
          "data/indicator_7-3-1.csv",
          na = "",
          row.names = FALSE,
          fileEncoding = "UTF-8")







  