# CIF 7.2.1 ---------------------------------------------------------------

# load libraries
library(dplyr)
library(cansim)
library(hablar)

# load CODR table from stc api
Raw_data <- get_cansim("25-10-0015-01", factors = FALSE) %>% 
  filter(
    # Filter for reference years 2015 to current year - 1
    REF_DATE >= 2015 & REF_DATE < substr(Sys.Date(), 1, 4),
    `Class of electricity producer` == "Total all classes of electricity producer"
  )

# Check if last year in raw data is complete
# i.e. all months up to December are available
if (substr(last(Raw_data$REF_DATE), 6, 7) != "12") {
  # If last year not complete, filter out last year
  Raw_data <- filter(Raw_data, REF_DATE < substr(max(REF_DATE), 1, 4))
}

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

total_electricity <-
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
    summarise(Renewable = sum_(Renewable), .groups = "drop") %>% 
    mutate(`Type of electricity generation` = "Total renewable and non-greenhouse gas emitting sources")


renewable <- 
  renewable_electricity %>% 
  bind_rows(total_renewable_electricity) %>% 
  left_join(total_electricity) %>% 
  mutate(Value = Renewable / Total * 100) %>% 
  mutate(
    Year = substr(Year, 1, 4)
  ) %>% 
  group_by(Year, Geography, `Type of electricity generation`) %>% 
  summarise(Value = round(mean_(Value), digits = 3), .groups = "drop") %>% 
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
  )


final_data <- bind_rows(total_line, non_total_line)


write.csv(final_data,
          "data/indicator_7-2-1.csv",
          na = "",
          row.names = FALSE,
          fileEncoding = "UTF-8")
