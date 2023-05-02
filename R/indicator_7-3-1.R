# CIF 7.3.1 ---------------------------------------------------------------

# load libraries
library(dplyr)
library(cansim)

# load CODR table from stc api
Raw_data <- get_cansim("25-10-0020-01", factors = FALSE)

# load geocode
geocodes <- read.csv("geocodes.csv")

total_electricty <-
  Raw_data %>%
  filter(
    REF_DATE >= 2015,
    `Class of electricity producer` == "Total all classes of electricity producer",
    `Type of electricity generation` == "Total all types of electricity generation"
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Type of electricity generation`,
    `Total generation` = VALUE
  )


renewable <- 
  Raw_data %>%
  filter(
    REF_DATE >= 2015,
    `Class of electricity producer` == "Total all classes of electricity producer",
    `Type of electricity generation` %in% c(
      "Total hydro, tidal, wind, solar and other generation",
      "Hydraulic turbine",
      "Tidal power turbine",
      "Wind power turbine",
      "Solar",
      "Other types of electricity generation"
    )
  ) %>% 
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Type of electricity generation`,
    `total renewable` = VALUE
  ) %>%  
  left_join(total_electricty, by = c("Year", "Geography")) %>% 
  mutate(
    Value = round(
      ((`total renewable` / `Total generation`)*100), 
      digits = 3
    )
  ) %>% 
  select(
    Year, 
    Geography, 
    `Type of electricity generation` =`Type of electricity generation.x`, 
    Value
  ) %>% 
  left_join(geocodes, by = "Geography") %>% 
  relocate(GeoCode, .before = Value)


total_line <-
  renewable %>%
  filter(
    Geography == "Canada",
    `Type of electricity generation` == "Total hydro, tidal, wind, solar and other generation",
  ) %>%
  mutate_at(2:(ncol(.) - 2), ~ "")

non_total_line <-
  renewable %>%
  filter(
    !(
      Geography == "Canada" &
        `Type of electricity generation` == "Total hydro, tidal, wind, solar and other generation"
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







  