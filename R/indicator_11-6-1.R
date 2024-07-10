# CIF 11.6.1 --------------------------------------------------------------------

#load libraries
library(dplyr)
library(cansim)


Raw_data <- get_cansim("38-10-0032-01", factors = FALSE)
Raw_data2 <- get_cansim("17-10-0005-01", factors = FALSE)


# load geocode
geocodes <- read.csv("geocodes.csv")


Population <-
  Raw_data2 %>%
  filter(REF_DATE >= 2002,
         Gender == "Total - gender",
         `Age group` == "All ages") %>%
  select(Year = REF_DATE,
         Geography = GEO,
         Population = VALUE) %>%
  # combine territories' populations to match waste data grouping
  mutate(Geography = replace(Geography, 
                             Geography %in% c("Yukon", "Northwest Territories", "Nunavut"),
                             "Yukon, Northwest Territories and Nunavut")
         ) %>%
  summarise(Population = sum(Population), .by = c("Year", "Geography"))

waste <-
  Raw_data %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Sources of waste for disposal`,
    Value1 = VALUE
  ) %>% 
  mutate(Value1 = Value1 * 1000) %>% # convert from metric tonnes to kgs
  inner_join(Population, by = c("Year", "Geography")) %>%
  mutate(Value = round((Value1 / Population), 1)) %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = Value) %>%
  select(Year,
         Geography,
         `Sources of waste for disposal`,
         GeoCode,
         Value)


# Create the total line
total <-
  waste %>%
  filter(
    Geography == "Canada",
    `Sources of waste for disposal` == "All sources of waste for disposal"
  ) %>%
  mutate_at(2:(ncol(.) - 2), ~ "")


non_total <-
  waste %>%
  filter(
    !(
      Geography == "Canada" &
        `Sources of waste for disposal` == "All sources of waste for disposal"
    )
  ) %>%
  mutate_at(2:(ncol(.) - 2), ~ paste0("data.", .x))


final_data <-
  bind_rows(total, non_total) %>%
  rename_at(2:(ncol(.) - 2), ~ paste0("data.", .x))


write.csv(final_data,
          "data/indicator_11-6-1.csv",
          na = "",
          row.names = FALSE,
          fileEncoding = "UTF-8")
