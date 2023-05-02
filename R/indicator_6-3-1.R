# 6.3.1 -------------------------------------------------------------------

# load libraries
library(cansim)
library(dplyr)

# load cansim table
water_use <- get_cansim("38-10-0250-01", factors = FALSE)

# selected sectors
sectors <- c("Total, industries and households",
             "Total, industries",
             "Households")

# transform data
cubic_metres <- 
  water_use %>% 
  filter(Sector %in% sectors) %>% 
  select(
    Year = REF_DATE,
    Sector,
    Value = VALUE
  ) %>% 
  arrange(Sector, Year) %>%
  mutate(
    Sector = case_when(
      Sector == "Total, industries and households" ~ "",
      Sector == "Total, industries" ~ "data.Industries",
      TRUE ~ paste0("data.", Sector)
    ),
    Units = "Cubic metres"
  ) %>% 
  group_by(Sector)

# calculate growth rate
growth_rate <- 
  cubic_metres %>% 
  transmute(
    Year, Sector,
    Value = ((Value - lag(Value)) / lag(Value)) * 100,
    Value = round(Value, 1),
    Units = "Growth rate"
  )

# bind cubic metres data to growth rate data 
data_final <- 
  bind_rows(cubic_metres, growth_rate) %>% 
  filter(Year >= 2013) %>% 
  ungroup() %>%
  relocate(Units, .after = Year) %>%
  arrange(Units, Sector, Year) %>%
  rename(data.Sector = Sector)

# write data to csv
write.csv(data_final,
          "data/indicator_6-3-1.csv",
          na = "",
          row.names = FALSE,
          fileEncoding = "UTF-8")
