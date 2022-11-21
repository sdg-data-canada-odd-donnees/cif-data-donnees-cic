library(cansim)
library(dplyr)
library(readr)
library(tidyr)

water_use <- get_cansim("38-10-0250-01", factors = FALSE)

sectors <- c(
  "Total, industries and households",
  "Total, industries",
  "Households"
  )

data_final <- 
  water_use %>% 
  filter(Sector %in% sectors) %>% 
  select(
    Year = REF_DATE,
    Sector,
    `Cubic metres` = VALUE
  ) %>% 
  arrange(Sector) %>% 
  mutate(
    Sector = case_when(
      Sector == "Total, industries and households" ~ "",
      Sector == "Total, industries" ~ "data.Industries",
      TRUE ~ paste0("data.", Sector)
    )
  ) %>% 
  group_by(Sector) %>% 
  mutate(
    `Growth rate` = ((`Cubic metres` - lag(`Cubic metres`)) / lag(`Cubic metres`)) * 100,
    `Growth rate` = round(`Growth rate`, 1)
  ) %>% 
  filter(Year >= 2013) %>% 
  ungroup() %>% 
  pivot_longer(
    cols = c("Cubic metres", "Growth rate"),
    names_to = "Units",
    values_to = "Value",
    names_transform = function(x) paste0("data.", x)
  ) %>% 
  relocate(Units, .after = Year) %>% 
  arrange(Units, Sector, Year) %>% 
  rename(data.Sector = Sector)


write_csv(data_final, "data/indicator_6-3-1.csv")
