# CIF 1.2.1 ---------------------------------------------------------------

# load libraries
library(cansim)
library(dplyr)

# load CODR table from stc api
raw_data <- get_cansim("11-10-0083-01", factors = FALSE)

# load geocode
geocodes <- read.csv("geocodes.csv")


asset_resilience <- 
  raw_data %>% 
  filter(
    `Income measure` == "National after-tax low income measure (LIM-AT)",
    `Confidence intervals` == "Estimate"
  ) %>% 
  select(
    REF_DATE,
    GEO,
    `Age group and family type`,
    Statistics,
    VALUE
  ) %>% 
  rename(
    Year = REF_DATE,
    Geography = GEO,
    Value = VALUE
  ) %>%
  left_join(geocodes, by = "Geography") %>% 
  relocate(GeoCode, .before = "Value")


total_line <- 
  asset_resilience %>%
  filter(
    Geography == "Canada",
    `Age group and family type` == "Persons all ages and family types",
    Statistics == "Persons who are asset resilient for at least three months"
  ) %>% 
  mutate_at(2:(ncol(.)-2), ~ "")

data_final <- 
  bind_rows(
    total_line, 
    asset_resilience %>% 
      filter(
        !(
          Geography == "Canada" &
            `Age group and family type` == "Persons all ages and family types" &
            Statistics == "Persons who are asset resilient for at least three months"
        )
      ) %>%
      mutate_at(2:4, ~ paste0("data.", .x)) 
    ) %>% 
  rename_at(2:4, ~ paste0("data.", .x))

write.csv(data_final,
          "data/indicator_1-2-1.csv",
          na = "",
          row.names = FALSE,
          fileEncoding = "UTF-8")
