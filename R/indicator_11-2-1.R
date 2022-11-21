library(tidyverse)
library(cansim)


core_housing <- get_cansim("46-10-0067-01", factors = FALSE)

names(core_housing)

data_final <- 
  core_housing %>% 
  filter(
    !`Living with housing problems` %in% c("All living situations with or without housing problems"),
    Statistics == "Percentage of households"
  ) %>% 
  select(
    Year = REF_DATE,
    `Select housing-vulnerable populations`,
    `Living with housing problems`,
    Value = VALUE
  )

total_line <- 
  data_final %>% 
  filter(
    `Select housing-vulnerable populations` == "All households",
    `Living with housing problems` == "Living in core housing need"
  ) %>% 
  mutate_at(c(2,3), ~ "")

data_final <- 
  bind_rows(
    total_line,
    
    data_final %>% 
    mutate_at(c(2, 3), ~ paste0("data.", .x)) %>% 
    filter(
      !(`Select housing-vulnerable populations` == "All households" & `Living with housing problems` == "Living in core housing need")
    )
  )

names(data_final)[2:3] <- paste0("data.", names(data_final)[2:3])

write_csv(data_final, "data/indicator_11-2-1.csv", na = "")

