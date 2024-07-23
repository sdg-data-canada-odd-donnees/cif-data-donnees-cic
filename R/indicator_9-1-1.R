#CIF 9.1.1

library(dplyr)
library(cansim)
library(stringr)
library(tidyr)

innovations <- get_cansim("27-10-0402-01", factors = FALSE)

geocodes <- read.csv("geocodes.csv")

industries <- c(
  "Total, all surveyed industries",
  "Agriculture, forestry, fishing and hunting",
  "Mining, quarrying, and oil and gas extraction",
  "Utilities",
  "Construction	",
  "Manufacturing",
  "Wholesale trade",
  "Retail trade",
  "Transportation and warehousing",
  "All transportation",
  "Postal services, couriers and messengers, warehousing and storage",
  "Information and cultural industries",
  "Finance and insurance excluding monetary authorities",
  "Real estate and rental and leasing",
  "Professional, scientific and technical services",
  "Management of companies and enterprises",
  "Administrative and support, waste management and remediation services"
)

innovations_filtered <-
  innovations %>%
  mutate(
    `North American Industry Classification System (NAICS)` = str_remove(`North American Industry Classification System (NAICS)`, " \\[.*\\]")
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    Industry = `North American Industry Classification System (NAICS)`,
    `Enterprise size`,
    `Innovations with environmental benefits`,
    Value = VALUE
  ) %>% 
  filter(
    Industry %in% industries
  ) %>%
  na.omit()

total_line <-
  innovations_filtered %>%
  filter(
    Geography == "Canada" & Industry == "Total, all surveyed industries" & `Enterprise size` == "Total, all enterprise sizes" & `Innovations with environmental benefits` == "Innovations with any environmental benefits"
  ) %>%
  mutate(
    Geography = "",
    Industry = "",
    `Enterprise size` = "",
    `Innovations with environmental benefits` = ""
  )
  
non_total <-
  innovations_filtered %>%
  filter(
    !(Geography == "Canada" & Industry == "Total, all surveyed industries" & `Enterprise size` == "Total, all enterprise sizes" & `Innovations with environmental benefits` == "Innovations with any environmental benefits")
  )

data_final <-
  bind_rows(total_line,non_total) %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = Value)

write.csv(
  data_final,
  "data/indicator_9-1-1.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)