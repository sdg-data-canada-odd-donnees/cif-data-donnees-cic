# CIF 12.4.1 -------------------------------------------------------

library(dplyr)
library(cansim)

plastic <- get_cansim("38-10-0150-01", factors = FALSE)
geocodes <- read.csv("geocodes.csv")

product_types <-
  plastic %>%
  filter(
    REF_DATE >= 2015,
    Variable == "Plastic leaked permanently into the environment"
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Product category`,
    product_types = VALUE
  )

all_plastic <-
  plastic %>%
  filter(
    REF_DATE >= 2015,
    Variable == "Total disposed plastic waste and scrap",
    `Product category` == "Total, all product categories"
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Product category`,
    Value = VALUE
  ) %>%
  rename(all_product = Value)

proportion <-
  left_join(all_plastic, product_types, c("Year", "Geography")) %>%
  mutate(Value = round((product_types / all_product) * 100, 2)) %>%
  select(
    Year,
    Geography,
    `Product category` = `Product category.y`,
    Value
  ) %>%
  na.omit()

total_line <-
  proportion %>%
  filter(
    Geography == "Canada",
    `Product category` == "Total, all product categories"
  ) %>%
  mutate(
    Geography = "",
    `Product category` = ""
  )

non_total <-
  proportion %>%
  filter(
    !(
      Geography == "Canada" & `Product category` == "Total, all product categories"
    )
  )

data_final <-
  bind_rows(total_line,non_total) %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = "Value")

write.csv(data_final,
          "data/indicator_12-4-1.csv",
          na = "",
          row.names = FALSE,
          fileEncoding = "UTF-8")