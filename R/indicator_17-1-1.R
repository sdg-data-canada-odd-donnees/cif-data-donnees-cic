# CIF 17.1.1 -----------------------------------------------------

library(dplyr)
library(readr)
library(tidyr)

# Source: World Bank SPI Github repository
raw_csv <- "https://raw.github.com/worldbank/SPI/master/03_output_data/SPI_index.csv"

data <- read.csv(raw_csv)

SPI_index <- data %>%
  filter(
    country == "Canada"
  ) %>%
  select(
    Year = date,
    `Pillar 1 - Data Use` = SPI.INDEX.PIL1,
    `Pillar 2 - Data Services` = SPI.INDEX.PIL2,
    `Pillar 3 - Data Products` = SPI.INDEX.PIL3,
    `Pillar 4 - Data Sources` = SPI.INDEX.PIL4,
    `Pillar 5 - Data Infrastructure` = SPI.INDEX.PIL5,
    SPI.INDEX
  ) %>%
  gather(key = "Pillar", value = "Value", -Year) %>%
  na.omit() %>%
  mutate(
    Pillar = case_match(Pillar, "SPI.INDEX" ~ NA, .default = Pillar)
  ) %>%
  arrange(desc(Year))

write.csv(SPI_index, "data/indicator_17-1-1.csv",
          na = "", row.names = FALSE, fileEncoding = "UTF-8")
