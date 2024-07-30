## CIF 15.3.1

library(dplyr)
library(readr)
library(stringr)
library(tidyr)

# National Canadian Species Index

csi_national <- read_csv("https://www.canada.ca/content/dam/eccc/documents/csv/cesindicators/canadian-species-index/2023/1-csi-national-en.csv")

colnames(csi_national) <- as.character(unlist(csi_national[2,]))

csi_national = na.omit(csi_national[-2, ])

# Systems Index

csi_system <- read_csv("https://www.canada.ca/content/dam/eccc/documents/csv/cesindicators/canadian-species-index/2023/2-csi-system-en.csv")

colnames(csi_system) <- as.character(unlist(csi_system[2,]))

csi_system = na.omit(csi_system[-2, ])

# Separate into different data sets (national)

national <-
  csi_national %>%
  select(
    Year,
    Value = `National index (cumulative percentage change since 1970)`
  ) %>%
  mutate(
    Series = ""
  )

bird <-
  csi_national %>%
  select(
    Year,
    Value = `Birds index (cumulative percentage change since 1970)`
  ) %>%
  mutate(
    Series = "By species group",
    `Species group` = "Birds index"
  )

mammal <-
  csi_national %>%
  select(
    Year,
    Value = `Mammals index (cumulative percentage change since 1970)`
  ) %>%
  mutate(
    Series = "By species group",
    `Species group` = "Mammals index"
  )

fish <-
  csi_national %>%
  select(
    Year,
    Value = `Fish index (cumulative percentage change since 1970)`
  ) %>%
  mutate(
    Series = "By species group",
    `Species group` = "Fish index"
  )

species <-
  bind_rows(national, bird, mammal, fish)

# Separate into different data sets (system)

terrestrial <-
  csi_system %>%
  select(
    Year,
    Value = `Terrestrial index (percent change since 1970)`
  ) %>%
  mutate(
    Series = "By system",
    `System` = "Terrestrial index"
  )

freshwater <-
  csi_system %>%
  select(
    Year,
    Value = `Freshwater index (percent change since 1970)`
  ) %>%
  mutate(
    Series = "By system",
    `System` = "Freshwater index"
  )

marine <-
  csi_system %>%
  select(
    Year,
    Value = `Marine index (percent change since 1970)`
  ) %>%
  mutate(
    Series = "By system",
    `System` = "Marine index"
  )

system <-
  bind_rows(terrestrial, freshwater, marine)

# Combine all data sets

data_final <-
  bind_rows(species,system) %>%
  select(
    Year,
    Series,
    `Species group`,
    `System`,
    Value
  )

# Write data to csv
write.csv(data_final,
          "data/indicator_15-3-1.csv",
          na = "",
          row.names = FALSE,
          fileEncoding = "UTF-8")