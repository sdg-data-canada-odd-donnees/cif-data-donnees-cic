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

national_species <-
  csi_national %>%
  select(
    Year,
    Value = `National index (cumulative percentage change since 1970)`
  ) %>%
  mutate(
    Series = "By species group"
  )

national_system <-
  csi_national %>%
  select(
    Year,
    Value = `National index (cumulative percentage change since 1970)`
  ) %>%
  mutate(
    Series = "By system"
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
  bind_rows(national_species, bird, mammal, fish)

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
  bind_rows(national_system,terrestrial, freshwater, marine)

# Combine all data sets

data_final <-
  bind_rows(species,system) %>%
  select(
    Year,
    Series,
    `Species group`,
    `System`,
    Value
  ) %>%
  mutate_at("Value", as.numeric)

data_final_with_progress <- data_final %>%
  mutate(
    # Percent change since 1970 values are not valid for the progress calculation because they can be positive and negative.
    # Choose 1970 value to be 1 and use percent change since 1970 values to calculate a valid apparent value for each year.
    # The apparent values can be used for the progress calculation as they are all positive.
    Progress = 1*(1+Value/100)
  ) %>%
  relocate(Progress, .before = "Value")

# Write data to csv
write.csv(data_final_with_progress,
          "data/indicator_15-3-1.csv",
          na = "",
          row.names = FALSE,
          fileEncoding = "UTF-8")