# CIF 10.4.1 ---------------------------------------------------------------

# load libraries
library(dplyr)
library(cansim)


# load CODR table from stc api
Raw_data <- get_cansim("11-10-0190-01", factors = FALSE)


# load geocode
geocodes <- read.csv("geocodes.csv")


# Format table
After_tax_income <-
  Raw_data %>%
  filter(REF_DATE >= 2015,
         `Income concept` == "Median after-tax income") %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Economic family type`,
    Value = VALUE
  ) %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = Value)


# Create total and non total tables
total <-
  After_tax_income %>%
  filter(
    Geography == "Canada",
    `Economic family type` == "Economic families and persons not in an economic family"
  ) %>%
  mutate_at(2:(ncol(.) - 2), ~ "")


non_total <-
  After_tax_income %>%
  filter(
    !(
      Geography == "Canada" &
        `Economic family type` == "Economic families and persons not in an economic family"
    )
  ) %>%
  mutate_at(2:(ncol(.) - 2), ~ paste0("data.", .x))


# Format the final table and write the csv
final_data <-
  bind_rows(total, non_total) %>%
  rename_at(2:(ncol(.) - 2), ~ paste0("data.", .x))


write.csv(final_data,
          "data/indicator_1-4-1.csv",
          na = "",
          row.names = FALSE,
          fileEncoding = "UTF-8")
