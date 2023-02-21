# CIF 9.5.1 ---------------------------------------------------------------

# load libraries
library(dplyr)
library(readr)
library(cansim)


# load CODR table from stc api
Raw_data <- get_cansim("36-10-0655-01", factors = FALSE)

# load geocode
geocodes <- read_csv("geocodes.csv")

selected_assets <- c("Total assets",
                     "Commercial buildings",
                     "Institutional buildings",
                     "Marine engineering infrastructure",
                     "Transportation engineering infrastructure", 
                     "Waterworks infrastructure",
                     "Sewage infrastructure",
                     "Communications networks",
                     "Electric power infrastructure",
                     "Oil and gas engineering construction",
                     "Other engineering construction",
                     "Other machinery and equipment",
                     "Transportation machinery and equipment") 


# Format table
emissions <-
  Raw_data %>%
  filter(
    REF_DATE >= 2015,
    Estimate == "Greenhouse gas emissions per value added",
    Impact == "Total impact",
    Asset %in% selected_assets
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    Asset,
    Value = VALUE
  ) %>% 
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = Value)


# Create the total and non total line
total <-
  emissions %>%
  filter(Geography == "Canada", Asset == "Total assets") %>%
  mutate_at(2:(ncol(.) - 2), ~ "")

non_total <-
  emissions %>%
  filter(!(Geography == "Canada" & Asset == "Total assets")) %>%
  mutate_at(2:(ncol(.) - 2), ~ paste0("data.", .x))


# Bind rows together and format the final table
final_data <- 
  bind_rows(total, non_total) %>% 
  rename_at(c(2:(ncol(.) - 2)), ~ paste0("data.", .x))

write_csv(final_data, "data/indicator_9-5-1.csv", na = "")

