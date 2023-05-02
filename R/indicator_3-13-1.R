# CIF 3.13.1 ---------------------------------------------------------------

# load libraries
library(dplyr)
library(cansim)


# load CODR table from stc api
Raw_data <- get_cansim("13-10-0096-01", factors = FALSE)


# load geocode
geocodes <- read.csv("geocodes.csv")


smoking <-
  Raw_data %>%
  filter(Indicators == "Current smoker, daily or occasional",
         Characteristics == "Percent") %>%
  select(Year = REF_DATE,
         Geography = GEO,
         `Age group`,
         Sex,
         Value = VALUE) %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = Value) %>%
  mutate(Geography = recode(Geography,
                            "Canada (excluding territories)" = "Canada"))


total <-
  smoking %>%
  filter(Geography == "Canada",
         `Age group` == "Total, 12 years and over",
         Sex == "Both sexes") %>%
  mutate_at(2:(ncol(.) - 2), ~ "")


non_total_line <-
  smoking %>%
  filter(!(
    Geography == "Canada" &
      `Age group` == "Total, 12 years and over" &
      Sex == "Both sexes"
  )) %>%
  mutate_at(2:(ncol(.) - 2), ~ paste0("data.", .x))


final_data <-
  bind_rows(total, non_total_line) %>% 
  rename_at(2:(ncol(.) - 2), ~ paste0("data.", .x))


write.csv(final_data,
          "data/indicator_3-13-1.csv",
          na = "",
          row.names = FALSE,
          fileEncoding = "UTF-8")
