
# CIF 16.5.1 -------------------------------------------------------------

# load packages
library(cansim)
library(dplyr)

# load cansim tables
adults_courts <- get_cansim("35-10-0029-01", factors = FALSE)
youth_courts <- get_cansim("35-10-0040-01", factors = FALSE)

# read in geocodes
geocodes <- read.csv("geocodes.csv")

selected_offenses <- c(
  "Total offences",
  "Total Criminal Code",
  "Crimes against the person",
  "Crimes against property",
  "Administration of justice",
  "Criminal Code traffic"
)

court_time <- function(data, type) {
  
  data %>% 
    filter(
      substr(REF_DATE, 1,  4) >= 2014,
      !GEO %in% c("Ten jurisdictions", "Eight jurisdictions"),
      Offences %in% selected_offenses,
      `Type of case` == "Total cases",
      Statistics == "Median"
    ) %>% 
    select(
      Year = REF_DATE,
      Geography = GEO,
      Offences,
      `Age of accused`,
      `Sex of accused`,
      Value = VALUE
    ) %>% 
    mutate(Series = type) %>% 
    relocate(Series, .after = Year)
  
}

all_court_time <- 
  bind_rows(
    court_time(adults_courts, "Adult criminal courts"),
    court_time(youth_courts, "Youth courts")
  ) %>% 
  left_join(geocodes) %>% 
  relocate(GeoCode, .before = Value)
  
total_line <-
  all_court_time %>% 
    filter(
      Geography == "Canada",
      Offences == "Total offences",
      `Age of accused` == "Total, age of accused",
      `Sex of accused` == "Total, sex of accused"
    ) %>% 
    mutate_at(3:6, ~ "")

non_total <- 
all_court_time %>%
  filter(
    !(
      Geography == "Canada" &
        Offences == "Total offences" &
        `Age of accused` == "Total, age of accused" &
        `Sex of accused` == "Total, sex of accused"
    )
  ) %>% 
  mutate_at(3:6, ~ paste0("data.", .x))

data_final <- 
  bind_rows(
    total_line,
    non_total
  ) %>% 
  rename_at(3:6, ~ paste0("data.", .x))

write.csv(
  data_final, 
  "data/indicator_16-5-1.csv",
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
