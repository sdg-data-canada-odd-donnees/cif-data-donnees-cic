# 12.2.1 ------------------------------------------------------------------

# load libraries
library(cansim)
library(dplyr)
library(stringr)

geocodes <- read.csv("geocodes.csv")

# load cansim tables
env_protection <- get_cansim("38-10-0132-01", factors = FALSE)
env_management <- get_cansim("38-10-0137-01", factors = FALSE)

env_protection_filtered <-
  env_protection %>%
  mutate(Industries = str_trim(str_remove(Industries, "\\[\\w*\\]"))) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    Industries,
    `Environmental protection activities` = `Environmental protection activities`,
    Value = VALUE
  )

env_management_filtered <-
  env_management %>%
  mutate(Industries = str_trim(str_remove(Industries, "\\[\\w*\\]"))) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    Industries,
    `Environmental management practices` = `Environmental management practices`,
    Value = VALUE
  )

# Don't use na.omit() here - it removes everything!
combined <- bind_rows(env_management_filtered, env_protection_filtered)

# create total line: protection activities
total_line_protection <- 
  combined %>% 
  filter(
    Geography == "Canada",
    Industries == "Total, industries",
    !is.na(`Environmental protection activities`),
    `Environmental protection activities` == "Total, environmental protection activities"
  ) %>% 
  mutate(across(c(Geography, Industries), ~ NA_character_))

# create total line: management practices
total_line_management <- 
  combined %>% 
  filter(
    Geography == "Canada",
    Industries == "Total, industries",
    !is.na(`Environmental management practices`),
    `Environmental management practices` == "Reported using one or more environmental management practices"
  ) %>% 
  mutate(across(c(Geography, Industries), ~ NA_character_))

# bind total lines to rest of data
data_final <- 
  bind_rows(
    total_line_protection,
    total_line_management,
    combined %>% 
      filter(
        !( Geography == "Canada" &
           Industries == "Total, industries" &
           ((!is.na(`Environmental protection activities`) & 
             `Environmental protection activities` == "Total, environmental protection activities") |
            (!is.na(`Environmental management practices`) & 
             `Environmental management practices` == "Reported using one or more environmental management practices"))
        )
      ) %>%
      filter(Geography != "British Columbia and the territories")
  ) %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = "Value")

# Reorder columns
data_final <- data_final[, c("Year","Geography","Industries","Environmental management practices","Environmental protection activities","GeoCode","Value")]

# write to csv
write.csv(
  data_final, 
  "data/indicator_12-2-1.csv", 
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)