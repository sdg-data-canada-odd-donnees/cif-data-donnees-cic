
# 12.2.1 ------------------------------------------------------------------

# load libraries
library(cansim)
library(dplyr)
library(stringr)

geocodes <- read.csv("geocodes.csv")

# load cansim tables
env_protection <- get_cansim("38-10-0132-01", factors = FALSE)
env_management <- get_cansim("38-10-0137-01", factors = FALSE)

# selected environmental activities
activities <- c(
  "Wastewater management",
  "Air pollution management",
  "Protection and remediation of soil, groundwater and surface water",
  "Protection of biodiversity and habitat",
  "Noise and vibration abatement",
  "Total, environmental protection activities",
  "Sold carbon offset credits only or sold more than purchased"
)

env_protection_filtered <-
  env_protection %>%
  filter(`Environmental protection activities` %in% activities) %>%
  mutate(Industries = str_trim(str_remove(Industries, "\\[\\w*\\]"))) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    Industries,
    `Environmental protection activities or management practices` = `Environmental protection activities`,
    Value = VALUE
  )

env_management_filtered <-
  env_management %>%
  filter(
    `Environmental management practices` %in% activities
  ) %>%
  mutate(Industries = str_trim(str_remove(Industries, "\\[\\w*\\]"))) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    Industries,
    `Environmental protection activities or management practices` = `Environmental management practices`,
    Value = VALUE
  )

combined <-
  bind_rows(env_management_filtered,env_protection_filtered) %>%
  na.omit()

# create total line
total_line <- 
  combined %>% 
  filter(
    Geography == "Canada",
    Industries == "Total, industries",
    `Environmental protection activities or management practices` == "Total, environmental protection activities"
  ) %>% 
  mutate_at(2:3, ~ "")

# bind total line to rest of data
data_final <- 
  bind_rows(
    total_line,
    combined %>% 
      filter(
        !( Geography == "Canada" &
           Industries == "Total, industries" &
           `Environmental protection activities or management practices` == "Total, environmental protection activities"
        )
      ) %>%
      filter(!Geography == "British Columbia and the territories")
  ) %>%
  left_join(geocodes, by = "Geography") %>%
  relocate(GeoCode, .before = "Value")

# write to csv
write.csv(
  data_final, 
  "data/indicator_12-2-1.csv", 
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
