
# 12.2.1 ------------------------------------------------------------------

# load libraries
library(cansim)
library(dplyr)
library(stringr)

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

# function to apply to both data tables to format
env_data <- function(data) {
  
  data %>%
    select(
      Year = REF_DATE,
      Industries,
      `Environmental protection activities or management practices` = starts_with("Env"),
      Value = VALUE
    ) %>%
    filter(`Environmental protection activities or management practices` %in% activities) %>%
    mutate(Industries = str_trim(str_remove(Industries, "\\[\\w*\\]")))
  
}

# bind data together 
data_final <- bind_rows(
  env_data(env_protection),
  env_data(env_management)
)

# create total line
total_line <- 
  data_final %>% 
  filter(
    Industries == "Total, industries",
    `Environmental protection activities or management practices` == "Total, environmental protection activities"
  ) %>% 
  mutate_at(2:3, ~ "")

# bind total line to rest of data
data_final <- 
  bind_rows(
    total_line,
    data_final %>% 
    filter(
     !( Industries == "Total, industries" &
      `Environmental protection activities or management practices` == "Total, environmental protection activities"
      )
    ) %>% 
    mutate_at(2:3, ~ paste0("data.", .x))
  ) %>% 
  rename_at(2:3, ~ paste0("data.", .x))

# write to csv
write.csv(
  data_final, 
  "data/indicator_12-2-1.csv", 
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
