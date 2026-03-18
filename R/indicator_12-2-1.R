# 12.2.1 ------------------------------------------------------------------

# load libraries
library(cansim)
library(dplyr)
library(stringr)

geocodes <- read.csv("geocodes.csv")

# load cansim tables
act_env_protection <- get_cansim("38-10-0132-01", factors = FALSE)
act_gest_ressources <- get_cansim("38-10-0147-01", factors = FALSE)
pra_env_management <- get_cansim("38-10-0137-01", factors = FALSE)


env_protection_filtered <-
  act_env_protection %>%
  mutate(Industries = str_trim(str_remove(Industries, "\\[\\w*\\]"))) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    Industries,
    `Environmental protection activities` = `Environmental protection activities`,
    Value = VALUE
  )

gest_ressources_filtered <-
  act_gest_ressources %>%
  mutate(Industries = str_trim(str_remove(Industries, "\\[\\w*\\]"))) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    Industries,
    `Resource management activities` = `Resource management activities`,
    Value = VALUE
  )

env_management_filtered <-
  pra_env_management %>%
  mutate(Industries = str_trim(str_remove(Industries, "\\[\\w*\\]"))) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    Industries,
    `Environmental management practices` = `Environmental management practices`,
    Value = VALUE
  )




# Don't use na.omit() here - it removes everything!
combined <- bind_rows(env_management_filtered, env_protection_filtered, gest_ressources_filtered)

# create total line: protection activities
total_line_protection <- 
  combined %>% 
  filter(
    Geography == "Canada",
    Industries == "Total, industries",
    !is.na(`Environmental protection activities`),
    `Environmental protection activities` == "Total, environmental protection activities"
  )

# create total line: management practices
total_line_management <- 
  combined %>% 
  filter(
    Geography == "Canada",
    Industries == "Total, industries",
    !is.na(`Environmental management practices`),
    `Environmental management practices` == "Reported using one or more environmental management practices"
  )

# create total line: getion ressources
total_line_gestion <- 
  combined %>% 
  filter(
    Geography == "Canada",
    Industries == "Total, industries",
    !is.na(`Resource management activities`),
    `Resource management activities` == "Total, resource management activities"
  )

# bind total lines to rest of data
data_final <- 
  bind_rows(
    total_line_protection,
    total_line_management,
    total_line_gestion,
    combined %>% 
      filter(
        !( Geography == "Canada" &
           Industries == "Total, industries" &
           ((!is.na(`Environmental protection activities`) & 
             `Environmental protection activities` == "Total, environmental protection activities") |
            (!is.na(`Resource management activities`) & 
             `Resource management activities` =="Total, resource management activities") |
            (!is.na(`Environmental management practices`) & 
             `Environmental management practices` == "Reported using one or more environmental management practices"))
        )
      ) %>%
      filter(Geography != "British Columbia and the territories")
  )  %>%
  left_join(geocodes, by = "Geography")

#create series
# Create series
data_final$Series <- NA_character_

data_final$Series[!is.na(data_final$`Environmental management practices`)]  <- "Environmental management practices"
data_final$Series[!is.na(data_final$`Resource management activities`)]       <- "Environmental protection activities"
data_final$Series[!is.na(data_final$`Environmental protection activities`)]  <- "Environmental protection activities"

# Reorder columns
data_final <- data_final[, c("Year","Series","Geography","Industries","Environmental management practices","Resource management activities","Environmental protection activities","GeoCode","Value")]


# write to csv
write.csv(
  data_final, 
  "data/indicator_12-2-1.csv", 
  na = "",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)