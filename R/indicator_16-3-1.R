# CIF 16.3.1 --------------------------------------------------------------------

# load libraries
library(dplyr)
library(cansim)

Raw_data <- get_cansim("35-10-0001-01", factors = FALSE)

cyber_violation <- c(
  "Total, all violations",
  "Luring a child via a computer",
  "Non-consensual distribution of intimate images",
  "Extortion",
  "Criminal harassment",
  "Indecent/Harassing communications",
  "Uttering threats",
  "Fraud",
  "Identity theft",
  "Identity fraud",
  "Child pornography",
  "Making or distribution of child pornography"
)

cyber_crime <-
  Raw_data %>%
  filter(
    REF_DATE >= 2015,
    `Cyber-related violation` %in% cyber_violation
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Cyber-related violation`,
    Value = VALUE
  ) %>%
  mutate(
    Geography = recode(
      Geography,
      "Canada, selected police services" = "Canada"
    )
  ) %>%
  mutate(`Type of cyber-related violation` = "Against a person") %>%
  relocate(`Type of cyber-related violation`, .after = Geography)


# Create the total and non-total line
total <-
  cyber_crime %>%
  filter(Geography == "Canada",
         `Cyber-related violation` == "Total, all violations") %>%
  mutate_at(2:(ncol(.) - 1), ~ "")

non_total <-
  cyber_crime %>%
  filter(!(
    Geography == "Canada" &
      `Cyber-related violation` == "Total, all violations"
  )) %>%
  mutate_at(2:(ncol(.) - 1), ~ paste0("data.", .x))

# Format final table and export to csv
final_data <-
  bind_rows(total, non_total) %>%
  rename_at(2:(ncol(.) - 1), ~ paste0("data.", .x))

write.csv(final_data,
          "data/indicator_16-3-1.csv",
          na = "",
          row.names = FALSE,
          fileEncoding = "UTF-8")
