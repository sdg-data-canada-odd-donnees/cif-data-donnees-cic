# Test script
# Testing the behaviour of accented characters handled by R scripts

# Summary of findings:
# OK:
# 1-1-1: data.Québec, Quebec / data.Montréal, Quebec
# 16-7-1: First Nations, Métis, or Inuit
# 1-4-1: data.Québec, Quebec / data.Montréal, Quebec
# 2-1-1: Métis aged 15 years and over
# 8-2-1: *many
# Issues:
# 4-1-1: Métis --- used in R script
# 5-2-1: Métis --- used in R script
# 16.1.1: Total - Visible minority --- used in R script
# ***only an issue when the accented character is used in the R script, fine otherwise.

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(cansim))

# Test 1: print an accented character
string <- "Éléphant"
print(string)

# Test 2: operate on a string with accented characters

# (a) Replace accented characters
newstring <- gsub("é", "!", string)
print(newstring)

# Test 3: handle data with accents within the R script
strings <- c("éléphant", "forêt", "lièvre")
values <- c(2, 3, 1)
df3 <- data.frame(Word = strings, Value = values)
print(df3)

df3_filtered <- filter(df3, Word %in% c("éléphant", "forêt"))
print(df3_filtered)

# Test 4: handle data with accents from CODR table

# get accented data from table used for indicator 5.2.1
raw_data <- get_cansim("35-10-0205-01", factors = FALSE)

df4 <-  raw_data %>%
  filter(Statistics == "Percentage",
         GEO == "Canada",
         Gender == "Women",
         Timeframe == "Experienced intimate partner violence in the past 12 months",
         `Type of intimate partner violence` == "Total, any type of intimate partner violence",
         ) %>%
  select(Year = REF_DATE,
         Geography = GEO,
         `Type of intimate partner violence`,
         `Population characteristics`,
         Value = VALUE,
  )

print(head(df4))

# Note: `Population characteristics` has "Métis" field
all_pop_characteristics <- unique(df4$`Population characteristics`)
print(all_pop_characteristics)

# (a) remove all population characteristics except Métis

all_fields_except_metis <- all_pop_characteristics[-11]
print(all_fields_except_metis)

df4a <- filter(df4, !(`Population characteristics` %in% all_fields_except_metis))
print(df4a)

# (b) filter by Métis
df4b <- filter(df4, `Population characteristics` == "Métis")
print(df4b)

# Test 5: try encoding data from CODR table first?
