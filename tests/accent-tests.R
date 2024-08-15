# Test script
# Testing the behaviour of accented characters handled by R scripts

suppressPackageStartupMessages(library(dplyr))

# Test 1: print an accented character
string <- "Éléphant"
print(string)

# Test 2: operate on a string with accented characters

# (a) Replace accented characters
newstring <- gsub("é", "!", string)
print(newstring)

# Test 3: 
strings <- c("éléphant", "forêt", "lièvre")
values <- c(2, 3, 1)
df <- data.frame(Word = strings, Value = values)
print(df)

df_filtered <- filter(df, Word %in% c("éléphant", "forêt"))
print(df_filtered)
