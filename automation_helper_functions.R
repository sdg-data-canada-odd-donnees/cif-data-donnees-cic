
library(lubridate)
library(cansim)

# opensdg_col_rename <- function(col_names) {
#   
# 
#   
# }
# 
# test_colnames <- c("Geography", "Sex", "GeoCode", "Value")
# ignore <- c("Geography", "Units", "GeoCode", "Value")
# 
# ignore[ignore == test_colnames]

geocodes <- data.frame(
  GeoCode = c('10', '11', '12', '13', '24', '35', '46', '47', '48', '59', '60', '61', '62'),
  Geography = c('Newfoundland and Labrador', 'Prince Edward Island', 'Nova Scotia', 'New Brunswick', 'Quebec', 'Ontario', 'Manitoba', 'Saskatchewan', 'Alberta', 'British Columbia', 'Yukon', 'Northwest Territories', 'Nunavut')
)

get_current_ref_date <- function(table_no) {
  
  # Retrieve CODR table metadata
  tbl_md <- cansim::get_cansim_cube_metadata(table_no)
  
  # Get the table end date
  current_date <- tbl_md$cubeEndDate
  
  # Extract the year from the end date
  current_year <- lubridate::year(current_date)
  
  return(current_year)
  
}

check_data_update <- function(data, table_no) {
  
  # Most recent year available in CODR table
  tbl_max_ref_date <- get_current_ref_date(table_no)
  
  # Most recent year available in data hub
  data_max_year <- max(data$Year)
  
  # Return TRUE if data update is needed
  return(ifelse(data_max_year < tbl_max_ref_date, TRUE, FALSE))
  
}

automation_scripts <- list.files(path = "R/", pattern = ".R")


source_codr_tables <- function() {
  
  # TODO: populate this with code that returns the list of all source tables from update codes
  
  
  
}