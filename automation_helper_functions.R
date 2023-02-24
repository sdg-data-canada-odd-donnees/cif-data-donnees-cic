# Automation helper functions ############################################
# Author: Maia Pelletier
#
# Script contains functions to use in automation script for CIF/GIF.
#
#########################################################################

# load libraries
library(lubridate)
library(cansim)

geocodes <- data.frame(
  GeoCode = c('10', '11', '12', '13', '24', '35', '46', '47', '48', '59', '60', '61', '62'),
  Geography = c('Newfoundland and Labrador', 'Prince Edward Island', 'Nova Scotia', 'New Brunswick', 'Quebec', 'Ontario', 'Manitoba', 'Saskatchewan', 'Alberta', 'British Columbia', 'Yukon', 'Northwest Territories', 'Nunavut')
)

indicator_file <- function(indicator, ext = c("R", "csv")) {
  
  paste0("indicator_", indicator, ext)
  
}


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
  
  # Most recent year available in data hub
  data_max_year <- max(data$Year)
  
  if (length(table_no) == 1) {
    
    # Most recent year available in CODR table
    tbl_max_ref_date <- get_current_ref_date(table_no)
    
    
  } else if (length(table_no) > 1) {
    
    tbl_max_ref_date <- c()
    
    for (tbl in table_no) {
      
      tbl_max_ref_date <- c(tbl_max_ref_date, get_current_ref_date(tbl))
      
    }
    
  }
  
  # Return TRUE if data update is needed
  return(ifelse(all(data_max_year < tbl_max_ref_date), TRUE, FALSE))
  
}

