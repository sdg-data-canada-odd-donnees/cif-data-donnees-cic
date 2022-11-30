# DATA HUB AUTOMATION ####################################################
#
# Author: Maia Pelletier
# Script to automate the updating of the Open SDG data hub.
#
# TODO: Wrap all automation scripts in functions to be called if update is
# needed
# TODO: Handle a source changing for an indicator (i.e. no longer use automation
# code so that updates don't override new data from new source)
# TODO: MAYBE create a codeset with indicator:source correspondences (cleaner
# than extracting from code but likely will need to be manually updated)
# TODO: replace "source" with R package with functions
#
##########################################################################


source("automation_helper_functions.R")
library(stringr)

get_codr_table <- function(indicator) {
  
  file <- paste0("indicator_", indicator, ".R")
  code <- readLines(file.path("R", file))
  data_line <- code[stringr::str_detect(code, "get_cansim")]
  
  if (length(data_line) > 0) {
    
    table_no <- stringr::str_extract(data_line, regex("([0-9]+-)+[0-9]+"))
    
  } else {
    
    # TODO: throw error when no table detected?
    table_no <- NA
    
  }
  
  return(table_no)
  
}

read_hub_data <- function(indicator) {
  
  data_path <- file.path("data", paste0("indicator_", indicator, ".csv"))
  
  if (file.exists(data_path)) {
    data <- readr::read_csv(data_path, show_col_types = FALSE)
    return(data)
  } else {
    stop("Data does not exist")
  }
  
}


update_sdg_data <- function() {
  
  automation_scripts <- list.files(path = "R/", pattern = ".R")
  required_updates <- c()
  
  for (file in automation_scripts) {
    
    # get indicator number from R file name
    indicator <- stringr::str_extract(file, "[0-9]+-[0-9]+-[0-9]+")
    print(indicator)
    
    # get data that's currently available in data hub
    current_data <- read_hub_data(indicator)
    
    # get codr table(s) from automation file 
    codr_tbls <- get_codr_table(indicator)
    
    # check if new data is available for codr tables
    update_required <- check_data_update(data, codr_tbls)
    
    if (update_required == TRUE) {
      
      required_updates <- c(required_updates, indicator)
      # TODO: after this, updates need to be ran for any indicator listed
      # in the required updates vector
      # source(paste0("scripts/R/", indicator))
      
    }
    
  }
  
  print(required_updates)
  
}

update_sdg_data()
  
