# DATA HUB AUTOMATION ####################################################
#
# Author: Maia Pelletier
# Script to automate the updating of the Open SDG data hub.
#
# TODO: return None if no updates required
##########################################################################


source("automation_helper_functions.R")
library(stringr)

get_data_table <- function(indicator) {

  file <- paste0("indicator_", indicator, ".R")
  code <- readLines(file.path("R", file))
}

# get_codr_table <- function(indicator) {
#   
#   file <- paste0("indicator_", indicator, ".R")
#   code <- readLines(file.path("R", file))
#   data_line <- code[stringr::str_detect(code, "get_cansim")]
#   
#   if (length(data_line) > 0) {
#     
#     table_no <- stringr::str_extract(data_line, regex("([0-9]+-)+[0-9]+"))
#     
#   } else {
#     
#     stop("No CODR table detected")
#     
#   }
#   
#   return(table_no)
#   
# }

read_hub_data <- function(indicator) {
  
  data_path <- file.path("data", paste0("indicator_", indicator, ".csv"))
  
  if (file.exists(data_path)) {
    
    data <- read.csv(data_path)
    return(data)
    
  } else {
    
    stop("Data does not exist")
    
  }
  
}


# update_sdg_data <- function() {
#   
#   automation_scripts <- list.files(path = "R/", pattern = ".R")
#   required_updates <- c()
#   
#   for (file in automation_scripts) {
#     
#     # file <- automation_scripts[1]
#     
#     # get indicator number from R file name
#     indicator <- stringr::str_extract(file, "[0-9]+-[0-9]+-[0-9]+")
#     print(indicator)
#     
#     # get data that's currently available in data hub
#     current_data <- read_hub_data(indicator)
#     
#     # get codr table(s) from automation file 
#     codr_tbls <- get_codr_table(indicator)
#     
#     # check if new data is available for codr tables
#     update_required <- check_data_update(current_data, codr_tbls)
#     
#     if (update_required == TRUE) {
#       
#       required_updates <- c(required_updates, indicator)
#       
#     }
#     
#   }
#   
#   if (length(required_updates > 0)) {
#     
#     print(paste0("data to be updated:",
#                  paste0(required_updates, collapse = ", ")))
#     
#     for (indicator in required_updates) {
#       
#       source(file.path("R", paste0("indicator_", indicator, ".R")))
#       print(paste0("indicator ", indicator, " has been updated"))
#       
#     }
#     
#     # quit(status = 0)
#     
#   }
#   
# }

update_sdg_data <- function() {
  
  automation_scripts <- list.files(path = "R/", pattern = ".R")
  # temporary manual override: don't update these indicators due to bugs when run with GitHub Actions
  bad_indicators <- c("indicator_4-1-1.R",
                      "indicator_5-2-1.R",
                      "indicator_11-2-1.R",
                      "indicator_11-4-1.R",
                      "indicator_16-1-1.R")
  automation_scripts <- automation_scripts[! automation_scripts %in% bad_indicators]
  required_updates <- c()
  
  for (file in automation_scripts) {
    
    # file <- automation_scripts[1]
    
    # get indicator number from R file name
    indicator <- stringr::str_extract(file, "[0-9]+-[0-9]+-[0-9]+")
    print(indicator)
    
    # get data that's currently available in data hub
    current_data <- read_hub_data(indicator)
    
    # get codr table(s) from automation file 
    # codr_tbls <- get_codr_table(indicator)
    
    # check if new data is available for codr table
      required_updates <- c(required_updates, indicator)  

  }
  
  if (length(required_updates > 0)) {
    
    print(paste0("data to be updated:",
                 paste0(required_updates, collapse = ", ")))
    
    for (indicator in required_updates) {
      
      source(file.path("R", paste0("indicator_", indicator, ".R")))
      print(paste0("indicator ", indicator, " has been updated"))
      
    }
    
    # quit(status = 0)
    
  }
  
}

update_sdg_data()



