
library(yaml)
library(readtext)
library(dplyr)

# Get list of .md files from meta folder
md_files <- list.files("C:/Users/plamjer/Documents/cif-data-donnees-cic/meta/fr", #changer le lien selon la personne qui le fait. C'est local sur l'ordi.
                       pattern = "\\.md$", 
                       full.names = TRUE)

# Read all files
md_data <- readtext(md_files)

# Function to extract fields safely
extract_fields <- function(text) {
  yaml_data <- tryCatch(yaml.load(text), error = function(e) list())
  
  data.frame(
    sdg_goal = if(is.null(yaml_data$sdg_goal)) NA_character_ else as.character(yaml_data$sdg_goal),
    indicator_number = if(is.null(yaml_data$indicator_number)) NA_character_ else as.character(yaml_data$indicator_number),
    indicator_name = if(is.null(yaml_data$indicator_name)) NA_character_ else as.character(yaml_data$indicator_name),
    target_name = if(is.null(yaml_data$target_name)) NA_character_ else as.character(yaml_data$target_name),
    computation_units = if(is.null(yaml_data$computation_units)) NA_character_ else as.character(yaml_data$computation_units),
    stringsAsFactors = FALSE
  )
}

# Apply to all files
results <- do.call(rbind, lapply(1:nrow(md_data), function(i) {
  cbind(filename = md_data$doc_id[i], extract_fields(md_data$text[i]))
}))

# Function to clean indicator_name field
clean_indicator_name <- function(name) {
  if(is.na(name)) return(NA_character_)
  
  # Check if it starts with "list("
  if(grepl("^list\\(", name)) {
    # Extract text after 'title = ""' and before '"")'
    if(grepl('title = ""', name)) {
      # Split by 'title = ""' 
      parts <- strsplit(name, 'title = ""', fixed = TRUE)[[1]]
      if(length(parts) > 1) {
        # Take everything after 'title = ""' and remove the trailing '"")'
        title_part <- parts[2]
        title_part <- gsub('""\\)$', '', title_part)
        return(title_part)
      }
    }
  }
  # Return as-is if not a list format
  return(name)
}

# Clean the indicator_name column
results$indicator_name <- sapply(results$indicator_name, clean_indicator_name)

# Read the progress status YML file
progress_data <- yaml.load_file("C:/Users/plamjer/Documents/cif-data-donnees-cic/indicator_calculation_components.yml")

# Add current_year column
results$current_year <- sapply(results$indicator_number, function(ind_num) {
  if(is.na(ind_num)) return(NA_real_)
  
  # Convert "1.1.1" to "1-1-1" format
  key <- gsub("\\.", "-", ind_num)
  
  # Access nested structure: progress_data[[key]][[key]]$current_year
  if(!is.null(progress_data[[key]]) && !is.null(progress_data[[key]][[key]]) && 
     !is.null(progress_data[[key]][[key]]$current_year)) {
    return(progress_data[[key]][[key]]$current_year)
  } else {
    return(NA_real_)
  }
})

# Add current_value column
results$current_value <- sapply(results$indicator_number, function(ind_num) {
  if(is.na(ind_num)) return(NA_real_)
  
  # Convert "1.1.1" to "1-1-1" format
  key <- gsub("\\.", "-", ind_num)
  
  # Access nested structure: progress_data[[key]][[key]]$current_value
  if(!is.null(progress_data[[key]]) && !is.null(progress_data[[key]][[key]]) && 
     !is.null(progress_data[[key]][[key]]$current_value)) {
    return(progress_data[[key]][[key]]$current_value)
  } else {
    return(NA_real_)
  }
})

# Add progress_status column
results$progress_status <- sapply(results$indicator_number, function(ind_num) {
  if(is.na(ind_num)) return(NA_character_)
  
  # Convert "1.1.1" to "1-1-1" format
  key <- gsub("\\.", "-", ind_num)
  
  if(!is.null(progress_data[[key]]) && !is.null(progress_data[[key]]$progress_status)) {
    return(as.character(progress_data[[key]]$progress_status))
  } else {
    return(NA_character_)
  }
})

# renommer les mesures de progrès

results$progress_status <-  recode(results$progress_status,
                             "deterioration" = "Détérioration",
                             "moderate_progress" = "Des progrès ont été réalisés, mais une accélération est nécessaire",
                             "target_achieved" = "Cible atteinte",
                             "limited_progress" = "Progrès limités",
                             "negligible_progress" = "Progrès limités",
                             "not_available_manual" = "Non disponible",
                             "substantial_progress" = "Sur la bonne voie"
                            )

# Reorder columns to put computation_units after current_value
results <- results[, c("filename", "sdg_goal", "indicator_number", "indicator_name", 
                       "target_name", "current_year", "current_value", 
                       "computation_units", "progress_status")]

# Sort by indicator_number numerically
# Split indicator_number into parts and sort by numeric values
results <- results[order(
  as.numeric(sapply(strsplit(results$indicator_number, "\\."), `[`, 1)),
  as.numeric(sapply(strsplit(results$indicator_number, "\\."), `[`, 2)),
  as.numeric(sapply(strsplit(results$indicator_number, "\\."), `[`, 3))
), ]

# Remove filename column and write to CSV
results_final <- results[, !names(results) %in% "filename"]
write.csv(results_final, "cif_metadata_fr.csv", row.names = FALSE)