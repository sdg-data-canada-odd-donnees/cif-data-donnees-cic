library(yaml)
library(readtext)

# Get list of .md files from meta folder
md_files <- list.files("C:/Users/plamjer/Documents/cif-data-donnees-cic/meta/", #changer le lien selon la personne qui le fait. C'est local sur l'ordi.
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
    stringsAsFactors = FALSE
  )
}

# Apply to all files
results <- do.call(rbind, lapply(1:nrow(md_data), function(i) {
  cbind(filename = md_data$doc_id[i], extract_fields(md_data$text[i]))
}))

# Read the progress status YML file
progress_data <- yaml.load_file("C:/Users/plamjer/Documents/cif-data-donnees-cic/indicator_calculation_components.yml")

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

# Remove filename column and write to CSV
results_final <- results[, !names(results) %in% "filename"]
write.csv(results_final, "cif_metadata.csv", row.names = FALSE)