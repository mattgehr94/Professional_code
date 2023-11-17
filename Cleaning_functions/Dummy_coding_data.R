#This tells which columns needs to be converted into Dummy codes and gives you a separate output that shows what the dummy codes equal. Like if Male =1 and Female =2 the 'coding_info_name' object shows how each level was dummy coded.

dummy_code_strings_columns <- function(data, patterns, coding_info_name = "coding_info") {
  result_data <- data
  
  # Identify columns based on patterns
  selected_cols <- colnames(data)[sapply(data, is.character) & grepl(patterns, colnames(data))]
  
  # Group the columns based on the pattern
  pattern_groups <- str_extract(selected_cols, patterns)
  pattern_groups[is.na(pattern_groups)] <- "NA"
  
  # Create separate coding_info for each pattern group
  coding_info <- list()
  for (pg in unique(pattern_groups)) {
    # Identify columns in this pattern group
    pg_cols <- selected_cols[pattern_groups == pg]
    # Create a global coding_info data frame for this pattern group
    all_unique_values <- unique(unlist(lapply(data[pg_cols], unique)))
    all_unique_values <- all_unique_values[!is.na(all_unique_values)]
    dummy_values <- 1:length(all_unique_values)
    coding_info[[pg]] <- data.frame(Orig_value = all_unique_values, Dummy_value = dummy_values, stringsAsFactors = FALSE)
    # Apply coding info to data for this pattern group
    for (col in pg_cols) {
      result_data[[col]] <- sapply(data[[col]], function(x) {
        if (is.na(x)) {
          return(NA)
        } else {
          return(coding_info[[pg]]$Dummy_value[coding_info[[pg]]$Orig_value == x])
        }
      })
    }
  }
  
  result <- list(dummy_coded_data = result_data)
  result[[coding_info_name]] <- coding_info
  return(result)
}

