#Function for BigQuery that would give basic information about the columns that were pulled. 

bq_column_stats <- function(project_id, dataset_id, table_id, data_type_summary, n = 10000) {
  
  # Fetch the raw data
  raw_data <- bq_basic_query(project_id, dataset_id, table_id, n = n)
  
  # Get column info from BigQuery
  column_info <- bq_table_column_information(project_id, dataset_id, table_id)
  
  # Translating R's datatype to BigQuery's datatype
  if(data_type_summary == "NUMERIC") {
    bigquery_data_type <- c("NUMERIC", "FLOAT64", "INT64")
  } else if(data_type_summary == "STRING") {
    bigquery_data_type <- c("STRING")
  } else if(data_type_summary == "TIMESTAMP") {
    bigquery_data_type <- c("TIMESTAMP")
  }
  
  # Filter column info by the specified data type
  filtered_columns <- column_info$column_name[column_info$data_type %in% bigquery_data_type]
  
  # Initialize a list to store the results
  stats_list <- list()
  
  # Loop over each column in the filtered columns
  for (column_name in filtered_columns) {
    # Get the column data
    column_data <- raw_data[[column_name]]
    
    # Compute stats based on data type
    if (data_type_summary == "STRING") {
      # String column: calculate counts of unique values
      column_stats <- table(column_data)
      # Convert to data frame
      column_stats <- data.frame(
        value = names(column_stats),
        count = as.integer(column_stats)
      )
      # Add column name to the data frame
      column_stats$column_name <- column_name
    } else if (data_type_summary == "NUMERIC") {
      # Numeric column: calculate mean, sd, min, max, median
      column_stats <- data.frame(
        column_name = column_name,
        mean = mean(column_data, na.rm = TRUE),
        sd = sd(column_data, na.rm = TRUE),
        min = min(column_data, na.rm = TRUE),
        max = max(column_data, na.rm = TRUE),
        median = median(column_data, na.rm = TRUE)
      )
    } else if (data_type_summary == "TIMESTAMP") {
      # Timestamp column: calculate min, max, and average interval
      column_data <- as.POSIXct(column_data)  # Convert to POSIXct type for date calculations
      sorted_timestamps <- sort(column_data, na.last = NA)
      intervals <- difftime(sorted_timestamps[-1], sorted_timestamps[-length(sorted_timestamps)], units = "mins")  # Compute intervals in minutes
      column_stats <- data.frame(
        column_name = column_name,
        min = min(column_data, na.rm = TRUE),
        max = max(column_data, na.rm = TRUE),
        avg_interval = mean(intervals, na.rm = TRUE)
      )
    }
    # Append the results to the stats list
    stats_list[[column_name]] <- column_stats
  }
  
  # Return the results
  return(stats_list)
}