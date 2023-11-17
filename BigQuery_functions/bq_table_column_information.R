#If there is an Information_Schema this function shows that information just in a faster fashion. 

bq_table_column_information <- function(project_id, dataset_id, table_id=NULL) {
  
  # Base of the SQL query
  base_query <- paste0(
    "SELECT column_name, data_type
     FROM `", project_id, ".", dataset_id, ".INFORMATION_SCHEMA.COLUMNS`"
  )
  
  # If table_id is provided, add a WHERE clause
  if (!is.null(table_id)) {
    sql_query <- paste0(
      base_query,
      " WHERE table_name='", table_id, "'"
    )
  } else {
    sql_query <- base_query
  }
  
  # Use the bq_query_function to run the query and get the results
  query_results <- bq_query_function(project_id, sql_query)
  
  # Return the results
  return(query_results)
}
