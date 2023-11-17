#Function to pull data from the BigQuery. Just easier than the SQL syntax

bq_basic_query <- function(project_id, dataset_id, table_id=NULL, columns=NULL, n=NULL) {
  
  # If columns are provided, turn them into a comma-separated string
  # If not, select all columns with *
  select_clause <- if (is.null(columns)) "*" else paste(columns, collapse = ", ")
  
  # Base of the SQL query
  base_query <- paste0("SELECT ", select_clause, " FROM `")
  
  # If table_id is provided, select only from this table
  if (!is.null(table_id)) {
    sql_query <- paste0(
      base_query,
      project_id, ".", dataset_id, ".", table_id,
      "`"
    )
  } else {
    # If table_id is not provided, select from all tables in the dataset
    sql_query <- paste0(
      base_query,
      project_id, ".", dataset_id,
      ".*`"
    )
  }
  
  # If n is provided, add a LIMIT clause
  if (!is.null(n)) {
    sql_query <- paste0(sql_query, " LIMIT ", n)
  }
  
  # Use your previous function to run the query and get the results
  query_results <- bq_query_function(project_id, sql_query)
  
  # Return the results
  return(query_results)
}