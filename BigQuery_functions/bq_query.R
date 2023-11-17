#Most basic version of pulling data from BigQuery where direct SQL syntax can be used

bq_query_function <- function(project_id, sql_query) {
  # Create a query job
  query_job <- bigrquery::bq_project_query(project_id, sql_query)
  
  # Download the query results into a data frame
  query_results <- bigrquery::bq_table_download(query_job, page_size = NULL)
  
  # Return the results
  return(query_results)
}