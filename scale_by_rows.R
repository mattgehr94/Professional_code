#Z-scoring by row instead of column

scale_rows<- function(data, cols_to_scale) {
  
  matrix_vars_indv_scaled <- data %>%
    select(., contains("SID"), all_of(cols_to_scale)) %>%
    mutate_at(vars(-contains("SID")), .funs = as.numeric) %>%
    drop_na() %>%
    column_to_rownames(var = names(.)[1]) %>%
    t() %>%
    as.data.frame() %>%
    mutate_all(., .funs = scale) %>%
    t() %>%
    as.data.frame() %>%
    rownames_to_column(var = "SID_all") %>%
    mutate_at(vars(SID_all), as.numeric)
  
  #SID column
  SID_column<- matrix_vars_indv_scaled %>%
    select(contains('SID')) %>%
    names()
  
  #merging the scaled columns back with original data 
  data_scaled<- data %>%
    select(-all_of(cols_to_scale)) %>%
    left_join(matrix_vars_indv_scaled, by = SID_column )
  
}
