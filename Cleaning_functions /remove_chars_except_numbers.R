#Removes characters from numeric columns. Example reposne 1 - Strongly Disagree. the function would keep the '1' but remove the ' - Strongly Disagree' 

library(dplyr)

remove_chars_except_numbers <- function(data, pattern = NULL) {
  result_data <- data
  
  if(is.null(pattern)){
    string_cols<- colnames(data)[sapply(data, is.character)]
  } else{
    string_cols<- colnames(data)[grep(pattern, colnames(data))]
  }
  
  for(col in string_cols){
    result_data[[col]]<- gsub("[^[:digit:]]","", data[[col]])
  }
  
  return(result_data)
}
