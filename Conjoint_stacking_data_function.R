#This function gets data outputed from Survey King into an appropriate format to be run through the conjoint function. 

conjoint_data_format <- function(data, attrs) {
  # Create a new dataframe to store the modified data
  new_data <- data.frame()
  
  # Get unique combinations of respondent and set
  respondent_set <- unique(data[, c("Respondent", "Set..")])
  
  # Loop through each unique combination of respondent and set
  for (i in 1:nrow(respondent_set)) {
    # Get the rows corresponding to the current respondent and set combination
    current_data <- subset(data, Respondent == respondent_set[i, 1] & Set.. == respondent_set[i, 2])
    
    total_cards <- max(current_data$Card..)
    
    if (sum(current_data$Choice) > total_cards) {
      current_data$Choice_new <- 0
      new_row <- current_data[1, ]
      new_row[, c("Card..", attrs)] <- 0
      new_row$Choice_new <- 1
      current_data <- rbind(current_data, new_row)
    } else {
      current_data$Choice_new <- 0
      index <- current_data$Choice[current_data$Choice > 0]
      current_data$Choice_new[index] <- 1
    }
    
    # Add the modified data to the new dataframe
    new_data <- rbind(new_data, current_data)
  }
  
  # Return the modified data
  return(new_data)
}
