#The amount of time someone takes to complete the survey 


# Load lubridate library
library(lubridate)

# Define function to calculate time taken
calculate_time_taken <- function(data, start_col, end_col, format = "%m/%d/%y %I:%M:%S %p") {
  # Convert date_started and date_finished to POSIXct format
  data$date_started <- as.POSIXct(data$date_started, format = "%m/%d/%y %I:%M:%S %p", tz = "UTC")
  data$date_finished <- as.POSIXct(data$date_finished, format = "%m/%d/%y %I:%M:%S %p", tz = "UTC")
  
  # Calculate time taken in seconds
  data$time_taken_seconds <- as.numeric(difftime(data$date_finished, data$date_started, units = "secs"))
  
  return(data)
}
