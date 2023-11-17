#Specific for internet provider questions but just matches them from the first time the respondent was asked about their internet service provider and compares it to the next time they were asked ot see if their answer has changed. 

ISP_provider_check <- function(data, col1, col2) {
  check_final <- data.frame()
  
  for (i in 1:nrow(data)) {
    current_row <- data[i, ]
    
    if (current_row[[col1]] %in% current_row[[col2]]) {
      current_row$ISP_provider_check <- "Pass"
    }
    if (current_row[[col1]] != current_row[[col2]]) {
      current_row$ISP_provider_check <- "Fail"
    }
    
    # Add your custom pass conditions here
    if(current_row[[col1]] %in% "Cox" & current_row[[col2]] %in% "Cox Fiber"){
      current_row$ISP_provider_check <- "Pass"
    }
    
    if(current_row[[col1]] %in% "Cox Fiber" & current_row[[col2]] %in% "Cox"){
      current_row$ISP_provider_check <- "Pass"
    }
    
    if(current_row[[col1]] %in% "AT&T" & current_row[[col2]] %in% "AT&T Fiber"){
      current_row$ISP_provider_check <- "Pass"
    }
    
    if(current_row[[col1]] %in% "AT&T Fiber" & current_row[[col2]] %in% "AT&T"){
      current_row$ISP_provider_check <- "Pass"
    }
    
    if(current_row[[col1]] %in% "Xfinity" & current_row[[col2]] %in% "Xfinity Fiber"){
      current_row$ISP_provider_check <- "Pass"
    }
    
    if(current_row[[col1]] %in% "Xfinity Fiber" & current_row[[col2]] %in% "Xfinity"){
      current_row$ISP_provider_check <- "Pass"
    }
    
    if(current_row[[col1]] %in% "Comcast" & current_row[[col2]] %in% "Xfinity Fiber"){
      current_row$ISP_provider_check <- "Pass"
    }
    
    if(current_row[[col1]] %in% "Xfinity Fiber" & current_row[[col2]] %in% "Comcast"){
      current_row$ISP_provider_check <- "Pass"
    }
    if(current_row[[col1]] %in% "Comcast" & current_row[[col2]] %in% "Xfinity"){
      current_row$ISP_provider_check <- "Pass"
    }
    
    if(current_row[[col1]] %in% "Xfinity" & current_row[[col2]] %in% "Comcast"){
      current_row$ISP_provider_check <- "Pass"
    }
    if(current_row[[col1]] %in% "CenturyLink" & current_row[[col2]] %in% "CenturyLink Fiber"){
      current_row$ISP_provider_check <- "Pass"
    }
    
    if(current_row[[col1]] %in% "CenturyLink Fiber" & current_row[[col2]] %in% "CenturyLink"){
      current_row$ISP_provider_check <- "Pass"
    }
    
    if(current_row[[col1]] %in% "Spectrum" & current_row[[col2]] %in% "Spectrum Fiber"){
      current_row$ISP_provider_check <- "Pass"
    }
    
    if(current_row[[col1]] %in% "Spectrum Fiber" & current_row[[col2]] %in% "Spectrum"){
      current_row$ISP_provider_check <- "Pass"
    }
    
    if(current_row[[col1]] %in% "Verizon" & current_row[[col2]] %in% "Verizon Fios"){
      current_row$ISP_provider_check <- "Pass"
    }
    
    if(current_row[[col1]] %in% "Verizon Fios" & current_row[[col2]] %in% "Verizon"){
      current_row$ISP_provider_check <- "Pass"
    }
    
    # Add more conditions as needed
    
    check_final <- rbind.data.frame(check_final, current_row)
  }
  
  return(check_final)
}

# Example usage
#internet_check_final <- ISP_provider_check(raw_data, "ISP_provider_1", "ISP_provider_2")
