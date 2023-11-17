#Gets the data frame into the format needed to run the max diff anaylsis. 


library(tidyr)
library(dplyr)
#Troubleshooting inputs

# raw_data<- MD_stacked_data
# MD_attr<- md_attributes
# num_questions = 8
# num_attrs = 5

stacked_data <- function(raw_data, MD_attr, num_questions, num_attrs) {
  
  MD_dat <- raw_data %>%
    select(-contains("anchors"))
  
  for(i in MD_attr){
    MD_dat[,i]<- NA
  }
  
  MD_stacked_data <- data.frame()
  
  for (row in 1:nrow(MD_dat)) {
    
    current_row <- MD_dat[row,] %>%
      pivot_longer(., cols = contains("MD"), names_to = "question", values_to = 'response')
    
    filled_data <- data.frame()
    
    for (q in 1:num_questions) {
      
      q_data <- current_row %>%
        filter(grepl(paste0('Q', q), question)) %>%
        pivot_wider(., names_from = 'question', values_from = response) %>%
        pivot_longer(., cols = contains('attr'), names_to = 'alt_number', values_to = "attributes_shown") %>%
        pivot_longer(., cols = contains(paste0("Q", q)), names_to = "set_q", values_to = "set_response") %>%
        separate(., col = attributes_shown, into = paste0("attr_", 1:num_attrs), sep = ";") %>%
        add_column(., 'set' = q)
      
      q_most <- q_data %>%
        filter(grepl('most', set_q)) %>%
        pivot_longer(., cols = contains("attr"), names_to = "alt_num", values_to = "alt_name")
      
      q_most_filled <- data.frame()
      
      for(i in 1:nrow(q_most)){
        
        q_most_current <- q_most[i,]
        
        q_most_current_col <- match(q_most_current$alt_name, colnames(q_most_current))
        
        q_most_current[,q_most_current_col] <- 1
        
        if(q_most_current$alt_name %in% q_most_current$set_response){
          q_most_current$choice <- 1
        } else(q_most_current$choice <- NA)
        
        q_most_filled <- rbind.data.frame(q_most_filled, q_most_current)
      }
      
      q_least <- q_data %>%
        filter(grepl('least', set_q)) %>%
        pivot_longer(., cols = contains("attr"), names_to = "alt_num", values_to = "alt_name")
      
      q_least_filled <- data.frame()
      
      for(i2 in 1:nrow(q_least)){
        
        q_least_current <- q_least[i2,]
        
        q_least_current_col <- match(q_least_current$alt_name, colnames(q_least_current))
        
        q_least_current[,q_least_current_col] <- -1
        
        if(q_least_current$alt_name %in% q_least_current$set_response){
          q_least_current$choice <- 1
        } else(q_least_current$choice <- NA)
        
        q_least_filled <- rbind.data.frame(q_least_filled, q_least_current)
      }
      
      q_filled <- rbind.data.frame(q_most_filled, q_least_filled) %>%
        mutate_at(vars(intersect(colnames(.), MD_attr$value), choice), as.numeric) %>%
        mutate_at(vars(intersect(colnames(.), MD_attr$value), choice), ~replace_na(.,0))
      
      filled_data <- rbind.data.frame(filled_data, q_filled)
      
    }
    
    MD_stacked_data <- rbind.data.frame(MD_stacked_data, filled_data)
    
  }
  
  ##Applying weights based on anchors###
  anchors<- raw_data %>%
    select(SID_all, contains('anchors')) %>%
    `colnames<-`(c("SID_all","anchors")) %>%
    filter(SID_all %in% MD_stacked_data$SID_all)
  
  # Merging MD_stacked_data and anchors dataframes by SID and creating weights column 
  MD_stacked_anchors <- MD_stacked_data %>%
    merge.data.frame(., anchors, by = "SID_all") %>%
    mutate(weights = 1)
  
  
  
  MD_stacked_data_final<- data.frame()
  for(r in unique(MD_stacked_anchors$SID_all)){
    
    # Function to remove single word entries
    remove_single_word <- function(column) {
      if(is.character(column)) {
        sapply(column, function(x) ifelse(length(strsplit(str_trim(x), " ")[[1]]) > 1, x, NA))
      } else {
        column
      }
    }
    
    current_participant <- MD_stacked_anchors %>%
      filter(SID_all %in% r)  %>%
      mutate_at(vars(anchors), as.character) %>%
      separate(anchors, into = paste0("anchor_", 1:50), sep = ",", fill = "right") %>%
      mutate(across(starts_with("anchor"), remove_single_word)) %>%
      select_if(~!any(is.na(.))) 
    
    # Get names of columns that start with 'anchor'
    anchor_cols <- grep("^anchor", names(current_participant), value = TRUE)
    
    #creating how much a weight should be multiplied 
    weight_multiplier <- length(anchor_cols)
    # Remove leading and trailing white spaces from 'anchor' columns
    current_participant[anchor_cols] <- lapply(current_participant[anchor_cols], trimws)
    
    # Now let's proceed with the creation of 'anchor_match' column
    
    # Create a function to check if 'set_response' starts with any values in 'anchor' columns
    check_start <- function(string, anchor_cols) {
      any(sapply(anchor_cols, function(x) startsWith(string, x)))
    }
    
    # Create a new column 'anchor_match' 
    current_participant_final <- current_participant %>%
      rowwise() %>%
      mutate(anchor_match = check_start(set_response, c_across(starts_with("anchor")))) %>%
      ungroup() %>%
      mutate(weights = ifelse(anchor_match & choice == 1, weights + (1 / weight_multiplier), weights)) %>%
      select(-starts_with('anchor_'), anchor_match)
    
    MD_stacked_data_final<- rbind.data.frame(MD_stacked_data_final, current_participant_final)
    
  }
  
  return(MD_stacked_data_final)
}

