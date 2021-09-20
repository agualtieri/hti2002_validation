## possible if statement - similar to Yann - not working
check_log <- function(data, log, variable = "question.name", old_log_var = "old.value", new_log_var = "new.value", uuid_data = "uuid", uuid_log = "uuid") {
   
  # Check that the inputs are correct  
  if(!all(log[[uuid_log]] %in% data[[uuid_data]])) message("Some uuids in the cleaning log are not present in the dataset.") 
  
  if(!all(log[[variable]] %in% names(data))) message("Some question names in the cleaning log are not present in the dataset.")
  

  # Turn NAs into characters
  #log[[old_log_var]] <- stringr::str_replace_na(log[[old_log_var]])
  #log[[new_log_var]] <- stringr::str_replace_na(log[[new_log_var]])

  # Extract cleaning log
  extract_value <- function(data, 
                            log,
                            variable,
                            uuid_data,
                            uuid_log) {
    # Indexation
    row.i<- which(data[[uuid_data]] == log[[uuid_log]])
    # Extraction
    value.c <- data[row.i, log[[variable]]]
    value.r <- data.frame(value.c, 
                          # unique id to join later
                          binding = paste0(log[[uuid_log]], log[[variable]]))
    names(value.r) <- c("value_extracted", "binding")
    return(value.r)
    
  }
  
    log.check <- mapply(extract_value, 
                        log = split(log, row.names(log)), 
                        variable = variable, 
                        MoreArgs = list(
                        data = data,
                        uuid_data = uuid_data,
                        uuid_log = uuid_log
                        ),
                           SIMPLIFY = F) %>% do.call(rbind, .)
    
    
   # Interim output - because how knows why...
   final <- log %>% 
      mutate(binding = paste0(log[[uuid_log]], log[[variable]])) %>%
      left_join(log.check) %>% 
      select(c(uuid_log, variable, old_log_var, new_log_var, value_extracted))
   

    
    return(final)
    

}




















