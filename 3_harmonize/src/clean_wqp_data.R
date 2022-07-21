#' @title Clean WQP data
#' 
#' @description 
#' Function to harmonize WQP data in preparation for further analysis. Included
#' in this function are steps to unite diverse characteristic names by assigning
#' them to more commonly-used water quality parameter names. 
#' 
#' @param wqp_data data frame containing the data downloaded from the WQP, 
#' where each row represents a unique data record.
#' @param wqp_params list object where each element of the list is a named vector.
#' The vectors correspond with a parameter group and contain character string(s)
#' representing known WQP characteristic names associated with each parameter. 
#' 
#' @returns 
#' Returns a formatted and harmonized data frame containing data downloaded from 
#' the Water Quality Portal, where each row represents a unique data record.
#' 
clean_wqp_data <- function(wqp_data, wqp_params){
  
  # Before we clean any data, format a table that indicates how various 
  # characteristic names map onto more commonly-used parameter names
  params_df <- lapply(names(wqp_params), function(x){
    params <- data.frame(char_name = wqp_params[[x]],
                         parameter = x) 
  }) %>%
    bind_rows()
  
  # Clean data and assign flags if applicable
  wqp_data_clean <- wqp_data %>%
    # harmonize characteristic names by assigning a common parameter name
    # to the groups of characteristics supplied in `wqp_params`.
    left_join(y = params_df, by = c("CharacteristicName" = "char_name"))
  
  return(wqp_data_clean)
  
  
}

