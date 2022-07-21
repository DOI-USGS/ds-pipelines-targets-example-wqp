#' @title Clean WQP data
#' 
#' @description 
#' Function to harmonize WQP data in preparation for further analysis. Included
#' in this function are steps to unite diverse characteristic names by assigning
#' them to more commonly-used water quality parameter names. 
#' 
#' @param char_names_crosswalk data frame containing columns "char_name" and 
#' "parameter". The column "char_name" contains character strings representing 
#' known WQP characteristic names associated with each parameter.
#' @param wqp_params list object where each element of the list is a named vector.
#' The vectors correspond with a parameter group and contain character string(s)
#' representing known WQP characteristic names associated with each parameter. 
#' 
#' @returns 
#' Returns a formatted and harmonized data frame containing data downloaded from 
#' the Water Quality Portal, where each row represents a unique data record.
#' 
clean_wqp_data <- function(wqp_data, char_names_crosswalk){

  # Clean data and assign flags if applicable
  wqp_data_clean <- wqp_data %>%
    # harmonize characteristic names by assigning a common parameter name
    # to the groups of characteristics supplied in `char_names_crosswalk`.
    left_join(y = char_names_crosswalk, by = c("CharacteristicName" = "char_name"))
  
  return(wqp_data_clean)
  
  
}

