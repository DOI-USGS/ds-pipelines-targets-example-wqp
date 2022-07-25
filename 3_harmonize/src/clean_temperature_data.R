#' @title Clean temperature data
#' 
#' @description 
#' Function to clean temperature data, including harmonizing diverse units. 
#' 
#' @param wqp_data data frame containing the data downloaded from the WQP, 
#' where each row represents a data record.
#' @param char_names_crosswalk data frame containing columns "char_name" and 
#' "parameter". The column "char_name" contains character strings representing 
#' known WQP characteristic names associated with each parameter.
#' @param temp_param_name character string indicating which string in the 
#' "parameter" column of `char_names_crosswalk` corresponds with temperature
#' data. 
#' 
#' @returns 
#' Returns a formatted and harmonized data frame containing data downloaded from 
#' the Water Quality Portal, where each row represents a data record. Temperature
#' units have been standardized to degrees celsius where possible. 
#' 
clean_temperature_data <- function(wqp_data, char_names_crosswalk, temp_param_name){
  
  # Grab characteristic names associated with temperature in `char_names_crosswalk`
  temp_char_names <- char_names_crosswalk %>%
    filter(parameter == temp_param_name) %>%
    pull(char_name)
  
  # Clean temperature data
  wqp_data_out <- wqp_data %>%
    # harmonize units
    mutate(ResultMeasureValue = if_else(CharacteristicName %in% temp_char_names & 
                                          !is.na(ResultMeasureValue) & 
                                          ResultMeasure.MeasureUnitCode == "deg F",
                                        ((ResultMeasureValue - 32) * (5/9)), ResultMeasureValue),
           ResultMeasure.MeasureUnitCode = if_else(CharacteristicName %in% temp_char_names &
                                                     !is.na(ResultMeasureValue) & 
                                                     ResultMeasure.MeasureUnitCode == "deg F",
                                                   "deg C", ResultMeasure.MeasureUnitCode))
  
  return(wqp_data_out)
  
}


                                    