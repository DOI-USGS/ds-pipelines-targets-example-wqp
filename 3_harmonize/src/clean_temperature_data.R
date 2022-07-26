#' @title Clean temperature data
#' 
#' @description 
#' Function to clean temperature data, including harmonizing diverse units. 
#' 
#' @param wqp_data data frame containing the data downloaded from the WQP, 
#' where each row represents a data record.
#' 
#' @returns 
#' Returns a formatted and harmonized data frame containing data downloaded from 
#' the Water Quality Portal, where each row represents a data record. Temperature
#' units have been standardized to degrees celsius where possible. 
#' 
clean_temperature_data <- function(wqp_data){

  # Clean temperature data
  wqp_data_out <- wqp_data %>%
    # harmonize units
    mutate(ResultMeasureValue = if_else(!is.na(ResultMeasureValue) & 
                                          grepl("deg F", ResultMeasure.MeasureUnitCode, ignore.case = TRUE),
                                        ((ResultMeasureValue - 32) * (5/9)), ResultMeasureValue),
           ResultMeasure.MeasureUnitCode = if_else(!is.na(ResultMeasureValue) & 
                                                     grepl("deg F", ResultMeasure.MeasureUnitCode, ignore.case = TRUE),
                                                   "deg C", ResultMeasure.MeasureUnitCode)) 
  
  return(wqp_data_out)
  
}


                                    