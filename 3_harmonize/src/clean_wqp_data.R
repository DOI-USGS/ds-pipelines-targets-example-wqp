#' @title Clean WQP data
#' 
#' @description 
#' Function to harmonize WQP data in preparation for further analysis. Included
#' in this function are steps to unite diverse characteristic names by assigning
#' them to more commonly-used water quality parameter names, and to flag missing
#' records as well as duplicate records.
#' 
#' @param char_names_crosswalk data frame containing columns "char_name" and 
#' "parameter". The column "char_name" contains character strings representing 
#' known WQP characteristic names associated with each parameter.
#' @param wqp_params list object where each element of the list is a named vector.
#' The vectors correspond with a parameter group and contain character string(s)
#' representing known WQP characteristic names associated with each parameter. 
#' @param commenttext_missing character string(s) indicating which strings from
#' the WQP column "ResultCommentText" correspond with missing result values. By 
#' default, the column "ResultCommentText" will be searched for the following 
#' strings: "analysis lost", "not analyzed", "not recorded", "not collected", 
#' and "no measurement taken", but other values may be added as well. 
#' 
#' @returns 
#' Returns a formatted and harmonized data frame containing data downloaded from 
#' the Water Quality Portal, where each row represents a unique data record.
#' 
clean_wqp_data <- function(wqp_data, char_names_crosswalk,
                           commenttext_missing = c('analysis lost', 'not analyzed', 
                                                   'not recorded', 'not collected', 
                                                   'no measurement taken')){

  # Clean data and assign flags if applicable
  wqp_data_clean <- wqp_data %>%
    # harmonize characteristic names by assigning a common parameter name
    # to the groups of characteristics supplied in `char_names_crosswalk`.
    left_join(y = char_names_crosswalk, by = c("CharacteristicName" = "char_name")) %>%
    # flag true missing results, i.e. when result measure value and detection
    # limit value are both NA, when "not reported" is found in the column
    # "ResultDetectionConditionText", or when any of the strings from
    # `commenttext_missing` are found in the column "ResultCommentText".
    mutate(flag_missing_result = case_when(
      is.na(ResultMeasureValue) & is.na(DetectionQuantitationLimitMeasure.MeasureValue) ~ TRUE,
      grepl("not reported", ResultDetectionConditionText, ignore.case = TRUE) ~ TRUE,
      grepl(paste(commenttext_missing, collapse = "|"), ResultCommentText, ignore.case = TRUE) ~ TRUE
    )) 
  
  return(wqp_data_clean)
  
  
}

