#' @title Format WQP data columns, including to convert some columns of class
#' character back to numeric
#' 
#' @description
#' Function to format WQP data columns, including to convert some columns of
#' class character back to numeric. This function also converts missing value
#' strings to NA and presents the option to drop undesired columns from the 
#' formatted data frame.
#' 
#' @param wqp_data data frame containing the data downloaded from the WQP, 
#' where each row represents a unique data record.
#' @param vars_to_numeric character string indicating which column(s) should be 
#' coerced to class numeric. Default includes two columns: `ResultMeasureValue` 
#' and `DetectionQuantitationLimitMeasure.MeasureValue`. Note that for all
#' columns coerced to numeric, the original entries will be retained in new
#' columns ending in "_original."
#' @param drop_vars optional; character string indicating any undesired
#' variables that should be dropped from the WQP dataset. Default is to retain 
#' all variables. Note that certain variables are used in downstream 
#' harmonization steps and should not be omitted from the dataset. If 
#' `drop_vars` contains a variable considered important for harmonization, an
#' error message will prompt the user to remove that variable from `drop_vars`.
#' 
#' @returns
#' Returns a formatted data frame containing data downloaded from the Water 
#' Quality Portal, where each row represents a unique data record.
#' 
format_columns <- function(wqp_data, 
                           vars_to_numeric = c('ResultMeasureValue',
                                               'DetectionQuantitationLimitMeasure.MeasureValue'),
                           drop_vars = NULL){
  
  # Some vars should not be dropped because they are needed in downstream 
  # harmonization steps.
  keep_vars <- c('CharacteristicName', 'ResultMeasureValue',
                 'DetectionQuantitationLimitMeasure.MeasureValue',
                 'ResultDetectionConditionText', 'ResultCommentText,',
                 'ResultMeasure.MeasureUnitCode')
  if(any(drop_vars %in% keep_vars)){
    drop_vars_to_keep <- keep_vars[keep_vars %in% drop_vars]
    stop(sprintf(paste0("Certain variables cannot be dropped. Please ",
                        "remove the following variables from drop_vars: \n%s\n"),
                 drop_vars_to_keep))
  }
 
  # Coerce variables to numeric, but first, retain original values in a new column
  wqp_data_out <- wqp_data %>%
    mutate(across(all_of(vars_to_numeric), ~., .names = "{col}_original"),
           across(all_of(vars_to_numeric), as.numeric)) %>%
    # format column order so that "original" cols are located next to formatted 
    # vars_to_numeric cols
    select(c("OrganizationIdentifier", "MonitoringLocationIdentifier",
             "ActivityStartDate", "ActivityStartTime.Time",
             "ActivityStartTime.TimeZoneCode", "CharacteristicName"), 
           any_of(ends_with("_original")),
           any_of(vars_to_numeric),
           everything()) %>%
    # suppress warnings including "NAs introduced by coercion" that will
    # appear if values within a column in `vars_to_numeric` contain text
    # and so cannot be parsed to a numeric value.
    suppressWarnings() %>%
    # drop any undesired columns 
    select(-c(any_of(drop_vars))) %>%
    # convert missing value strings ("", " ", "<Blank>") to NA
    mutate(across(where(is.character), ~ na_if(.,""))) %>%
    mutate(across(where(is.character), ~ na_if(.," "))) %>%
    mutate(across(where(is.character), ~ na_if(.,"<Blank>")))
  
  return(wqp_data_out)
}


