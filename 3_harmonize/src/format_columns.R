#' @title Format WQP data columns, including to convert some columns of class
#' character back to numeric
#' 
#' @description
#' Function to format WQP data columns, including to convert some columns of
#' class character back to numeric. This function also presents the option 
#' to drop undesired columns from the formatted data frame.
#' 
#' @param wqp_data data frame containing the data downloaded from the WQP, 
#' where each row represents a unique data record.
#' @param save_dir character string indicating the directory where an output
#' file should be saved. The output file is intended to inform the user of any
#' WQP entries in columns `vars_to_numeric` that were not able to be parsed to
#' numeric values and so were replaced with NA.
#' @param vars_to_numeric character string indicating which column(s) should be 
#' coerced to class numeric. Default includes two columns: `ResultMeasureValue` 
#' and `DetectionQuantitationLimitMeasure.MeasureValue`.
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
                           save_dir,
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
    select(-c(any_of(drop_vars)))
    
  # Save a file containing which values were replaced with NA when column
  # was converted from character to numeric  
  save_NA_vals(wqp_data, save_dir, vars_to_numeric)
  
  return(wqp_data_out)
}


#' @title Record values that have been replaced with NA
#' 
#' @description
#' Function to save an output file that contains any values that were replaced
#' with NA when a column was converted from class character to class numeric. 
#' Values were replaced with NA if the original entry could not be parsed as 
#' a numeric value.
#' 
#' @param wqp_data data frame containing the data downloaded from the WQP, 
#' where each row represents a unique data record.
#' @param save_dir character string indicating the directory where an output
#' file should be saved. The output file is intended to inform the user of any
#' WQP entries in columns `vars_to_numeric` that were not able to be parsed to
#' numeric values and so were replaced with NA.
#' @param vars_to_numeric character string indicating which column(s) should be 
#' coerced to class numeric. 
#' 
#' @returns 
#' Returns a character string indicating the file path of the saved output file.
#' The saved .txt file includes a section for each column in `vars_to_numeric` 
#' that contains the original WQP entries for that column that were replaced with 
#' NA when the column was converted from class character to class numeric.
#' 
save_NA_vals <- function(wqp_data, save_dir, vars_to_numeric){

  # Find any values from the original columns that were replaced with NA
  values_replaced_with_na <- lapply(vars_to_numeric, function(x){
    
    vars_vals_na <- wqp_data %>%
      filter(!is.na(.data[[x]]),
             is.na(as.numeric(.data[[x]]))) %>%
      suppressWarnings() %>%
      pull(.data[[x]]) %>% 
      unique()
    
    if(length(vars_vals_na) > 0){
      out <- sprintf(paste0("The following entries for ", x, " cannot be ",
                            "parsed as numeric and will be converted to ",
                            "NA:\n\n%s\n"),
                     paste(unique(vars_vals_na), collapse = "\n"))
    } else {
      out <- paste0("Good news! All entries for ", x, " could be parsed as ",
                    "numeric. \n")
    }
    return(out)
  })

  # Save a file containing which values were replaced with NA when column
  # was converted from character to numeric
  out_file <- paste0(save_dir,"/WQP_entries_replaced_with_NA.txt")
  writeLines(paste(unlist(lapply(values_replaced_with_na, paste, collapse = " "))), 
             out_file)
  
  return(out_file)
}

