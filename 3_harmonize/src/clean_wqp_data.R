#' @title Clean WQP data
#' 
#' @description 
#' Function to harmonize WQP data in preparation for further analysis. Included
#' in this function are steps to unite diverse characteristic names by assigning
#' them to more commonly-used water quality parameter names, and to flag missing
#' records as well as duplicate records.
#' 
#' @param wqp_data data frame containing the data downloaded from the WQP, 
#' where each row represents a unique data record.
#' @param char_names_crosswalk data frame containing columns "char_name" and 
#' "parameter". The column "char_name" contains character strings representing 
#' known WQP characteristic names associated with each parameter.
#' @param commenttext_missing character string(s) indicating which strings from
#' the WQP column "ResultCommentText" correspond with missing result values. By 
#' default, the column "ResultCommentText" will be searched for the following 
#' strings: "analysis lost", "not analyzed", "not recorded", "not collected", 
#' and "no measurement taken", but other values may be added by passing in a new
#' vector with all values to be treated as missing.  
#' @param duplicate_definition character string(s) indicating which columns are
#' used to identify a duplicate record. Duplicate records are defined as those 
#' that share the same value for each column within `duplicate_definition`. By 
#' default, a record will be considered duplicated if it shares the same 
#' organization, site id, date, time, characteristic name, and sample fraction. 
#' However, these options can be customized by passing a vector of column names 
#' to the argument `duplicate_definition`.
#' 
#' @returns 
#' Returns a formatted and harmonized data frame containing data downloaded from 
#' the Water Quality Portal, where each row represents a unique data record.
#' 
clean_wqp_data <- function(wqp_data, char_names_crosswalk,
                           commenttext_missing = c('analysis lost', 'not analyzed', 
                                                   'not recorded', 'not collected', 
                                                   'no measurement taken'),
                           duplicate_definition = c('OrganizationIdentifier',
                                                    'MonitoringLocationIdentifier',
                                                    'ActivityStartDate', 
                                                    'ActivityStartTime.Time',
                                                    'CharacteristicName', 
                                                    'ResultSampleFractionText')){

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
      grepl(paste(commenttext_missing, collapse = "|"), ResultCommentText, ignore.case = TRUE) ~ TRUE,
      TRUE ~ FALSE
    )) %>%
    # Flag duplicate records
    flag_duplicates(., duplicate_definition)
  
  return(wqp_data_clean)
  
  
}


#' @title Flag duplicated records
#' 
#' @description 
#' Function to flag duplicated rows based on a user-supplied definition
#' of a duplicate record. Additional flags can help reconcile duplicate sets.
#' For example, a flag "flag_duplicate_drop_random" is assigned based on 
#' randomly selecting the first record from a duplicated set, making it 
#' easier to drop all others from the dataset. 
#' 
#' @param wqp_data data frame containing the data downloaded from the WQP, 
#' where each row represents a data record.
#' @param duplicate_definition character string(s) indicating which columns are
#' used to identify a duplicate record. Duplicate records are defined as those 
#' that share the same value for each column within `duplicate_definition`.
#'
#' @returns 
#' Returns a data frame containing data downloaded from the Water Quality Portal,
#' where each row represents a unique data record. New columns appended to the 
#' original data frame include flags for duplicated records. 
#' 
flag_duplicates <- function(wqp_data, duplicate_definition){
  
  wqp_data_out <- wqp_data %>%
    # Step 1: Flag duplicate records using the `duplicate_definition`
    group_by(across(all_of(duplicate_definition))) %>% 
    mutate(n_duplicated = n(),
           flag_duplicated_row = if_else(n_duplicated > 1, TRUE, FALSE)) %>% 
    # Step 2: For remaining duplicates, randomly select the first record from
    # each duplicated set and flag all others for exclusion
    mutate(dup_number = seq(n_duplicated),
           flag_duplicate_drop_random = if_else(n_duplicated > 1 & 
                                                  dup_number != 1, 
                                                TRUE, FALSE)) %>%
    ungroup() %>%
    select(-c(n_duplicated, dup_number))
  
  return(wqp_data_out)
  
}


