#' @title Summarize WQP records
#' 
#' @description 
#' Function to group WQP data records using any combination of columns, 
#' summarize the number of records in each group, and save the results 
#' to a .csv file. 
#' 
#' @param wqp_data data frame containing the data downloaded from the WQP, 
#' where each row represents a data record. 
#' @param grouping_cols character string or vector of strings indicating
#' which columns should be used for grouping and summarizing data records.
#' @param fileout character string indicating the name of the saved file, 
#' including file path and extension.
#' 
#' @returns 
#' Returns a .csv file containing a summary of the records in `wqp_data`. Each 
#' row represents a unique combination of columns defined in `grouping_cols`. 
#' The column "n_records" indicates the number of records within each group.
#' 
summarize_wqp_records <- function(wqp_data, grouping_cols, fileout){
  
  # Group the WQP dataset by the columns in `grouping_cols` and tally
  # the number of records within each group.
  summary <- wqp_data %>% 
    group_by(across(any_of(grouping_cols))) %>% 
    summarize(n_records = n(),
              .groups = 'drop')
  
  # Save the summary file
  readr::write_csv(x = summary, file = fileout)
  
  return(fileout)
}

