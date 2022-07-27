#' @title Summarize available data in the Water Quality Portal
#' 
#' @description
#' Function to summarize the expected number of sites and records that would
#' be returned from the WQP for the area of interest.
#'
#' @param wqp_inventory data frame containing sites returned from the WQP 
#' inventory, with one row per site; must contain columns CharacteristicName 
#' and resultCount.
#' @param fileout character string indicating the name of the saved file, 
#' including the file path and extension.
#' 
#' @returns 
#' Saves a .csv file containing the total number of sites and records available
#' in the Water Quality Portal for each requested characteristic name.
#' 
summarize_wqp_inventory <- function(wqp_inventory, fileout){
  
  # Summarize WQP inventory
  wqp_summary <- wqp_inventory %>%
    group_by(CharacteristicName) %>%
    summarize(n_sites = n(),
              n_records = sum(resultCount),
              .groups = 'drop') %>%
    select(CharacteristicName, n_sites, n_records)
  
  # Write summary file
  write_csv(wqp_summary, file = fileout)
  
  return(fileout)
  
}


#' @title Summarize data downloaded from the Water Quality Portal
#' 
#' @description 
#' Function to summarize the number of sites and records downloaded from the 
#' WQP and compare against the values that were expected based on the WQP 
#' inventory.
#' 
#' @param wqp_inventory_summary_csv character string indicating the name of the 
#' saved inventory summary file, including the file path and extension.
#' @param wqp_data data frame containing the data downloaded from the WQP, where
#' each row represents a unique data record.
#' @param fileout character string indicating the name of the saved file, including
#' the file path and extension.
#' 
#' @returns 
#' Saves a .csv file containing the total number of sites and records downloaded
#' from the Water Quality Portal for each requested characteristic name.
#'  
summarize_wqp_data <- function(wqp_inventory_summary_csv, wqp_data, fileout){
  
  # Read in WQP inventory summary
  wqp_inventory_summary <- readr::read_csv(wqp_inventory_summary_csv, show_col_types = FALSE) %>%
    rename(n_sites_expected = n_sites, n_records_expected = n_records)
  
  # Summarize WQP data pull
  wqp_summary <- wqp_data %>%
    group_by(CharacteristicName) %>%
    summarize(n_sites = length(unique(MonitoringLocationIdentifier)),
              n_records = length(ResultMeasureValue),
              .groups = 'drop') %>%
    select(CharacteristicName, n_sites, n_records)
  
  # Compare WQP inventory summary with data pull 
  compare_summaries <- wqp_summary %>%
    left_join(wqp_inventory_summary, by = "CharacteristicName") %>%
    mutate(results_diff = n_records_expected - n_records,
           sites_diff = n_sites_expected - n_sites)
  
  msg_suggest <- "Check that wqp_args are the same for both p1_wqp_inventory and p1_wqp_data_aoi. 
  In addition, check whether the inventory is outdated by running tar_outdated() 
  or tar_visnetwork(). If outdated, try running tar_invalidate(p1_wqp_inventory) 
  followed by tar_make() to re-pull the data."
  
  msg_if_greater <- sprintf("The expected number of records from the WQP inventory is greater than
  the number of records in the data pull for the following characteristics: \n\n%s\n\n%s\n",
  paste(compare_summaries$CharacteristicName[compare_summaries$results_diff > 0], collapse="\n"),
  msg_suggest)
  
  msg_if_fewer <- sprintf("The expected number of records from the WQP inventory is less than
  the number of records in the data pull for the following characteristics:\n\n%s\n\n%s\n",
  paste(compare_summaries$CharacteristicName[compare_summaries$results_diff < 0], collapse="\n"),
  msg_suggest)
  
  if(any(compare_summaries$results_diff != 0)) {
    # Find any characteristics for which the inventory contains more records than the data pull
    if(any(compare_summaries$results_diff > 0)){
      warning(msg_if_greater)
    }
    # Find any characteristics for which the inventory contains fewer records than the data pull
    if(any(compare_summaries$results_diff < 0)){
      warning(msg_if_fewer)
    }
  } else {
    message("All good! The expected number of records from the WQP inventory matches the data pull for all characteristics.")
  }
  
  # Write summary file
  write_csv(wqp_summary, file = fileout)
  
  return(fileout)
  
}


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

