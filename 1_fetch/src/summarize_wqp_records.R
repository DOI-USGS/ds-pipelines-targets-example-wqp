#' Function to summarize the expected number of sites and samples to be returned
#' from the WQP for the area of interest
#'
#' @param wqp_inventory data frame containing sites returned from the WQP inventory,
#' with one row per site; contains columns CharacteristicName and resultCount.
#' @param fileout character string indicating the name of the saved file, including
#' file path and extension.
#' 
summarize_wqp_inventory <- function(wqp_inventory, fileout){
  
  # Summarize WQP inventory
  wqp_summary <- wqp_inventory %>%
    group_by(CharacteristicName) %>%
    summarize(sites_count = n(),
              results_count = sum(resultCount),
              .groups = 'drop') %>%
    select(CharacteristicName, sites_count, results_count)
  
  # Write summary file
  write_csv(wqp_summary, file = fileout)
  
  return(fileout)
  
}


#' Summarize data downloaded from the Water Quality Portal
#' 
#' @description Function to summarize the data downloaded from the WQP and 
#' compare with the number of sites and samples that were expected based on 
#' the WQP inventory.
#' 
#' @param wqp_inventory_summary_csv character string indicating the name of the saved 
#' inventory summary file, including file path and extension.
#' @param wqp_data data frame containing the output from fetch_wqp_data; contains 
#' data downloaded from the WQP, where each row represents a unique data record.
#' @param fileout character string indicating the name of the saved file, including
#' file path and extension.
#'  
summarize_wqp_data <- function(wqp_inventory_summary_csv, wqp_data, fileout){
  
  # Read in WQP inventory summary
  wqp_inventory_summary <- readr::read_csv(wqp_inventory_summary_csv, show_col_types = FALSE) %>%
    rename(sites_count_expected = sites_count, results_count_expected = results_count)
  
  # Summarize WQP data pull
  wqp_summary <- wqp_data %>%
    group_by(CharacteristicName) %>%
    summarize(sites_count = length(unique(MonitoringLocationIdentifier)),
              results_count = length(ResultMeasureValue),
              .groups = 'drop') %>%
    select(CharacteristicName, sites_count, results_count)
  
  # Compare WQP inventory summary with data pull 
  compare_summaries <- wqp_summary %>%
    left_join(wqp_inventory_summary, by = "CharacteristicName") %>%
    mutate(results_diff = results_count_expected - results_count,
           sites_diff = sites_count_expected - sites_count)
  
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

