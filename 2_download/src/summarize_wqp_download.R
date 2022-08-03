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
summarize_wqp_download <- function(wqp_inventory_summary_csv, wqp_data, fileout){
  
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

