#' Function to summarize the expected number of sites and samples to be returned
#' from the WQP for the area of interest
#'
#' @param wqp_inventory data frame containing sites returned from the WQP query,
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
#' @param wqp_query_summary_csv character string indicating the name of the saved 
#' query summary file, including file path and extension.
#' @param wqp_data data frame containing the output from fetch_wqp_data; contains 
#' data downloaded from the WQP, where each row represents a unique data record.
#' @param fileout character string indicating the name of the saved file, including
#' file path and extension.
#'  
summarize_wqp_data <- function(wqp_query_summary_csv, wqp_data, fileout){
  
  # Read in WQP query summary
  expected_results <- readr::read_csv(wqp_query_summary_csv, 
                                      show_col_types = FALSE) %>%
    pull(results_count) %>%
    sum()
  realized_results <- dim(wqp_data)[1]
  
  # Compare WQP query summary with data pull 
  if(realized_results != expected_results){
    warning(sprintf("Was expecting %s rows of data based on the WQP inventory but data pull returned %s rows.", 
                    expected_results, realized_results))
  } else {
    message(sprintf("All good! Was expecting %s rows of data based on the WQP inventory and data pull returned %s rows.",
                    expected_results, realized_results))
  }
  
  # Summarize WQP data pull
  wqp_summary <- wqp_data %>%
    group_by(CharacteristicName) %>%
    summarize(sites_count = length(unique(MonitoringLocationIdentifier)),
              results_count = length(ResultMeasureValue),
              .groups = 'drop') %>%
    select(CharacteristicName, sites_count, results_count)
  
  # Write summary file
  write_csv(wqp_summary, file = fileout)
  
  return(fileout)
  
}

