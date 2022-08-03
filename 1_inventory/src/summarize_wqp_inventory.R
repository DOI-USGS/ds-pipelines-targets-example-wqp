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

