#' Group sites for downloading data without hitting the WQP cap
#' 
#' @description This function returns a data frame with a single column used
#' to group the sites into reasonably sized chunks for downloading data.
#' 
#' @param siteids vector of character strings containing site identifiers
#' @param max_sites_allowed integer indicating the maximum number
#' of sites allowed in each download group. Defaults to 500.
#' 
#' @return returns a data frame with columns site id, site number, and an 
#' additional column called `download_grp` which is made up of unique groups 
#' to `group_by()` and then `tar_group()` for downloading.
#' 
add_download_groups <- function(siteids, max_sites_allowed = 500) {
  
  siteids_df <- tibble(site_id = siteids) %>%
    mutate(site_n = row_number()) %>%
    # Add a column indicating which chunk a site belongs to; the number of rows
    # in each chunk should not exceed `max_sites_allowed`
    mutate(download_grp = ((site_n -1) %/% max_sites_allowed) + 1)
  
  return(siteids_df)

}


#' Download data from the Water Quality Portal
#' 
#' @description Function to pull WQP data given a vector of site ids
#'  
#' @param siteids_grouped vector of character strings containing site identifiers
#' @param characteristics vector of character strings indicating which WQP
#' CharacteristicName to query
#' @param wqp_args list containing additional arguments to pass to whatWQPdata(),
#' defaults to NULL. See https://www.waterqualitydata.us/webservices_documentation 
#' for more information.  
#' 
#' @return returns a data frame containing data downloaded from the Water Quality Portal, 
#' where each row represents a unique data record. 
#' 
fetch_wqp_data <- function(siteids_grouped, characteristics, wqp_args = NULL){
  
  message(sprintf("Retrieving WQP data for sites %s:%s",
                  min(siteids_grouped$site_n), max(siteids_grouped$site_n)))
  
  # Define arguments for readWQPdata
  wqp_args_all <- c(wqp_args, list(siteid = siteids_grouped$site_id,
                                   characteristicName = c(characteristics)))
  
  # Pull data
  wqp_data <- dataRetrieval::readWQPdata(wqp_args_all)
  
  return(wqp_data)
}



#' Summarize data downloaded from the Water Quality Portal
#' 
#' @description Function to summarize the data downloaded from the WQP and 
#' compare with the number of sites and samples that were expected based on 
#' the WQP inventory.
#' 
#' @param wqp_query_summary character string indicating the name of the saved 
#' query summary file, including file path and extension.
#' @param wqp_data data frame containing the output from fetch_wqp_data; contains 
#' data downloaded from the WQP, where each row represents a unique data record.
#' @param fileout character string indicating the name of the saved file, including
#' file path and extension.
#'  
summarize_data_pull <- function(wqp_query_summary_csv, wqp_data, fileout){
  
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

