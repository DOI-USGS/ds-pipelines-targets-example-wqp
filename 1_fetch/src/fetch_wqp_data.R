#' Group sites for downloading data without hitting the WQP cap
#' 
#' @description This function returns a data frame with a single column used
#' to group the sites into reasonably sized chunks for downloading data.
#' 
#' @param sitecounts_df data frame containing the site identifiers and total number of
#' records available for each site. Must contain columns `MonitoringLocationIdentifier`.
#' @param max_sites integer indicating the maximum number of sites allowed in each
#' download group. Defaults to 500.
#' @param max_results integer indicating the maximum number of records allowed in
#' each download group. Defaults to 250,000.
#' 
#' @return returns a data frame with columns site id, the total number of records,
#' (retains the column from `sitecounts_df`), site number, and an additional column 
#' called `download_grp` which is made up of unique groups that enable use of 
#' `group_by()` and then `tar_group()` for downloading.
#' 
add_download_groups <- function(sitecounts_df, max_sites = 500, max_results = 250000) {
  
  # Check whether any individual sites have a records count that exceeds `max_results`
  if(any(sitecounts_df$results_count > max_results)){
    sites_w_many_records <- sitecounts_df %>%
      filter(results_count > max_results) %>%
      pull(MonitoringLocationIdentifier)
    # Print a message to inform the user that some sites contain a lot of data
    message(sprintf(paste0("results_count exceeds max_results for the sites below. ",
                           "Assigning data-heavy sites to their own download group to ",
                           "set up a manageable query to WQP. If you are not already ",
                           "branching across characteristic names, consider doing so to ",
                           "further limit query size. \n\n%s\n"),
                    paste(sites_w_many_records, collapse="\n")))
  }
  
  # Within each unique grid_id, use the cumsumbinning function from the MESS
  # package to group sites based on the cumulative sum of results_count 
  # across sites within the grid, resetting the download group/task number 
  # if the number of records exceeds the threshold set by `max_results`
  sitecounts_grouped <- sitecounts_df %>%
    rename(site_id = MonitoringLocationIdentifier) %>% 
    split(.$grid_id) %>%
    purrr::map_dfr(.f = function(df){
      
      df_grouped <- df %>%
        arrange(desc(results_count)) %>%
        mutate(task_num = MESS::cumsumbinning(x = results_count, 
                                              threshold = max_results, 
                                              maxgroupsize = max_sites),
               download_grp = paste0(grid_id,"_",task_num))
    }) %>%
    mutate(site_n = row_number()) %>%
    select(site_id, results_count, site_n, download_grp)
  
  return(sitecounts_grouped)

}


#' Download data from the Water Quality Portal
#' 
#' @description Function to pull WQP data given a dataset of site ids
#'  
#' @param site_counts_grouped data frame containing site identifiers, the 
#' total number of records, site numbers, and a download group assigned
#' for each site. Must contain columns `site_id` and `site_n`.
#' @param characteristics vector of character strings indicating which WQP
#' CharacteristicName to query
#' @param wqp_args list containing additional arguments to pass to whatWQPdata(),
#' defaults to NULL. See https://www.waterqualitydata.us/webservices_documentation 
#' for more information.  
#' @param max_tries integer, maximum number of attempts if the data download 
#' step returns an error. Defaults to 3.
#' 
#' @return returns a data frame containing data downloaded from the Water Quality Portal, 
#' where each row represents a unique data record. 
#' 
fetch_wqp_data <- function(site_counts_grouped, characteristics, wqp_args = NULL, max_tries = 3){
  
  message(sprintf("Retrieving WQP data for sites %s:%s",
                  min(site_counts_grouped$site_n), 
                  max(site_counts_grouped$site_n)))
  
  # Define arguments for readWQPdata
  wqp_args_all <- c(wqp_args, list(siteid = site_counts_grouped$site_id,
                                   characteristicName = c(characteristics)))
  
  # Pull data, retrying up to the number of times indicated by `max_tries`
  wqp_data <- retry::retry(dataRetrieval::readWQPdata(wqp_args_all),
                           when = "Error:", 
                           max_tries = max_tries)
  
  # Some records return character strings when we expect numeric values, 
  # e.g. when "*Non-detect" appears in the "ResultMeasureValue" field. 
  # For now, consider all columns to be character so that individual data
  # frames returned from fetch_wqp_data can be joined together. 
  wqp_data_out <- wqp_data %>%
    mutate(across(everything(), as.character))
  
  return(wqp_data_out)
}

