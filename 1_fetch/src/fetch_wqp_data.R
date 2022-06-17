#' Group sites for downloading data without hitting the WQP cap
#' 
#' @description This function returns a data frame with a single column used
#' to group the sites into reasonably sized chunks for downloading data.
#' 
#' @param sitecounts_df data frame containing the site identifiers and total number of
#' records available for each site. Must contain columns `MonitoringLocationIdentifier`.
#' @param max_sites_allowed integer indicating the maximum number of sites allowed
#' in each download group. Defaults to 500.
#' 
#' @return returns a data frame with columns site id, the total number of records,
#' (retains the column from `sitecounts_df`), site number, and an additional column 
#' called `download_grp` which is made up of unique groups that enable use of 
#' `group_by()` and then `tar_group()` for downloading.
#' 
add_download_groups <- function(sitecounts_df, max_sites_allowed = 500) {
  
  sitecounts_grouped <- sitecounts_df %>%
    rename(site_id = MonitoringLocationIdentifier) %>% 
    mutate(site_n = row_number()) %>%
    # Add a column indicating which chunk a site belongs to; the number of rows
    # in each chunk should not exceed `max_sites_allowed`
    mutate(download_grp = ((site_n -1) %/% max_sites_allowed) + 1)
  
  return(sitecounts_grouped)

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

