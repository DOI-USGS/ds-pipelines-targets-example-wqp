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
#' @param min_results integer indicating the minumum number of records a site
#' should contain to include that site in the data pull, defaults to 1.
#' @param min_date character string indicating the earliest requested activity 
#' start date, defaults to "", which returns all available records. 
#' @param wqp_args list containing additional arguments to pass to whatWQPdata(),
#' defaults to NULL. See https://www.waterqualitydata.us/webservices_documentation 
#' for more information.  
#' 
#' @return returns a data frame containing data downloaded from the Water Quality Portal, 
#' where each row represents a unique data record. 
#' 
fetch_wqp_data <- function(siteids_grouped, characteristics,
                           min_results = 1, min_date = "", wqp_args = NULL){
  
  message(sprintf("Retrieving WQP data for sites %s:%s",
                  min(siteids_grouped$site_n), max(siteids_grouped$site_n)))
  
  # Define arguments for readWQPdata
  wqp_args_all <- c(wqp_args, list(siteid = siteids_grouped$site_id,
                                   characteristicName = c(characteristics),
                                   minresults = min_results, 
                                   startDateLo = min_date))
  
  # Pull data
  wqp_data <- dataRetrieval::readWQPdata(wqp_args_all)
  
  return(wqp_data)
}

