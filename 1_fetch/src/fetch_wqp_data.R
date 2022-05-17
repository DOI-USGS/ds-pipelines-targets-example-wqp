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
#' @param siteids vector of character strings containing site identifiers
#' @param query_site_limit integer; chunk up the WQP data pulls by setting 
#' the maximum number of sites to include in any single data pull. Defaults
#' to 500.
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
fetch_wqp_data <- function(siteids, characteristics, query_site_limit = 500, 
                           min_results = 1, min_date = "", wqp_args = NULL){
  
  n_total_sites <- length(siteids) 
  
  # Break up sites into multiple WQP calls to avoid timeout issues 
  start_time <- Sys.time()
  sites_chunk <- ceiling(n_total_sites/query_site_limit)

  # Pull data for each chunk
  wqp_data_ls <- list()
  
  for(i in seq_len(sites_chunk)){
    start_chk <- (i-1)*query_site_limit + 1
    end_chk <- ifelse(i == sites_chunk, n_total_sites, i*query_site_limit)

    message(sprintf("Retrieving WQP data for sites %s:%s out of %s total sites",
                    start_chk, end_chk, n_total_sites))
    
    # define arguments for readWQPdata
    wqp_args_all <- c(wqp_args, list(siteid = siteids[start_chk:end_chk],
                                 characteristicName = c(characteristics),
                                 minresults = min_results, 
                                 startDateLo = min_date))
    # pull data
    site_data <- dataRetrieval::readWQPdata(wqp_args_all)
    wqp_data_ls[[i]] <- site_data
  }
  
  # Return data frame containing all of the downloaded data
  wqp_data <- bind_rows(wqp_data_ls)
  
  # Print messages that indicate how the download went
  end_time <- Sys.time()
  elapsed_time <- round(as.numeric(difftime(end_time, start_time, units = "secs")), 1)

  message(sprintf("Retrieved %s rows of data in %s seconds", 
                  length(wqp_data$ResultMeasureValue), elapsed_time))
  
  return(wqp_data)
}

