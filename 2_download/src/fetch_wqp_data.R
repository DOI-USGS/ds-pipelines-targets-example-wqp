source("2_download/src/retry.R")

#' @title Download data from the Water Quality Portal
#' 
#' @description 
#' Function to pull WQP data given a dataset of site ids and/or site coordinates.
#' 
#' @details 
#' This function will retry the data pull if the initial request fails or if
#' the query takes too long to return results. See `retry_with_timeout` for 
#' more information about retry handling and for argument descriptions. 
#'  
#' @param site_counts_grouped data frame containing a row for each site. Columns 
#' contain the site identifiers, the total number of records, and an assigned
#' download group. Must contain columns `site_id` and `pull_by_id`, where
#' `pull_by_id` is logical and indicates whether data should be downloaded
#' using the site identifier or by querying a small bounding box around the site.
#' @param char_names vector of character strings indicating which WQP 
#' characteristic names to query.
#' @param wqp_args list containing additional arguments to pass to whatWQPdata(),
#' defaults to NULL. See https://www.waterqualitydata.us/webservices_documentation 
#' for more information.  
#' 
#' @returns
#' Returns a data frame containing data downloaded from the Water Quality Portal, 
#' where each row represents a unique data record.
#' 
#' @examples
#' site_counts <- data.frame(site_id = c("USGS-01475850"), pull_by_id = c(TRUE))
#' fetch_wqp_data(site_counts, "Temperature, water", wqp_args = list(siteType = "Stream"))
#' 
fetch_wqp_data <- function(site_counts_grouped, 
                           char_names, 
                           wqp_args = NULL, 
                           max_tries = 3, 
                           timeout_minutes_per_site = 5, 
                           sleep_on_error = 0, 
                           verbose = FALSE){
  
  message(sprintf("Retrieving WQP data for %s sites in group %s, %s",
                  nrow(site_counts_grouped), unique(site_counts_grouped$download_grp), 
                  char_names))
  
  # Define arguments for readWQPdata
  # sites with pull_by_id = FALSE cannot be queried by their site
  # identifiers because of undesired characters that will cause the WQP
  # query to fail. For those sites, query WQP by adding a small bounding
  # box around the site(s) and including bBox in the wqp_args.
  if(all(site_counts_grouped$pull_by_id)){
    wqp_args_all <- c(wqp_args, 
                      list(siteid = site_counts_grouped$site_id,
                           characteristicName = c(char_names)))
  } else {
    wqp_args_all <- c(wqp_args, 
                      list(bBox = create_site_bbox(site_counts_grouped),
                           characteristicName = c(char_names)))
  }

  # Pull the data, retrying up to the number of times indicated by `max_tries`.
  # For any single attempt, stop and retry if the time elapsed exceeds
  # `timeout_minutes`. Use at least 1 min so that it doesn't error if 
  # `length(site_counts_grouped$site_ids) == 0`
  timeout_minutes <- 1 + timeout_minutes_per_site * length(site_counts_grouped$site_id)
  
  wqp_data <- retry_with_timeout(dataRetrieval::readWQPdata, wqp_args_all,
                                 timeout_minutes = timeout_minutes,
                                 max_tries = max_tries, 
                                 sleep_on_error = sleep_on_error,
                                 verbose = verbose)

  # We applied special handling for sites with pull_by_id = FALSE (see comments
  # above). Filter wqp_data to only include sites requested in site_counts_grouped
  # in case our bounding box approach picked up any additional, undesired sites. 
  # In addition, some records return character strings when we expect numeric 
  # values, e.g. when "*Non-detect" appears in the "ResultMeasureValue" field. 
  # For now, consider all columns to be character so that individual data
  # frames returned from fetch_wqp_data can be joined together. 
  wqp_data_out <- wqp_data %>%
    filter(MonitoringLocationIdentifier %in% site_counts_grouped$site_id) %>%
    mutate(across(everything(), as.character))
  
  return(wqp_data_out)
}

