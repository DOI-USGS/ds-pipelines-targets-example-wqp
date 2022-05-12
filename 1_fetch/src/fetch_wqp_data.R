fetch_wqp_data <- function(wqp_inventory, characteristics, query_site_limit = 500, 
                           min_results = 1, min_date = "", wqp_args = NULL){
  
  siteids <- wqp_inventory %>%
    pull(MonitoringLocationIdentifier) %>%
    unique()
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

