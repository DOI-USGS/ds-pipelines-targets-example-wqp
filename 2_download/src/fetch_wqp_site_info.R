#' @title Group unique site identifiers
#' 
#' @description 
#' Function to split unique WQP site identifiers into smaller groups to 
#' facilitate reasonably sized requests to the WQP "Station" service. 
#' 
#' @param site_ids character vector indicating the site identifiers that will
#' be split into distinct groups based on the some number of max sites.
#' @param max_sites integer indicating the maximum number of sites allowed in
#' each group. Defaults to 500.
#' 
#' @returns 
#' Returns a data frame with columns "site_id" and "site_group".
#' 
add_site_groups <- function(site_ids, max_sites){
  
  site_ids_grouped <- tibble(site_id = site_ids) %>%
    distinct() %>%
    mutate(site_group = ceiling(row_number()/max_sites))
  
  return(site_ids_grouped)
}



#' @title Fetch WQP site metadata
#' 
#' @description 
#' Function to pull site location metadata for Water Quality Portal sites.
#' 
#' @details 
#' This function will make multiple attempts to call the WQP "Station" service
#' if the initial request fails or if the query takes too long to return results.
#' See `2_download/src/retry.R` for more information about retry handling.
#' 
#' @param site_ids character string or character vector indicating the site
#' identifiers to request from the WQP "Station" service.
#' @param max_tries integer indicating the maximum number of retry attempts.
#' @param timeout_minutes_per_site integer; indicates the maximum time that should be
#' allowed to elapse per site before retrying the data download step. The total time
#' used for the retry is calculated as number of sites * `timeout_minutes_per_site`.
#' Defaults to 5 minutes per site. 
#' @param sleep_on_error integer indicating how long (in seconds) we should wait 
#' before making another attempt. Defaults to zero.
#' @param verbose logical; should messages about retry status be printed to the
#' console? Defaults to FALSE.  
#' 
#' @returns 
#' Returns a data frame containing the site location metadata. Note that
#' some records contain character strings when we expect numeric values.
#' To bind the records together, all columns are formatted as character.
#'
fetch_wqp_site_info <- function(site_ids, 
                                max_tries = 3, 
                                timeout_minutes_per_site = 5, 
                                sleep_on_error = 0, 
                                verbose = FALSE){

  # Define how long to wait before the WQP query times out. For any single 
  # attempt, stop and retry if the time elapsed exceeds `timeout_minutes`. Use 
  # at least 1 minute so that it doesn't error if `length(site_ids) == 0`.
  timeout_minutes <- 1 + timeout_minutes_per_site * length(site_ids)
  
  # Download site metadata from the "Station" service, using the fault-tolerant
  # wrapper function `pull_data_safely` (see `2_download/src/retry.R`).
  wqp_args <- list(siteid = site_ids, service = "Station")
  site_info <- pull_data_safely(dataRetrieval::whatWQPsites, wqp_args,
                                timeout_minutes = timeout_minutes,
                                max_tries = max_tries, 
                                sleep_on_error = sleep_on_error,
                                verbose = verbose)
  
  # Add columns to match NWIS format and `dataRetrieval::readWQPdata` output:
  # https://github.com/DOI-USGS/dataRetrieval/blob/main/R/readWQPdata.R#L233-L257
  site_info_full <- site_info %>%
    mutate(station_nm = MonitoringLocationName,
           agency_cd = OrganizationIdentifier,
           site_no = MonitoringLocationIdentifier,
           dec_lat_va = LatitudeMeasure,
           dec_lon_va = LongitudeMeasure,
           hucCd = HUCEightDigitCode) %>%
    select(station_nm, agency_cd, site_no, dec_lat_va, dec_lon_va, hucCd,
           everything())
  
  # Some records return character strings when we expect numeric values, e.g. if
  # `HorizontalAccuracyMeasure.MeasureValue == "Unknown"`. For now, format all 
  # columns as character so that individual data frames can be joined together 
  # in the targets pipeline. 
  site_info_fmt <- site_info_full %>%
    mutate(across(everything(), as.character))
  
  return(site_info_fmt)
}


