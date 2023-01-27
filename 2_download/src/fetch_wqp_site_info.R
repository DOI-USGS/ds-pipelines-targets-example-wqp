#' @title Fetch WQP site metadata
#' 
#' @description 
#' Function to pull site location metadata from the WQP data attributes.
#' 
#' @details 
#' Data downloaded from WQP using `dataRetrieval` contain an appended attribute 
#' called "siteInfo" by default. These attributes are not retained when `targets`
#' maps across the groups of sites to build `p2_wqp_data_aoi`. This function pulls
#' out the site information for each branch within `p2_wqp_data_aoi` and then binds
#' the rows into a single data frame.
#' 
#' @param wqp_data data frame containing the data downloaded from the WQP, 
#' where each row represents a unique data record.
#' 
#' @returns 
#' Returns a data frame containing the site location metadata. Note that
#' some records contain character strings when we expect numeric values.
#' To bind the records together, all columns are formatted as character.
#'
fetch_wqp_site_info <- function(wqp_data){

  # Some records return character strings when we expect numeric values, e.g. if
  # `HorizontalAccuracyMeasure.MeasureValue == "Unknown"`. For now, format all 
  # columns as character so that individual data frames can be joined together 
  # in the targets pipeline. 
  site_info_fmt <- attributes(wqp_data)$siteInfo %>%
    mutate(across(everything(), as.character))
  
  return(site_info_fmt)
}


