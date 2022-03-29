#' Inventory WQP within area of interest
#' 
#' @description Function to inventory WQP sites and observations within the area of interest
#' 
#' @param bbox_tbl data frame containing an attribute id and bounding box for each
#' box sampled within the area of interest. The column bbox is of class list.
#' @param char_names character string indicating the WQP CharacteristicNames to query
#' @param site_type character string indicating which water feature type(s) to query
#' @param year_earliest integer; the earliest year to include in the returned inventory,
#' defaults to NULL, which returns all available years
#' @param year_latest integer; the most recent year to include in the returned inventory,
#' defaults to NULL, which returns all available years
#' 
#' @value returns a list of sites from the Water Quality Portal
#' 
#' @example inventory_wqp(aoi_bbox, "Conductivity", "Stream")
#' @example inventory_wqp(aoi_bbox, "Temperature", "Lake, Reservoir, Impoundment")
#' 

inventory_wqp <- function(bbox, char_names, site_type, year_earliest = NULL, year_latest = NULL){
  
  # Inventory available WQP data
  wqp_inventory <- dataRetrieval::readWQPsummary(
    bBox = c(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax),
    summaryYears = "all",
    siteType = site_type)
  
  # Format WQP inventory and subset years of interest
  wqp_inventory_out <- wqp_inventory %>%
    select(MonitoringLocationIdentifier, YearSummarized, OrganizationIdentifier, 
           CharacteristicName, MonitoringLocationLongitude, 
           MonitoringLocationLatitude, ResultCount) %>%
    rename(lon = MonitoringLocationLongitude, 
           lat = MonitoringLocationLatitude) %>%
    filter(CharacteristicName %in% char_names) %>%
    filter(if(!is.null(year_earliest)) YearSummarized >= year_earliest else TRUE) %>%
    filter(if(!is.null(year_latest)) YearSummarized <= year_latest else TRUE) 
  
  return(wqp_inventory_out)
  
}


