#' Inventory WQP within area of interest
#' 
#' @description Function to inventory WQP sites and observations within the area of interest
#' 
#' @param grid sf object representing the area over which to query the WQP
#' @param char_names character string or list of character strings indicating the 
#' WQP CharacteristicNames to query
#' @param wqp_args list containing additional arguments to pass to dataRetrieval::whatWQPdata(),
#' defaults to NULL; see https://www.waterqualitydata.us/webservices_documentation for more 
#' information.  
#' 
#' @value returns a data frame containing sites available from the Water Quality Portal
#' 
#' @example inventory_wqp(aoi_bbox, "Conductivity", wqp_args = list(siteType = "Stream"))
#' @example inventory_wqp(aoi_bbox, "Temperature", wqp_args = list(siteType = "Lake, Reservoir, Impoundment"))
#' 
inventory_wqp <- function(grid, char_names, wqp_args = NULL){
  
  # Get bounding box for the grid polygon
  bbox <- sf::st_bbox(grid)
  
  # Format characteristic names
  char_names <- as.character(unlist(char_names))
  
  # Print time-specific message so user can see progress
  message(sprintf('Retrieving whatWQPdata for grid %s...', grid$id))

  # Inventory available WQP data
  wqp_inventory <- lapply(char_names,function(x){
    # define arguments for whatWQPdata
    wqp_args_all <- c(wqp_args, list(bBox = c(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax),
                                     characteristicName = x))
    # query WQP
    dataRetrieval::whatWQPdata(wqp_args_all) %>%
      mutate(CharacteristicName = x, grid_id = grid$id)
  }) %>%
    bind_rows()
  
  # Fetch missing CRS information from WQP
  site_location_metadata <- dataRetrieval::whatWQPsites(
    bBox = c(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax)) %>%
    select(MonitoringLocationIdentifier, HorizontalCoordinateReferenceSystemDatumName) %>%
    filter(MonitoringLocationIdentifier %in% wqp_inventory$MonitoringLocationIdentifier)
  
  # Join WQP inventory with site metadata 
  wqp_inventory_out <- wqp_inventory %>%
    left_join(site_location_metadata, by = "MonitoringLocationIdentifier")

  return(wqp_inventory_out)
  
}

