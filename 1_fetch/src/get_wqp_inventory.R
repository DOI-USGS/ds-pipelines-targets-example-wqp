#' Inventory WQP within area of interest
#' 
#' @description Function to inventory WQP sites and observations within the area of interest
#' 
#' @param grid sf object representing the area over which to query the WQP
#' @param char_names character string or list of character strings indicating the 
#' WQP CharacteristicNames to query
#' @param ... see  https://www.waterqualitydata.us/webservices_documentation for more 
#' information on the options accepted by dataRetrieval::whatWQPdata().
#' 
#' @value returns a list of sites from the Water Quality Portal
#' 
#' @example inventory_wqp(aoi_bbox, "Conductivity", siteType = "Stream")
#' @example inventory_wqp(aoi_bbox, "Temperature", siteType = "Lake, Reservoir, Impoundment")
#' 

inventory_wqp <- function(grid, char_names, ...){
  
  # Get bounding box for the grid polygon
  bbox <- sf::st_bbox(grid)
  
  # Format characteristic names
  char_names <- as.character(unlist(char_names))
  
  # Print time-specific message so user can see progress
  message(sprintf('Retrieving whatWQPdata for grid %s...', grid$id))

  # Inventory available WQP data
  wqp_inventory <- lapply(char_names,function(x){
    dataRetrieval::whatWQPdata(
      bBox = c(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax),
      characteristicName = x,
      ...) %>%
      mutate(CharacteristicName = x)
  }) %>%
    bind_rows()
  
  # Format WQP inventory 
  wqp_inventory_out <- wqp_inventory %>%
    group_by(CharacteristicName) %>%
    summarize(sites_count = n(),
              results_count = sum(resultCount)) %>%
    mutate(grid_id = grid$id) %>%
    select(grid_id, CharacteristicName, sites_count, results_count)

  return(wqp_inventory_out)
  
}

