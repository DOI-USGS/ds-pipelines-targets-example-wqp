#' Inventory WQP within boxes that overlap area of interest
#' 
#' @description Function to inventory WQP sites and observations within grids
#' that overlap the area of interest
#'  
#' @param grid sf object representing the area over which to query the WQP
#' @param char_names character string or list of character strings indicating 
#' the WQP CharacteristicNames to query
#' @param wqp_args list containing additional arguments to pass to whatWQPdata(),
#' defaults to NULL. See https://www.waterqualitydata.us/webservices_documentation 
#' for more information.  
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



#' Function to transform WQP sites to use a consistent coordinate reference system
#'
#' @param sites data frame containing sites returned from the WQP query; contains
#' columns lon, lat, and HorizontalCoordinateReferenceSystemDatumName.
#' @param crs_out character string indicating the desired output coordinate reference
#' system (crs); options include "NAD83" or "WGS84", defaults to "WGS84"
#'
#' @examples
#' transform_site_locations(sites)
#' transform_site_locations(sites, "NAD83")
#' 
transform_site_locations <- function(sites, crs_out = "WGS84"){
  
  message("Attempting to harmonize different site CRS...")
  
  # Define EPSG code for output data frame 
  epsg_out <- case_when(crs_out == "NAD83" ~ 4269,
                        crs_out == "WGS84" ~ 4326)
  
  # Transform sites into a consistent crs defined by epsg_out
  sites_transformed <- sites %>%
    # transform the sites data frame into a list where all sites within each 
    # list element share the same crs/datum
    rename(datum = HorizontalCoordinateReferenceSystemDatumName) %>%
    split(.,.$datum) %>%
    # Each list element contains sites with different crs. Transform the sites 
    # within each element to the desired crs
    lapply(., function(x){
      # assume unknown crs (datum equals "UNKNWN", "OTHER") correspond with WGS84
      epsg_in <- case_when(unique(x$datum) == "NAD83" ~ 4269,
                           unique(x$datum) == "WGS84" ~ 4326,
                           unique(x$datum) == "NAD27" ~ 4267,
                           unique(x$datum) == "UNKWN" ~ 4326,
                           unique(x$datum) == "OTHER" ~ 4326)
      
      # convert data frame to sf object and transform to desired crs
      if(!is.na(epsg_in)){
        sites_transformed <- x %>%
          sf::st_as_sf(coords=c("lon","lat"), crs = epsg_in) %>%
          sf::st_transform(epsg_out) %>%
          mutate(lon_new = sf::st_coordinates(.)[,1],
                 lat_new = sf::st_coordinates(.)[,2],
                 datum_new = crs_out) %>%
          sf::st_drop_geometry() %>%
          select(-datum) %>%
          rename("lon"="lon_new", "lat"="lat_new", "datum"="datum_new")
      } else {
        sites_transformed <- x
        message(sprintf("Unable to transform sites with datum = %s; returning sites untransformed",
                        x$datum[1]))
      }
      return(sites_transformed)
    }) %>%
    # combine transformed sites into a single data frame
    bind_rows()
  
  return(sites_transformed)
  
}



#' Subset WQP inventory by the area of interest polygon
#' 
#' @description Function to filter out any sites in the WQP inventory that are
#' outside of the area of interest
#' 
#' @param wqp_inventory data frame containing sites returned from the WQP query; 
#' contains columns lon, lat, and HorizontalCoordinateReferenceSystemDatumName.
#' @param aoi_sf sf object representing the area of interest
#' @param buffer_dist_m integer reflecting a desired buffer distance (in meters) around
#' the area of interest; if a site is within buffer_dist_m of aoi_sf, retain that site.
#' Defaults to zero.
#' 
#' @value returns a data frame containing sites from the Water Quality Portal that 
#' are located within the area of interest.
#' 
subset_inventory <- function(wqp_inventory, aoi_sf, buffer_dist_m = 0){
  
  # Harmonize different coordinate reference systems used across sites
  queried_sites_transformed <- transform_site_locations(wqp_inventory, crs_out= "WGS84") %>%
    sf::st_as_sf(coords = c("lon","lat"), crs = 4326) 
  
  # Filter wqp inventory to only include sites within area of interest + some user-specified 
  # distance to ensure we retain all sites within the AOI. 
  # Note that the workflow below is similar to one where we would draw a buffer around the 
  # AOI and then use sf::st_intersects to find the points that intersect the AOI polygon. 
  # st_is_within_distance is used here instead because buffers created using the s2 engine
  # are rough and can be glitchy when working with geographic coordinates. See this blog
  # post by the sf maintainers: https://r-spatial.github.io/sf/articles/sf7.html#buffers-1
  queried_sites_aoi <- queried_sites_transformed %>%
    sf::st_filter(y = sf::st_transform(aoi_sf,sf::st_crs(.)),
                  .predicate = st_is_within_distance,dist=units::set_units(buffer_dist_m, m)) %>%
    sf::st_drop_geometry() %>%
    pull(MonitoringLocationIdentifier) %>%
    unique()
  
  wqp_inventory_aoi <- wqp_inventory %>%
    filter(MonitoringLocationIdentifier %in% queried_sites_aoi)
  
  message(sprintf("Returned %s sites within area of interest.",
                  length(queried_sites_aoi)))
  
  return(wqp_inventory_aoi)
  
}


