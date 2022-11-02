#' @title Inventory available data from the Water Quality Portal (WQP)
#' 
#' @description 
#' Function to inventory WQP sites and records within grid cells that overlap
#' the area of interest.
#'  
#' @param grid sf object representing the area over which to query the WQP.
#' @param char_names character string or list of character strings indicating 
#' which WQP characteristic names to query.
#' @param wqp_args list containing additional arguments to pass to whatWQPdata(),
#' defaults to NULL. See https://www.waterqualitydata.us/webservices_documentation 
#' for more information.  
#' @param max_tries integer, maximum number of attempts if the data download 
#' step returns an error. Defaults to 3.
#' 
#' @returns 
#' Returns a data frame with a row for each site within the Water Quality 
#' Portal. Columns contain site information and record counts.
#' 
#' @examples 
#' aoi <- sf::st_as_sf(data.frame(lon = c(-77.063, -75.333, -75.437), 
#'                                     lat = c(40.547, 41.029, 39.880)), 
#'                          coords = c("lon", "lat"), crs = 4326) 
#' inventory_wqp(aoi, "Conductivity", wqp_args = list(siteType = "Stream"))
#' inventory_wqp(aoi, "Temperature, water", 
#'               wqp_args = list(siteType = "Lake, Reservoir, Impoundment"))
#'
# explicitly load and attach sf package to handle geometry data in `grid`
library(sf)

inventory_wqp <- function(grid, char_names, wqp_args = NULL, max_tries = 3){
  
  # First, check dataRetrieval package version and inform user if outdated
  if(packageVersion('dataRetrieval') < "2.7.6.9003"){
    stop(sprintf(paste0("dataRetrieval version %s is installed but this pipeline ",
                        "requires package 2.7.6.9003. Please update dataRetrieval."),
                 packageVersion('dataRetrieval')))
  }
  
  # Get bounding box for the grid polygon
  bbox <- sf::st_bbox(grid)
  
  # Format characteristic names
  char_names <- as.character(unlist(char_names))
  
  # Print time-specific message so user can see progress
  message(sprintf('Inventorying WQP data for grid %s, %s', 
                  grid$id, char_names))

  # Inventory available WQP data
  wqp_inventory <- lapply(char_names,function(x){
    # define arguments for whatWQPdata
    wqp_args_all <- c(wqp_args, list(bBox = c(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax),
                                     characteristicName = x))
    # query WQP
    retry::retry(dataRetrieval::whatWQPdata(wqp_args_all),
                 when = "Error:", 
                 max_tries = max_tries) %>%
      mutate(CharacteristicName = x, grid_id = grid$id)
  }) %>%
    bind_rows()
  
  # Fetch missing CRS information from WQP. Note that an empty query will not
  # contain CRS information. In the event that the lines below throw an error, 
  # "Column `HorizontalCoordinateReferenceSystemDatumName` doesn't exist", return
  # an empty data frame for the site location metadata. 
  site_location_metadata <- tryCatch(
    dataRetrieval::whatWQPsites(bBox = c(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax)) %>%
      select(MonitoringLocationIdentifier, HorizontalCoordinateReferenceSystemDatumName) %>%
      filter(MonitoringLocationIdentifier %in% wqp_inventory$MonitoringLocationIdentifier),
    error = function(e){
      data.frame(MonitoringLocationIdentifier = character(), 
                 HorizontalCoordinateReferenceSystemDatumName = character())
    }
  )

  # Join WQP inventory with site metadata 
  wqp_inventory_out <- wqp_inventory %>%
    left_join(site_location_metadata, by = "MonitoringLocationIdentifier")

  return(wqp_inventory_out)
  
}



#' @title Transform site coordinates
#' 
#' @description
#' Function to transform WQP sites to use a consistent coordinate reference system.
#'
#' @param sites data frame containing sites returned from the WQP query; must
#' contain columns lon, lat, and HorizontalCoordinateReferenceSystemDatumName.
#' @param crs_out character string indicating the desired output coordinate 
#' reference system (crs). Options include "NAD83" or "WGS84", defaults to "WGS84".
#' 
#' @returns 
#' Returns a data frame containing the transformed site coordinates in columns
#' "lon" and "lat" and the new, harmonized crs is indicated by the column "datum". 
#' All other columns in `sites` are retained. 
#'
#' @examples
#' sites <- data.frame(lon = c(-74.62238, -74.94351, -75.25741), 
#'                     lat = c(39.79456, 39.31011, 39.52289),
#'                     HorizontalCoordinateReferenceSystemDatumName = rep('NAD83', 3))
#' transform_site_locations(sites)
#' transform_site_locations(sites, "NAD83")
#' 
transform_site_locations <- function(sites, crs_out = "WGS84"){
  
  message("Attempting to harmonize different site CRS...")
  
  # Check that crs_out is one of two allowable entries
  if(!crs_out %in% c("WGS84","NAD83"))
    stop("crs_out must be either 'WGS84' or 'NAD83'")
  
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
          mutate(lon_new = as.numeric(sf::st_coordinates(.)[,1]),
                 lat_new = as.numeric(sf::st_coordinates(.)[,2]),
                 datum_new = crs_out) %>%
          sf::st_drop_geometry() %>%
          select(-datum) %>%
          rename("lon" = "lon_new", "lat" = "lat_new", "datum" = "datum_new")
      } else {
        sites_transformed <- x
        message(sprintf("Unable to transform sites with datum = %s; returning sites untransformed",
                        unique(x$datum)))
      }
      return(sites_transformed)
    }) %>%
    # combine transformed sites into a single data frame
    bind_rows()
  
  return(sites_transformed)
  
}



#' @title Subset WQP inventory to the area of interest
#' 
#' @description 
#' Function to filter out any sites in the WQP inventory that are
#' outside of the area of interest polygon. 
#' 
#' @param wqp_inventory data frame containing sites returned from the WQP query; 
#' must contain columns lon, lat, and HorizontalCoordinateReferenceSystemDatumName.
#' @param aoi_sf sf polygon object representing the area of interest.
#' @param buffer_dist_m integer reflecting a desired buffer distance (in meters)
#' around the area of interest. If a site is within `buffer_dist_m` of `aoi_sf`, 
#' retain that site. Defaults to 0 meters. 
#' 
#' @returns 
#' Returns a data frame with a row for each site in the Water Quality Portal
#' that overlaps the area of interest. Columns contain site information and 
#' record counts. 
#' 
subset_inventory <- function(wqp_inventory, aoi_sf, buffer_dist_m = 0){
  
  # Harmonize different coordinate reference systems used across sites
  queried_sites_transformed <- transform_site_locations(wqp_inventory, crs_out= "WGS84") 
  queried_sites_transformed_sf <- sf::st_as_sf(queried_sites_transformed, 
                                               coords = c("lon","lat"), crs = 4326) 
  
  # Filter wqp inventory to only include sites within area of interest + some user-specified 
  # buffer distance to ensure we retain all sites within the AOI. 
  # Note that the workflow below is similar to one where we would draw a buffer around the 
  # AOI and then use sf::st_intersects to find the points that intersect the AOI polygon. 
  # st_is_within_distance is used here instead because buffers created using the s2 engine
  # are rough and can be glitchy when working with geographic coordinates. See this blog
  # post by the sf maintainers: https://r-spatial.github.io/sf/articles/sf7.html#buffers-1
  queried_sites_aoi <- queried_sites_transformed_sf %>%
    sf::st_filter(y = sf::st_transform(aoi_sf, sf::st_crs(.)),
                  .predicate = sf::st_is_within_distance,
                  dist = units::set_units(buffer_dist_m, m)) %>%
    mutate(lon = as.numeric(sf::st_coordinates(.)[,1]),
           lat = as.numeric(sf::st_coordinates(.)[,2])) %>%
    sf::st_drop_geometry() %>%
    select(c(any_of(names(wqp_inventory)), datum))
  
  message(sprintf("Returned %s sites within area of interest.",
                  length(queried_sites_aoi$MonitoringLocationIdentifier)))
  
  return(queried_sites_aoi)
  
}

