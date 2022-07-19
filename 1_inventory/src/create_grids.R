#' @title Create a global grid of boxes
#'  
#' @description 
#' Function to create a big grid of boxes to use for chunked data queries.
#' 
#' @param cellsize the target cell size of each box in map units (here, degrees);
#' takes two values indicating the size in the x direction and the size in the 
#' y direction. Defaults to 2 degree cell size. See ??sf::st_make_grid for 
#' further details.
#' 
#' @returns 
#' Returns an sf polygon object containing the geometries and an attribute 
#' id for each box within the global grid.
#' 
#' @examples 
#' create_global_grid(cellsize = c(1,1))
#' 
create_global_grid <- function(cellsize = c(2,2)){
 
  global_box <- sf::st_bbox(c(xmin = -180, xmax = 180,
                              ymax = 90, ymin = -90), 
                            crs = sf::st_crs(4326))
  
  # create square grid with cell sizes equal to `cellsize` passed in degrees.
  global_grid <- global_box %>%
    sf::st_make_grid(cellsize = c(cellsize[1],cellsize[2]), square = TRUE, 
                     # Start lower left of grid right at 180th meridian to avoid
                     # issues with WQP query bboxes crossing from E longitudes to W
                     # See https://github.com/USGS-R/dataRetrieval/issues/616
                     offset = c(-180, sf::st_bbox(global_box)$ymin)) %>%
    # convert to sf object and add an "id" attribute
    sf::st_as_sf() %>%
    mutate(id = row.names(.))
  
  # Check that global_grid produces a valid bounding box to pass to WQP. Check that
  # the west-east coordinates don't cross 180. Note that this behavior could 
  # result from picking a cellsize[1] that 180 is not evenly divisible by (like 7)
  # because it results in a grid cell crossing the 180th meridian.
  grid_xmaxes <- purrr::map(global_grid$x, function(grid) st_bbox(grid)$xmax)
  if(any(unlist(grid_xmaxes) > 180)) {
    stop("Grid cells cross over 180 deg meridian and will cause WQP 
    calls to fail. Change `cellsize[1]` so that 180 is divisible 
    by it or make adjustments to `create_global_grid()`")
  }
  
  return(global_grid)
  
}



#' @title Subset grid cells to an area of interest
#' 
#' @description 
#' Function to spatially subset a holistic grid of boxes to find the
#' grid cells that overlap the area of interest. 
#' 
#' @param grid sf polygon object containing the geometries and an attribute 
#' id for each box within the grid.
#' @param aoi_sf sf polygon object representing the area of interest.
#' @param buffer_dist_m integer; grid geometries will be returned if the
#' distance between the grid polygon and the aoi polygon is smaller or equal
#' to this value. Defaults to 0 meters, although users may increase the buffer 
#' distance.
#' 
#' @returns 
#' Returns an sf polygon object containing the geometries for each grid cell 
#' within the holistic grid that overlaps the buffered area of interest.
#' 
#' @examples 
#' tar_load(p1_global_grid)
#' tar_load(p1_AOI_sf)
#' subset_grids_to_aoi(p1_global_grid, p1_AOI_sf, buffer_dist_m = 5000)
#' 
subset_grids_to_aoi <- function(grid, aoi_sf, buffer_dist_m = 0){

  
  # Filter the big grid of boxes to only include those that overlap/are within
  # a given distance of the area of interest
  grid_subset_aoi <- grid %>%
    sf::st_filter(y = sf::st_transform(aoi_sf, sf::st_crs(grid)),
                  .predicate = st_is_within_distance,
                  dist = units::set_units(buffer_dist_m, m))   
  
  return(grid_subset_aoi)
  
}

