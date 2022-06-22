#' Create a nationwide grid including AK/HI and territories
#'  
#' @description Function to create a big grid of boxes to use for chunked
#' data queries
#' 
#' @param cellsize the target cell size of each box in map units (here, degrees);
#' takes two values indicating the size in the x direction and the size in the 
#' y direction. Defaults to 2 degree cell size. See ??sf::st_make_grid for 
#' further details.
#' @param year integer; year of the state polygon data download (defaults to 2020); 
#' see ??tigris::states for further details.
#' @param progress_bar logical; indicates whether a progress bar showing the status
#' of the state polygon build should be returned (defaults to FALSE)
#' 
#' @value returns an sf polygon object containing the geometries and an attribute 
#' id for each box within the nationwide grid.
#' 
#' @example create_nationwide_grid(cellsize = c(2,2))
#' 

create_nationwide_grid <- function(cellsize = c(2,2), year = 2020, progress_bar = FALSE){
 
  usa_sf <- tigris::states(class = "sf", year = year, progress_bar = progress_bar)
  
  # create square grid with cell sizes equal to `cellsize` passed in degrees.
  usa_grid <- usa_sf %>%
    sf::st_make_grid(cellsize = c(cellsize[1],cellsize[2]), square = TRUE, 
                     # Start lower left of grid right at 180th meridian to avoid
                     # issues with WQP query bboxes crossing from E longitudes to W
                     # See https://github.com/USGS-R/dataRetrieval/issues/616
                     offset = c(-180, sf::st_bbox(usa_sf)$ymin)) %>%
    # convert to sf object and add an "id" attribute
    sf::st_as_sf() %>%
    mutate(id = row.names(.))
  
  # Check that usa_grid produces a valid bounding box to pass to WQP. Check that
  # the west-east coordinates don't cross 180. Note that this behavior could 
  # result from picking a cellsize[1] that 180 is not evenly divisible by (like 7)
  # because it results in a grid cell crossing the 180th meridian.
  grid_xmaxes <- purrr::map(usa_grid$x, function(grid) st_bbox(grid)$xmax)
  if(any(unlist(grid_xmaxes) > 180)) {
    stop("Grid cells cross over 180 deg meridian and will cause WQP 
    calls to fail. Change `cellsize[1]` so that 180 is divisible 
    by it or make adjustments to `create_national_grid()`")
  }
  
  return(usa_grid)
  
}



#' Subset grid to AOI
#' 
#' @description Function to spatially subset a holistic grid of boxes
#' to find the boxes that overlap the area of interest. The area of 
#' interest is buffered to ensure that all overlapping boxes are returned.
#' 
#' @param grid sf polygon object containing the geometries and an attribute 
#' id for each box within the grid.
#' @param aoi_poly sf polygon object representing the area of interest
#' @param dist_m integer; grid geometries will be returned if distances between
#' the grid polygons and the aoi polygon are smaller or equal to this value.
#' 
#' @value returns an sf polygon object containing the geometries for each box 
#' within the holistic grid that overlaps the buffered area of interest.
#' 
#' @example subset_grids_to_aoi(grid = p1_nationwide_grid, dist_m = 5000)
#' 

subset_grids_to_aoi <- function(grid, aoi_poly, dist_m){

  
  # Filter the big grid of boxes to only include those that overlap/are within
  # a given distance of the area of interest
  grid_subset_aoi <- grid %>%
    sf::st_filter(y = sf::st_transform(aoi_poly,sf::st_crs(grid)),
                  .predicate = st_is_within_distance,dist=units::set_units(dist_m, m))   
  
  return(grid_subset_aoi)
  
}



#' Return bounding box as a vector
#' 
#' @description Function to find the bounding box for an sf object(s) and return
#' as a vector
#' 
#' @param grid sf polygon object containing the geometries and an attribute
#' id for each box within the grid
#' 

return_bbox <- function(grid){
  
  bbox <- lapply(sf::st_geometry(grid), sf::st_bbox) %>% 
    setNames(grid$id)
  
  return(bbox)
  
}


