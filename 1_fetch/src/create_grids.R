create_conus_grid <- function(cellsize, year = 2020, progress_bar = FALSE){
  #' 
  #' @description Function to create a big grid of boxes to use for chunked
  #' data queries
  #' 
  #' @param cellsize the target cell size of each box in map units (here, degrees);
  #' see ??sf::st_make_grid for further details.
  #' @param year integer; year of the state polygon data download (defaults to 2020); 
  #' see ??tigris::states for further details.
  #' @param progress_bar logical; indicates whether a progress bar showing the status
  #' of the state polygon build should be returned (defaults to FALSE)
  #' 
  #' @value returns an sf polygon object containing the geometries and an attribute 
  #' id for each box within the CONUS grid.
  #' 
  #' @example create_conus_grid(cellsize = c(1,1))
  #'  
  
  conus_grid <- tigris::states(class = "sf", year = year, progress_bar = progress_bar) %>%
    # filter state polygons to CONUS extent
    filter(REGION != "9", 
           !STUSPS %in% c("HI","AK")) %>%
    # create square grid with cell sizes equal to 1 deg. x 1 deg.
    sf::st_make_grid(cellsize = c(cellsize[1],cellsize[2]), square = TRUE) %>%
    # convert to sf object and add an "id" attribute
    sf::st_as_sf() %>%
    mutate(id = row.names(.))
  
  return(conus_grid)
  
}



subset_grids_to_aoi <- function(grid, aoi_poly, buffer_dist_m){
  #' 
  #' @description Function to spatially subset a holistic grid of boxes
  #' to find the boxes that overlap the area of interest. The area of 
  #' interest is buffered to ensure that all overlapping boxes are returned.
  #' 
  #' @param grid sf polygon object containing the geometries and an attribute 
  #' id for each box within the grid.
  #' @param aoi_poly sf polygon object representing the area of interest
  #' @param buffer_dist_m integer; indicates the distance in meters to buffer 
  #' around the area of interest polygon. See ??sf::st_buffer for further details.
  #' 
  #' @value returns an sf polygon object containing the geometries for each box 
  #' within the holistic grid that overlaps the buffered area of interest.
  #' 
  #' @example subset_grids_to_aoi(grid = p1_conus_grid, buffer_dist_m = 5000)
  #' 
  
  # Project area of interest to calculate buffer and set up for intersection
  buffered_aoi <- aoi_poly %>%
    sf::st_transform(5070) %>%
    sf::st_buffer(5000)
  
  # Filter the big grid of boxes to only include those that overlap
  # with the buffered area of interest
  grid_subset_aoi <- grid %>%
    sf::st_transform(5070) %>%
    sf::st_filter(y = buffered_aoi,
                  .predicate = st_intersects)  
  
  return(grid_subset_aoi)
  
}

