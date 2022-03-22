# Source the functions that will be used to build the targets in p1_targets_list
source("1_fetch/src/create_conus_grid.R")

p1_targets_list <- list(
  
  # Get common parameter groups and WQP CharacteristicNames
  tar_target(
    p1_wqp_params_yml,
    '1_fetch/cfg/wqp_codes.yml',
    format = "file"
  ),
  
  tar_target(
    p1_wqp_params,
    yaml::read_yaml(p1_wqp_params_yml) 
  ),
  
  # Get a vector of WQP characteristicNames to match parameter groups of interest
  tar_target(
    p1_charNames,
    p1_wqp_params[param_groups_select]
  ),
  
  # Define the spatial area of interest (AOI) for the WQP data pull
  # This target could also be edited to read in coordinates from a local file
  # that contains the columns 'lon' and 'lat', e.g. replace data.frame() with 
  # read_csv("1_fetch/in/my_sites.csv"). See README for an example of how to 
  # use a shapefile to define the AOI.
  tar_target(
    p1_AOI,
    data.frame(lon = coords_lon,
               lat = coords_lat)
  ),
  
  # Create a spatial (sf) object representing the area of interest
  tar_target(
    p1_AOI_sf,
    sf::st_as_sf(p1_AOI,coords=c("lon","lat"),crs=4326) %>%
      summarize(geometry = st_combine(geometry)) %>%
      sf::st_cast("POLYGON")
  ),
  
  # Create a big grid of boxes to set up chunked data queries
  tar_target(
    p1_conus_grid,
    create_conus_grid(cellsize = c(1,1))
  ),
  
  # Use spatial subsetting to find boxes that overlap the area of interest
  # (buffered here using a 5 km buffer distance). These boxes will be used
  # to query the WQP.
  tar_target(
    p1_conus_grid_aoi,
    {
      # Project area of interest to calculate buffer and set up for intersection
      buffered_AOI <- p1_AOI_sf %>%
        sf::st_transform(5070) %>%
        sf::st_buffer(5000)
      
      # Filter the big grid of boxes to only include those that overlap
      # with the buffered area of interest
      p1_conus_grid %>%
        sf::st_transform(5070) %>%
        sf::st_filter(y = buffered_AOI,
                      .predicate = st_intersects)
    }
  )

)
