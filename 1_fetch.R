source('scratch/download_file.R')

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
  # read_csv("1_fetch/in/my_sites.csv")
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
  )

)
