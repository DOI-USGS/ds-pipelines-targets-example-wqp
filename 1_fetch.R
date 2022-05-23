# Source the functions that will be used to build the targets in p1_targets_list
source("1_fetch/src/check_characteristics.R")
source("1_fetch/src/create_grids.R")
source("1_fetch/src/get_wqp_inventory.R")
source("1_fetch/src/fetch_wqp_data.R")
source("1_fetch/src/summarize_wqp_records.R")

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
    p1_char_names,
    filter_characteristics(p1_wqp_params, param_groups_select)
  ),
  
  # Save log file containing WQP characteristic names that are similar to the
  # parameter groups of interest
  tar_target(
    p1_similar_char_names_txt,
    find_similar_characteristics(p1_char_names, param_groups_select, "1_fetch/out"),
    format = "file"
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
  # (i.e., are within dist_m of p1_AOI_sf). These boxes will be used to
  # query the WQP.
  tar_target(
    p1_conus_grid_aoi,
    subset_grids_to_aoi(p1_conus_grid, p1_AOI_sf, dist_m = 5000)
  ),
  
  # Inventory data available from the WQP within each of the boxes that overlap
  # the area of interest. To prevent timeout issues that result from large data 
  # requests, use {targets} dynamic branching capabilities to map the function 
  # inventory_wqp() over each grid within p1_conus_grid_aoi. {targets} will 
  # then combine all of the grid-scale inventories into one table.
  tar_target(
    p1_wqp_inventory,
    # inventory_wqp() requires grid and char_names as inputs, but users can 
    # also pass additional arguments to WQP, e.g. sampleMedia or siteType, using 
    # wqp_args. Below, wqp_args is defined in _targets.R. See documentation
    # in 1_fetch/src/get_wqp_inventory.R for further details.
    inventory_wqp(grid = p1_conus_grid_aoi,
                  char_names = p1_char_names,
                  wqp_args = wqp_args),
    pattern = map(p1_conus_grid_aoi)
  ),
  
  # Subset the WQP inventory to only retain sites within the area of interest
  tar_target(
    p1_wqp_inventory_aoi,
    subset_inventory(p1_wqp_inventory, p1_AOI_sf, buffer_dist_m = 100)
  ),
  
  # Summarize the data that would come back from the WQP
  tar_target(
    p1_wqp_inventory_summary,
    summarize_wqp_inventory(p1_wqp_inventory_aoi, "1_fetch/out/summary_wqp_inventory.csv"),
    format = "file"
  ),
  
  # Pull site id's from the WQP inventory
  tar_target(
    p1_site_ids,
    p1_wqp_inventory_aoi %>%
      pull(MonitoringLocationIdentifier) %>%
      unique()
  ),
  
  # Group the sites into reasonably sized chunks for downloading data 
  tar_target(
    p1_site_ids_grouped,
    add_download_groups(p1_site_ids, max_sites_allowed = 500) %>%
      group_by(download_grp) %>%
      tar_group(),
    iteration = "group"
  ),

  # Map over groups of sites to download data   
  tar_target(
    p1_wqp_data_aoi,
    fetch_wqp_data(p1_site_ids_grouped, p1_char_names, wqp_args = wqp_args),
    pattern = map(p1_site_ids_grouped)
  ),
  
  # Summarize the data downloaded from the WQP
  tar_target(
    p1_wqp_data_summary,
    summarize_wqp_data(p1_wqp_inventory_summary, p1_wqp_data_aoi, 
                       "1_fetch/out/summary_wqp_data.csv"),
    format = "file"
  )

)
