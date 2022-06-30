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
    sf::st_as_sf(p1_AOI, coords = c("lon","lat"), crs = 4326) %>%
      summarize(geometry = st_combine(geometry)) %>%
      sf::st_cast("POLYGON")
  ),
  
  # Create a big grid of boxes to set up chunked data queries.
  # The resulting grid, which covers the globe, allows for queries
  # outside of CONUS, including AK, HI, and US territories. 
  tar_target(
    p1_global_grid,
    create_global_grid()
  ),
  
  # Use spatial subsetting to find boxes that overlap the area of interest
  # (i.e., are within dist_m of p1_AOI_sf). These boxes will be used to
  # query the WQP.
  tar_target(
    p1_global_grid_aoi,
    subset_grids_to_aoi(p1_global_grid, p1_AOI_sf)
  ),
  
  # Inventory data available from the WQP within each of the boxes that overlap
  # the area of interest. To prevent timeout issues that result from large data 
  # requests, use {targets} dynamic branching capabilities to map the function 
  # inventory_wqp() over each grid within p1_global_grid_aoi. {targets} will then
  # combine all of the grid-scale inventories into one table. See comments below
  # associated with target p1_wqp_data_aoi regarding the use of error = 'continue'.
  tar_target(
    p1_wqp_inventory,
    # inventory_wqp() requires grid and char_names as inputs, but users can 
    # also pass additional arguments to WQP, e.g. sampleMedia or siteType, using 
    # wqp_args. Below, wqp_args is defined in _targets.R. See documentation
    # in 1_fetch/src/get_wqp_inventory.R for further details.
    inventory_wqp(grid = p1_global_grid_aoi,
                  char_names = p1_char_names,
                  wqp_args = wqp_args),
    pattern = cross(p1_global_grid_aoi, p1_char_names),
    error = "continue"
  ),
  
  # Subset the WQP inventory to only retain sites within the area of interest
  tar_target(
    p1_wqp_inventory_aoi,
    subset_inventory(p1_wqp_inventory, p1_AOI_sf)
  ),
  
  # Summarize the data that would come back from the WQP
  tar_target(
    p1_wqp_inventory_summary_csv,
    summarize_wqp_inventory(p1_wqp_inventory_aoi, "1_fetch/log/summary_wqp_inventory.csv"),
    format = "file"
  ),
  
  # Pull site id's and total number of records for each site from the WQP inventory
  tar_target(
    p1_site_counts,
    p1_wqp_inventory_aoi %>%
      group_by(MonitoringLocationIdentifier) %>%
      summarize(results_count = sum(resultCount, na.rm = TRUE),
                grid_id = unique(grid_id))
  ),
  
  # Group the sites into reasonably sized chunks for downloading data 
  tar_target(
    p1_site_counts_grouped,
    add_download_groups(p1_site_counts, 
                        max_sites = 500,
                        max_results = 250000) %>%
      group_by(download_grp) %>%
      tar_group(),
    iteration = "group"
  ),

  # Map over groups of sites to download data.
  # Note that because error = 'continue', {targets} will attempt to build all 
  # of the "branches" represented in p1_site_counts_grouped, even if one branch 
  # returns an error. This way, we will not need to re-build branches that have
  # already run successfully. However, if a branch fails, {targets} will throw
  # an error reading `could not load dependencies of [immediate downstream target]. invalid 
  # 'description' argument` because it cannot merge the individual branches and so did not  
  # complete the branching target. The error(s) associated with the failed branch will therefore 
  # need to be resolved before the full target can be successfully built. A 
  # common reason a branch may fail is due to WQP timeout errors. Timeout errors 
  # can sometimes be resolved by waiting a few hours and retrying tar_make().
  tar_target(
    p1_wqp_data_aoi,
    fetch_wqp_data(p1_site_counts_grouped, p1_char_names, wqp_args = wqp_args),
    pattern = map(p1_site_counts_grouped),
    error = "continue"
  ),
  
  # Summarize the data downloaded from the WQP
  tar_target(
    p1_wqp_data_summary_csv,
    summarize_wqp_data(p1_wqp_inventory_summary_csv, p1_wqp_data_aoi, 
                       "1_fetch/log/summary_wqp_data.csv"),
    format = "file"
  )

)
