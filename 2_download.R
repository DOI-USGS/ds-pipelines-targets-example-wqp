# Source the functions that will be used to build the targets in p2_targets_list
source("2_download/src/fetch_wqp_data.R")
source("2_download/src/summarize_wqp_download.R")

p2_targets_list <- list(
  
  # Pull site id's and total number of records for each site from the WQP inventory
  tar_target(
    p2_site_counts,
    p1_wqp_inventory_aoi %>%
      group_by(MonitoringLocationIdentifier, lon, lat, datum, grid_id, CharacteristicName) %>%
      summarize(results_count = sum(resultCount, na.rm = TRUE),
                .groups = 'drop')
  ),
  
  # Group the sites into reasonably sized chunks for downloading data 
  tar_target(
    p2_site_counts_grouped,
    add_download_groups(p2_site_counts, 
                        max_sites = 500,
                        max_results = 250000) %>%
      group_by(download_grp) %>%
      tar_group(),
    iteration = "group"
  ),
  
  # Map over groups of sites to download data.
  # Note that because error = 'continue', {targets} will attempt to build all 
  # of the "branches" represented by each unique combination of characteristic 
  # name and download group, even if one branch returns an error. This way, 
  # we will not need to re-build branches that have already run successfully. 
  # However, if a branch fails, {targets} will throw an error reading `could
  # not load dependencies of [immediate downstream target]. invalid 'description'
  # argument` because it cannot merge the individual branches and so did not  
  # complete the branching target. The error(s) associated with the failed branch 
  # will therefore need to be resolved before the full target can be successfully 
  # built. A common reason a branch may fail is due to WQP timeout errors. Timeout 
  # errors can sometimes be resolved by waiting a few hours and retrying tar_make().
  tar_target(
    p2_wqp_data_aoi,
    fetch_wqp_data(p2_site_counts_grouped, 
                   char_names = unique(p2_site_counts_grouped$CharacteristicName), 
                   wqp_args = wqp_args),
    pattern = map(p2_site_counts_grouped),
    error = "continue"
  ),
  
  # Summarize the data downloaded from the WQP
  tar_target(
    p2_wqp_data_summary_csv,
    summarize_wqp_download(p1_wqp_inventory_summary_csv, p2_wqp_data_aoi, 
                       "2_download/log/summary_wqp_data.csv"),
    format = "file"
  )

)
