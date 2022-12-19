#' @title Find bad site identifiers
#' 
#' @description
#' Function to check sites for identifier names that are likely to cause 
#' problems when downloading data from WQP by siteid. Some site identifiers
#' contain characters that cannot be parsed by WQP, including "/". This function
#' identifies and subsets sites with potentially problematic identifiers.
#' 
#' @param sites data frame containing the site identifiers. Must contain
#' column `MonitoringLocationIdentifier`.
#' 
#' @returns 
#' Returns a data frame where each row represents a site with a problematic
#' identifier, indicated by the new column `site_id`. All other columns within
#' `sites` are retained. Returns an empty data frame if no problematic site
#' identifiers are found.
#' 
#' @examples 
#' siteids <- data.frame(MonitoringLocationIdentifier = 
#'                         c("USGS-01573482","COE/ISU-27630001"))
#' identify_bad_ids(siteids)
#' 
identify_bad_ids <- function(sites){
  
  # Check that string format matches regex used in WQP
  sites_bad_ids <- sites %>%
    rename(site_id = MonitoringLocationIdentifier) %>% 
    mutate(site_id_regex = stringr::str_extract(site_id, "[\\w]+.*[\\S]")) %>%
    filter(site_id != site_id_regex) %>%
    select(-site_id_regex)
  
  return(sites_bad_ids)
}


#' @title Group sites for downloading data without hitting the WQP cap
#' 
#' @description 
#' Function to group inventoried sites into reasonably sized chunks for
#' downloading data.
#' 
#' @param site_counts data frame containing the site identifiers and total 
#' number of records available for each site. Must contain columns 
#' `MonitoringLocationIdentifier` and `results_count`.
#' @param max_sites integer indicating the maximum number of sites allowed in
#' each download group. Defaults to 500.
#' @param max_results integer indicating the maximum number of records allowed
#' in each download group. Defaults to 250,000.
#' 
#' @returns 
#' Returns a data frame with columns site id, the total number of records,
#' (retains the column from `site_counts`), site number, and an additional column 
#' called `download_grp` which is made up of unique groups that enable use of 
#' `group_by()` and then `tar_group()` for downloading.
#' 
add_download_groups <- function(site_counts, max_sites = 500, max_results = 250000) {
  
  # Check whether any individual sites have a records count that exceeds `max_results`
  if(any(site_counts$results_count > max_results)){
    sites_w_many_records <- site_counts %>%
      filter(results_count > max_results) %>%
      pull(MonitoringLocationIdentifier)
    # Print a message to inform the user that some sites contain a lot of data
    message(sprintf(paste0("results_count exceeds max_results for the sites below. ",
                           "Assigning data-heavy sites to their own download group to ",
                           "set up a manageable query to WQP. If you are not already ",
                           "branching across characteristic names, consider doing so to ",
                           "further limit query size. \n\n%s\n"),
                    paste(sites_w_many_records, collapse="\n")))
  }
  
  # Check whether any sites have identifiers that are likely to cause problems when
  # downloading the data from WQP
  sitecounts_bad_ids <- identify_bad_ids(site_counts)
  
  # Inform user if we detect any sites with 'bad' identifiers
  if(nrow(sitecounts_bad_ids) > 0){
    message(sprintf(paste0("Some site identifiers contain undesired characters and cannot ",
                           "be parsed by WQP. Assigning %s sites and %s records with bad ",
                           "identifiers to their own download groups so that they can be ",
                           "queried separately using a different method."),
                    nrow(sitecounts_bad_ids), sum(sitecounts_bad_ids$results_count)))
  }
  
  # Subset 'good' sites with identifiers that can be parsed by WQP
  sitecounts_good_ids <- site_counts %>%
    filter(!MonitoringLocationIdentifier %in% sitecounts_bad_ids$site_id)
  
  # Within each unique grid_id, use the cumsumbinning function from the MESS package
  # to group sites based on the cumulative sum of results_count across sites that 
  # share the same characteristic name, resetting the download group/task number if 
  # the number of records exceeds the threshold set by `max_results`.
  sitecounts_grouped_good_ids <- sitecounts_good_ids %>%
    rename(site_id = MonitoringLocationIdentifier) %>% 
    split(.$grid_id) %>%
    purrr::map_dfr(.f = function(df){
      
      df_grouped <- df %>%
        group_by(CharacteristicName) %>%
        arrange(desc(results_count), .by_group = TRUE) %>%
        mutate(task_num_by_results = MESS::cumsumbinning(x = results_count, 
                                                         threshold = max_results, 
                                                         maxgroupsize = max_sites), 
               char_group = cur_group_id()) %>%
        ungroup() %>% 
        # Each group from before (which represents a different characteristic 
        # name) will have task numbers that start with "1", so now we create 
        # a new column called `task_num` to create unique task numbers within
        # each grid. For example, both "Specific conductance" and "Temperature" 
        # may have values for `task_num_by_results` of 1 and 2 but the values 
        # of char_group (1 and 2, respectively) would mean that they have unique
        # values for `task_num` equaling 1, 2, 3, and 4.
        group_by(char_group, task_num_by_results) %>% 
        mutate(task_num = cur_group_id()) %>% 
        ungroup() %>%
        mutate(pull_by_id = TRUE)
    }) 
  
  # Assign a separate task number and download group for each site with bad ids
  sitecounts_grouped_bad_ids <- sitecounts_bad_ids %>%
    group_by(site_id) %>%
    mutate(task_num = max(sitecounts_grouped_good_ids$task_num) + cur_group_id(),
           pull_by_id = FALSE) %>%
    ungroup()
  
  # Combine all sites back together (now with assigned download_grp id's) and
  # format columns
  sitecounts_grouped_out <- sitecounts_grouped_good_ids %>%
    bind_rows(sitecounts_grouped_bad_ids) %>%
    # Ensure the groups are ordered correctly by prepending a dynamic number of 0s
    # before the task number based on the maximum number of tasks.
    mutate(download_grp = sprintf(paste0("%s_%0", nchar(max(task_num)), "d"), 
                                  grid_id, task_num)) %>% 
    arrange(download_grp) %>%
    select(site_id, lat, lon, datum, grid_id, CharacteristicName, results_count, 
           download_grp, pull_by_id) 
  
  return(sitecounts_grouped_out)
  
}



#' @title Return a small bounding box around site(s)
#' 
#' @description 
#' Some site identifiers contain undesired characters and cannot be
#' parsed by WQP. This function creates a small bounding box around the 
#' site(s) for the purposes of querying WQP by bBox rather than site id. 
#' 
#' @param sites data frame containing at minimum columns lat, lon, and datum.
#' @param buffer_dist_degrees double; value indicating how large of a buffer
#' should be added to the site(s). Bounding box will be computed in WGS84 and
#' units are in degrees. Defaults to 0.005 degrees (~500 m).
#' 
#' @returns
#' Returns an object of class "bbox" that represents the bounding box around
#' the point locations included in `sites`. Contains vectors "xmin", "ymin",
#' "xmax", and "ymax". 
#' 
#' @examples 
#' sites <- data.frame(lon = c(-74.62238, -74.94351, -75.25741), 
#'                     lat = c(39.79456, 39.31011, 39.52289),
#'                     datum = rep('NAD83', 3))
#' create_site_bbox(sites)
#' 
create_site_bbox <- function(sites, buffer_dist_degrees = 0.005){
  
  # assume unknown crs (datum equals "UNKNWN", "OTHER") correspond with WGS84
  epsg_in <- case_when(unique(sites$datum) == "NAD83" ~ 4269,
                       unique(sites$datum) == "WGS84" ~ 4326,
                       unique(sites$datum) == "NAD27" ~ 4267,
                       unique(sites$datum) == "UNKWN" ~ 4326,
                       unique(sites$datum) == "OTHER" ~ 4326)
  
  # create a small buffer around the site(s) and then compute the bounding box
  site_bbox <- sites %>%
    sf::st_as_sf(coords = c("lon","lat"), crs = epsg_in) %>%
    sf::st_transform(4326) %>%
    sf::st_buffer(buffer_dist_degrees) %>%
    sf::st_bbox()
  
  return(site_bbox)
}

