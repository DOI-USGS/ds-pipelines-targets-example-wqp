#' Find sites with identifier names that are likely to cause problems
#' when downloading data from WQP by siteid
#' 
#' @details Some site identifiers contain characters that cannot be parsed 
#' by WQP, including "/". Identify and subset sites with potentially
#' problematic identifiers.
#' 
#' @param sitecounts_df data frame containing the site identifiers. Data frame must
#' at minimum contain column `MonitoringLocationIdentifier`.
#' 
identify_bad_ids <- function(sitecounts_df){
  
  # Check that string format matches regex used in WQP and doesn't contain "/"
  sitecounts_bad_ids <- sitecounts_df %>%
    rename(site_id = MonitoringLocationIdentifier) %>% 
    mutate(site_id_regex = stringr::str_extract(site_id, "[\\w]+\\-.*\\S")) %>%
    filter(site_id != site_id_regex | grepl("/", site_id)) %>%
    select(-site_id_regex)

  return(sitecounts_bad_ids)
}


#' Group sites for downloading data without hitting the WQP cap
#' 
#' @description This function returns a data frame with a single column used
#' to group the sites into reasonably sized chunks for downloading data.
#' 
#' @param sitecounts_df data frame containing the site identifiers and total number of
#' records available for each site. Must contain columns `MonitoringLocationIdentifier`
#' and `results_count`.
#' @param max_sites integer indicating the maximum number of sites allowed in each
#' download group. Defaults to 500.
#' @param max_results integer indicating the maximum number of records allowed in
#' each download group. Defaults to 250,000.
#' 
#' @return returns a data frame with columns site id, the total number of records,
#' (retains the column from `sitecounts_df`), site number, and an additional column 
#' called `download_grp` which is made up of unique groups that enable use of 
#' `group_by()` and then `tar_group()` for downloading.
#' 
add_download_groups <- function(sitecounts_df, max_sites = 500, max_results = 250000) {
  
  # Check whether any individual sites have a records count that exceeds `max_results`
  if(any(sitecounts_df$results_count > max_results)){
    sites_w_many_records <- sitecounts_df %>%
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
  sitecounts_bad_ids <- identify_bad_ids(sitecounts_df)
  
  # Inform user if we detect any sites with 'bad' identifiers
  if(nrow(sitecounts_bad_ids) > 0){
    message(sprintf(paste0("Some site identifiers contain undesired characters and cannot ",
                           "be parsed by WQP. Assigning %s sites and %s records with bad ",
                           "identifiers to their own download groups so that they can be ",
                           "queried separately using a different method."),
                    nrow(sitecounts_bad_ids), sum(sitecounts_bad_ids$results_count)))
  }
  
  # Subset 'good' sites with identifiers that can be parsed by WQP
  sitecounts_good_ids <- sitecounts_df %>%
    filter(!MonitoringLocationIdentifier %in% sitecounts_bad_ids$site_id)
  
  # Within each unique grid_id, use the cumsumbinning function from the MESS
  # package to group sites based on the cumulative sum of results_count 
  # across sites within the grid, resetting the download group/task number 
  # if the number of records exceeds the threshold set by `max_results`
  sitecounts_grouped_good_ids <- sitecounts_good_ids %>%
    rename(site_id = MonitoringLocationIdentifier) %>% 
    split(.$grid_id) %>%
    purrr::map_dfr(.f = function(df){
      
      df_grouped <- df %>%
        arrange(desc(results_count)) %>%
        mutate(task_num = MESS::cumsumbinning(x = results_count, 
                                              threshold = max_results, 
                                              maxgroupsize = max_sites),
               download_grp = paste0(grid_id,"_",task_num))
    }) %>%
    mutate(pull_by_id = TRUE)
  
  # Assign a separate task number and download group for each site with bad ids
  sitecounts_grouped_bad_ids <- sitecounts_bad_ids %>%
    group_by(site_id) %>%
    mutate(task_num = max(sitecounts_grouped_good_ids$task_num) + cur_group_id(),
           download_grp = paste0(grid_id,"_",task_num),
           pull_by_id = FALSE) %>%
    ungroup()
  
  # Combine all sites back together (now with assigned download_grp id's) and
  # format columns
  sitecounts_grouped_out <- sitecounts_grouped_good_ids %>%
    bind_rows(sitecounts_grouped_bad_ids) %>%
    arrange(download_grp) %>%
    mutate(site_n = row_number()) %>% 
    select(site_id, lat, lon, datum, results_count, site_n, download_grp, pull_by_id)
  
  return(sitecounts_grouped_out)

}



#' Function to return a small bounding box around site(s)
#' 
#' @details Some site identifiers contain undesired characters and cannot be
#' parsed by WQP. This function creates a small bounding box around the 
#' site(s) for the purposes of querying WQP by bBox rather than site id. 
#' 
#' @param sites data frame containing at minimum columns lat, lon, and datum
#' @param buffer_dist_degrees double; value indicating how large of a buffer
#' should be added to the site(s). Bounding box will be computed in WGS84 and
#' units are in degrees. Defaults to 0.005 degrees (~500 m).
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
  


#' Download data from the Water Quality Portal
#' 
#' @description Function to pull WQP data given a dataset of site ids
#'  
#' @param site_counts_grouped data frame containing site identifiers, the 
#' total number of records, site numbers, and a download group assigned
#' for each site. Must contain columns `site_id`, `site_n`, and `pull_by_id`.
#' `pull_by_id` is logical and indicates whether data should be downloaded
#' using site identifiers or by querying a small bounding box around the site.
#' @param characteristics vector of character strings indicating which WQP
#' CharacteristicName to query
#' @param wqp_args list containing additional arguments to pass to whatWQPdata(),
#' defaults to NULL. See https://www.waterqualitydata.us/webservices_documentation 
#' for more information.  
#' @param max_tries integer, maximum number of attempts if the data download 
#' step returns an error. Defaults to 3.
#' 
#' @return returns a data frame containing data downloaded from the Water Quality Portal, 
#' where each row represents a unique data record. 
#' 
fetch_wqp_data <- function(site_counts_grouped, characteristics, wqp_args = NULL, max_tries = 3){
  
  message(sprintf("Retrieving WQP data for sites %s:%s",
                  min(site_counts_grouped$site_n), 
                  max(site_counts_grouped$site_n)))
  
  # Define arguments for readWQPdata
  # sites with pull_by_id = FALSE cannot be queried by their site
  # identifiers because of undesired characters that will cause the WQP
  # query to fail. For those sites, query WQP by adding a small bounding
  # box around the site(s) and including bBox in the wqp_args.
  if(unique(site_counts_grouped$pull_by_id)){
    wqp_args_all <- c(wqp_args, 
                      list(siteid = site_counts_grouped$site_id,
                           characteristicName = c(characteristics)))
  } else {
    wqp_args_all <- c(wqp_args, 
                      list(bBox = create_site_bbox(site_counts_grouped),
                           characteristicName = c(characteristics)))
  }
  
  # Pull data, retrying up to the number of times indicated by `max_tries`
  wqp_data <- retry::retry(dataRetrieval::readWQPdata(wqp_args_all),
                           when = "Error:", 
                           max_tries = max_tries)
  
  # We applied special handling for sites with pull_by_id = FALSE (see comments
  # above). Filter wqp_data to only include sites requested in site_counts_grouped
  # in case our bounding box approach picked up any additional, undesired sites. 
  # In addition, some records return character strings when we expect numeric 
  # values, e.g. when "*Non-detect" appears in the "ResultMeasureValue" field. 
  # For now, consider all columns to be character so that individual data
  # frames returned from fetch_wqp_data can be joined together. 
  wqp_data_out <- wqp_data %>%
    filter(MonitoringLocationIdentifier %in% site_counts_grouped$site_id) %>%
    mutate(across(everything(), as.character))
  
  return(wqp_data_out)
}

