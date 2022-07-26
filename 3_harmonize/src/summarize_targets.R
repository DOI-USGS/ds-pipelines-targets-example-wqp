#' @title Summarize pipeline metadata
#' 
#' @description 
#' Function to summarize and save pipeline metadata
#'
#' @param names character string indicating which targets to return pipeline
#' metadata for. Defaults to NULL, which returns all targets.
#' @param fileout character string indicating the name of the saved file, 
#' including file path and extension.
#' 
#' @returns 
#' Returns a csv file containing pipeline metadata, with a row for each 
#' requested `tar_name` and columns representing a unique data hash, the
#' date of last build, and the build time for each target in seconds. 
#' 
summarize_targets <- function(names = NULL, fileout) {
  
  if(is.null(names)){
    meta_tbl <- tar_meta()
  } else {
    meta_tbl <- tar_meta(all_of(names))
  }
  
  ind_tbl <- meta_tbl %>%
    # filter the metadata table to only include targets (and not functions or
    # other tracked elements)
    filter(!is.na(seconds)) %>%
    select(tar_name = name, hash = data, date_last_build = time, build_time_seconds = seconds) 
  
  # Save total build time
  build_time <- round((sum(ind_tbl$build_time_seconds)/60),1)
  message(sprintf("The pipeline took %s minutes to build.", build_time))
  
  # Save the summary file
  readr::write_csv(ind_tbl, fileout)
  
  return(fileout)
}

