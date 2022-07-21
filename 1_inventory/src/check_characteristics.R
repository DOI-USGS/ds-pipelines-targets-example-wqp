#' @title Create parameter-characteristics crosswalk table
#' 
#' @description 
#' Function to create a crosswalk table that maps characteristic names onto
#' more commonly-used water quality parameter names. 

#' @param wqp_params list object where each element of the list is a named vector.
#' The vectors correspond with a parameter group and contain character string(s)
#' representing known WQP characteristic names associated with each parameter.
#' 
#' @returns 
#' Returns a data frame containing a row for each unique characteristic name.
#' Columns represent the characteristic name and its corresponding parameter 
#' name.
#' 
crosswalk_characteristics <- function(wqp_params){
  
  params_df <- lapply(names(wqp_params), function(x){
    params <- data.frame(char_name = wqp_params[[x]],
                         parameter = x) 
  }) %>%
    bind_rows()
  
  return(params_df)
  
}


#' @title Read valid WQP characteristic names
#' 
#' @description 
#' Function to read and return all valid entries for `CharacteristicName` 
#' from the WQP web service. 
#' 
#' @returns 
#' vector of character strings representing all of the valid
#' characteristic names from WQP.
#' 
#' @examples 
#' read_wqp_characteristics()
#' 
read_wqp_characteristics <- function(){
  
  # Fetch all characteristics from WQP
  all_characteristic_names <- xml2::read_xml("https://www.waterqualitydata.us/Codes/Characteristicname?mimeType=xml") %>% 
    xml2::xml_children() %>% xml2::xml_attr("value") %>% na.omit()
  
  return(all_characteristic_names)
}


#' @title Check requested characteristics
#' 
#' @description 
#' Function to check whether all user-specified entries for `CharacteristicName` 
#' are valid.
#' 
#' @param char_names vector of character strings indicating which characteristic
#' names to check against valid entries in the WQP.
#' 
#' @returns 
#' logical statement for each string within the supplied characteristics list;
#' if any characteristics are not valid, a printed message will indicate which 
#' characteristics do not exist in the WQP. 
#' 
#' @examples 
#' check_valid_characteristics(c("Specific conductivity", "Specific conductance"))
#' 
check_valid_characteristics <- function(char_names) {
  
  # Fetch all WQP characteristics and test whether each user-supplied characteristic
  # name is within the list of valid entries from WQP
  all_characteristic_names <- read_wqp_characteristics()
  chars_exist <- char_names %in% all_characteristic_names
  
  if(!all(chars_exist)) {
    warning(sprintf("The following characteristics do not exist in WQP:\n\n%s\n",
                    paste(char_names[which(!chars_exist)], collapse="\n")))
  }
  
  return(chars_exist)
}


#' @title Subset characteristic names by parameter group
#' 
#' @description
#' Function to filter WQP parameters and associated characteristic names from a 
#' characteristics-to-parameter crosswalk table. Characteristic names associated 
#' with the requested parameter groups of interest will be included in the data pull.  
#' 
#' @param char_names_crosswalk data frame containing columns "char_name" and 
#' "parameter". The column "char_name" contains character strings representing 
#' known WQP characteristic names associated with each parameter.
#' @param param_groups_select character string indicating which parameter groups 
#' to request in the WQP data pull. Parameter strings should match the vector
#' names in `wqp_params`.
#' 
#' @returns 
#' returns a vector of character strings containing the characteristic
#' names that will be used to query the WQP. If any characteristics in 
#' `wqp_params` are not valid, a printed message will indicate which 
#' characteristics do not exist in the WQP.
#' 
#' @examples 
#' params <- data.frame(char_name = c("PH", "pH", "pH, lab"),
#'                      parameter = rep("pH", 3))
#' filter_characteristics(params, "pH")
#' 
filter_characteristics <- function(char_names_crosswalk, param_groups_select){
  
  # Create character string containing desired characteristic names
  characteristics_select <- char_names_crosswalk %>%
    filter(parameter %in% param_groups_select) %>%
    pull(char_name)
  
  # Return desired characteristic names that represent valid WQP characteristics
  characteristics_in_wqp <- characteristics_select[check_valid_characteristics(characteristics_select)]
  
  return(characteristics_in_wqp)
}


#' @title Find similar WQP characteristic names
#' 
#' @description 
#' Function to search a list of WQP characteristic names and find other
#' characteristic names that are similar to the requested parameter names.
#' 
#' @param characteristics_select character string containing desired characteristic 
#' names as identified from the wqp codes configuration (cfg) file.
#' @param param_groups_select character string indicating what parameter groups 
#' will be used for the WQP data pull.
#' @param save_dir file path indicating where output files containing similar 
#' characteristic names should be saved.
#' 
#' @returns 
#' Saves one .txt file for each parameter in param_groups_select. The saved 
#' file(s) includes valid characteristic names in WQP that are similar to that 
#' parameter, and indicates which entries are already included in the cfg file and
#' which entries are not. Those entries not already included in the cfg file are 
#' meant to provide a quick reference for additional characteristic names that 
#' might warrant further consideration. 
#' 
#' @examples
#' chars_select <- c("Temperature", "Temperature, sample")
#' params_select <- c("temperature")
#' save_dir <- "1_inventory/out"
#' find_similar_characteristics(chars_select, params_select, save_dir)
#' 
find_similar_characteristics <- function(characteristics_select, param_groups_select, save_dir){
  
  # Read in characteristic names from WQP
  characteristics_all <- read_wqp_characteristics()
  
  # For each parameter group, search for approximate string matches
  # within list of WQP characteristic names
  matched_characteristics <- lapply(param_groups_select,function(x){
    
    message(sprintf("Searching for other WQP characteristics that may belong to parameter: %s",x))
    
    matched_chars <- agrep(x, characteristics_all,
                           value = TRUE, ignore.case = TRUE)
    matched_chars_selected <- matched_chars[matched_chars %in% characteristics_select]
    matched_chars_not_selected <- matched_chars[!matched_chars %in% characteristics_select]
    
    header_selected <- "The following characteristics are already included in the WQP codes cfg file: "
    header_not_selected <- paste0("The following characteristics might be relevant to the selected ",
                                  "parameter and are not included in the WQP codes cfg file: ")
    out_file <- paste0(save_dir,"/wqp_characteristics_",x,".txt")
    writeLines(c(header_selected,matched_chars_selected,"\n",
                 header_not_selected, matched_chars_not_selected),
               sep = "\n" ,out_file)
    return(out_file)
  })
  
  return(unlist(matched_characteristics))
}


