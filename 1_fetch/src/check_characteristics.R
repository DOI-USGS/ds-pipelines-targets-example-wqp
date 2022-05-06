#' Function to read all valid characteristic names from WQP web service
#' 
#' @return vector of character strings representing all of the valid
#' characteristic names from WQP
#' 
read_wqp_characteristics <- function(){
  
  # Fetch all characteristics from WQP
  all_characteristic_names <- xml2::read_xml("https://www.waterqualitydata.us/Codes/Characteristicname?mimeType=xml") %>% 
    xml2::xml_children() %>% xml2::xml_attr("value") %>% na.omit()
  
  return(all_characteristic_names)
}


#' Function to check whether all user-specified characteristics are valid.
#' 
#' @param chars vector of character strings representing which characteristic
#' names to check against valid entries in the WQP.
#' 
#' @return logical statement for each string within supplied characteristics list;
#' if any characteristics are not valid, a printed message will indicate which 
#' characteristics do not exist in WQP. 
#' @example check_valid_characteristics(c("Specific conductivity", "Specific conductance"))
#' 
check_valid_characteristics <- function(chars) {
  
  # Fetch all WQP characteristics and test whether each user-supplied characteristic
  # name is within the list of valid entries from WQP
  all_characteristic_names <- read_wqp_characteristics()
  chars_exist <- chars %in% all_characteristic_names
  
  if(!all(chars_exist)) {
    warning(sprintf("The following characteristics do not exist in WQP:\n\n%s\n",
                    paste(chars[which(!chars_exist)], collapse="\n")))
  }
  
  return(chars_exist)
}


#' Function to filter WQP parameters and associated characteristic names from a 
#' general configuration file to only include the selected parameter groups of 
#' interest for the data pull. 
#' 
#' @param wqp_params list containing data frames corresponding to various parameter
#' groups of interest. Each parameter data frame contains a vector of character
#' strings representing known WQP characteristic names associated with each 
#' parameter. 
#' @param param_groups_select character string indicating what parameter groups will
#' be used for the WQP data pull.
#' 
filter_characteristics <- function(wqp_params, param_groups_select){
  
  # Create character string containing desired characteristic names
  characteristics_select <- as.character(unlist(wqp_params[param_groups_select]))
  
  # Return desired characteristic names that represent valid WQP characteristics
  characteristics_in_wqp <- characteristics_select[check_valid_characteristics(characteristics_select)]
  
  return(characteristics_in_wqp)
}


#' Function to search a list of valid WQP characteristic names and find valid
#' characteristic names that are similar to the parameters requested.
#' 
#' @param characteristics_select character string of desired characteristic names 
#' identified from the wqp codes cfg file
#' @param param_groups_select character string indicating what parameter groups 
#' will be used for the WQP data pull
#' @param save_dir file path indicating where log files containing similar 
#' characteristic names should be saved
#' 
#' @return saves one .txt file for each parameter in param_groups_select. The saved 
#' file(s) indicates valid characteristic names in WQP that are similar to that 
#' parameter, and indicates which entries are already included in the cfg file and
#' which entries are not. Those entries not already included in the cfg file are 
#' meant to provide a quick reference for additional characteristic names that 
#' might warrant further consideration. 
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
    header_not_selected <- "The following characteristics might be relevant to the selected parameter and are not included in the WQP codes cfg file: "
    out_file <- paste0(save_dir,"/wqp_characteristics_",x,".txt")
    writeLines(c(header_selected,matched_chars_selected,"\n",
                 header_not_selected, matched_chars_not_selected),
               sep = "\n" ,out_file)
    return(out_file)
  })
  
  return(unlist(matched_characteristics))
}


