# Before you run this script!
# 1. This assumes that you have a folder called `0_inventory_exploration` in your current working directory
# 2. It also assumes you have the SRS names crosswalk from WQP in your 0_inventory_exploration folder 
#   called `public_srsnames_October_2020.csv`. You can also download the CSV from https://www.waterqualitydata.us/public_srsnames/
# 3. You need to have the list of desired characteristics compiled by the PROXIES Data 3c group. These
#   are stored in a file called `Characteristics for HABS, PFAS, METALS and COMMON.xlsx` and have 4 tabs

stopifnot("0_inventory_exploration" %in% list.dirs(full.names=FALSE))
stopifnot("public_srsnames_October_2020.csv" %in% list.files("0_inventory_exploration"))
stopifnot("Characteristics for HABS, PFAS, METALS and COMMON.xlsx" %in% list.files("0_inventory_exploration"))

library(readr)
library(readxl)
library(dplyr)
library(dataRetrieval)
library(xml2)
library(feather)
library(stringr)
library(tidyr)
library(writexl)

# Define basins
drb_huc4s <- "0204"
irl_huc4s <- c("0712", "0713")
uco_huc4s <- c("1401", "1402")

##### Identify characteristic names to use #####

###### > fxns ######

read_and_filter_characteristics <- function(characteristic_fn, sheet_nm) {
  char_obj_fn <- "0_inventory_exploration/characteristics_to_pull_cleaned.rds"
  if(!file.exists(char_obj_fn)) {
    characteristics_all <- purrr::map(excel_sheets(characteristic_fn), function(sheet_nm) {
      df <- read_xlsx(characteristic_fn, sheet = sheet_nm, skip = 3) %>% mutate(Proxies_Grp = sheet_nm)
      if(sheet_nm == "PFAS") df <- apply_pfas_casrn_fix(df)
      if(sheet_nm == "COMMON") df <- filter(df, GenericName != "DROP")
      return(df)
    }) %>% bind_rows()
    
    characteristics_in_wqp <- characteristics_all[which_characteristics_exist(characteristics_all$CharacteristicName),]
    saveRDS(characteristics_in_wqp, char_obj_fn)
  }
  return(char_obj_fn)
}

which_characteristics_exist <- function(chars) {
  
  # Read all characteristics from WQP
  all_characteristic_names <- read_xml("https://www.waterqualitydata.us/Codes/Characteristicname?mimeType=xml") %>% 
    xml_children() %>% xml_attr("value") %>% na.omit()
  
  chars_exist <- chars %in% all_characteristic_names
  
  if(!all(chars_exist)) {
    message(sprintf("The following characteristics do not exist in WQP:\n\n%s\n",
                    paste(chars[which(!chars_exist)], collapse="\n")))
  }
  
  return(chars_exist)
}

apply_pfas_casrn_fix <- function(pfas_char_df_in) {
  
  # Some of the CASRN values didn't match characteristics, so needed to do a bit of work to get those.
  
  # First, take CASRN and get PCode
  get_pcodes_from_casrn <- function(casrn_vals) {
    xwalk_df <- data.frame()
    for(casrn in na.omit(casrn_vals)) {
      casrn_df <- readr::read_tsv(sprintf("%s%s%s",
                                          "https://nwis.waterdata.usgs.gov/usa/nwis/pmcodes?pm_search=&radio_pm_search=casrn_search&casrn_search=",
                                          casrn, "&srsname_search=&format=rdb&show=parameter_nm&show=casrn"), skip = 12, col_types = readr::cols())
      xwalk_df <- bind_rows(xwalk_df, casrn_df)
      message(sprintf("Completed CASRN to PCode match for '%s'", casrn))
    }
    return(xwalk_df)
  }
  
  pcodes_casrn_xwalk <- get_pcodes_from_casrn(pfas_char_df_in$CASRN)
  
  # Then take PCodes and match to WQP parameter using xwalk here: https://www.waterqualitydata.us/public_srsnames/
  # Download as a CSV (there should be a button near the top)
  wqp_pcode_xwalk <- read_csv("0_inventory_exploration/public_srsnames_October_2020.csv")
  
  # Now match CASRN TO PCODE TO WQP Characteristic names
  casrn_matched_to_wqp <- pcodes_casrn_xwalk %>% 
    select(PCode = parameter_cd, CASRN = casrn) %>% 
    left_join(select(wqp_pcode_xwalk, PCode = parm_cd, CorrectCharacteristicName = characteristicname)) %>% 
    select(-PCode) %>% 
    filter(!is.na(CorrectCharacteristicName)) %>% unique()
  
  # Now replace Characteristic names in Excel sheet data with appropriate WQP ones when possible:
  pfas_char_df_replaced <- pfas_char_df_in %>% 
    left_join(casrn_matched_to_wqp) %>% 
    # Make new column using the CASRN matched name when available and the existing one when not
    mutate(fixed_char_name = ifelse(is.na(CorrectCharacteristicName), CharacteristicName, CorrectCharacteristicName)) %>% 
    select(fixed_char_name, Proxies_Grp) %>% rename(CharacteristicName = fixed_char_name)
  
  return(pfas_char_df_replaced)
}

###### > use fxns ######

# First, read in the Excel file compiled by group & filter out any characteristics that are not in WQP
# Also, apply a fix for the PFAS chars by matching the CASRN to CharacteristicNames
characteristic_df_fn <- read_and_filter_characteristics("0_inventory_exploration/Characteristics for HABS, PFAS, METALS and COMMON.xlsx")
characteristic_df <- readRDS(characteristic_df_fn)

##### Download the data #####

###### > fxns ######

force_columns_to_be_character <- function(df, ...) {
  cols <- c(...)
  for(col in cols) {
    df[[col]] <- as.character(df[[col]])
  }
  return(df)
}

force_columns_to_be_numeric <- function(df, ...) {
  cols <- c(...)
  for(col in cols) {
    df[[col]] <- as.numeric(df[[col]])
  }
  return(df)
}

basin_char_site_summary <- function(basin_huc4s, basin_nm, characteristics) {
  query_summary_fn <- sprintf("0_inventory_exploration/%s_main_query_summary.rds", basin_nm)
  if(!file.exists(query_summary_fn)) {
    whatWQPsites(huc = sprintf("%s*", basin_huc4s), characteristicName = characteristics, sampleMedia = c("water", "Water")) %>% 
      saveRDS(file = query_summary_fn)
  }
  return(query_summary_fn)
}

fetch_wqp_data <- function(basin_file_nm, basin_query_fn, characteristics, query_site_limit = 50, min_results = 1, min_date = "") {
  
  basin_query <- readRDS(basin_query_fn)
  siteids <- unique(basin_query$MonitoringLocationIdentifier)
  n_total_sites <- length(siteids) 
  
  # Break into multiple WQP calls so that there isn't a timeout
  sites_chunk <- ceiling(n_total_sites/query_site_limit)
  site_data_fns <- c()
  
  for(chk in seq_len(sites_chunk)) {
    start_chk <- (chk-1)*query_site_limit + 1
    end_chk <- ifelse(chk == sites_chunk, n_total_sites, chk*query_site_limit)
    site_data_fn_i <- sprintf("0_inventory_exploration/tmp_by_basin/wqp_pull_%s_chk_%s.feather", basin_file_nm, sprintf("%05d", chk))
    site_data_fns <- c(site_data_fns, site_data_fn_i)
    
    if(file.exists(site_data_fn_i)) {
      message(sprintf("ALREADY fetched sites %s:%s, skipping ...", start_chk, end_chk))
      next
    } else {
      site_data_df_i <- readWQPdata(siteid = siteids[start_chk:end_chk],
                                    characteristicName = characteristics,
                                    sampleMedia = c("water", "Water"), 
                                    minresults = min_results, 
                                    startDateLo = min_date)
      write_feather(site_data_df_i, site_data_fn_i)
      message(sprintf("Fetched sites %s:%s out of %s", start_chk, end_chk, n_total_sites))
    }
  }
  return(site_data_fns)
}

###### > use fxns ######

# Pull all but the COMMON ones. Will pull those separately because they should only be pulled for
# sites that had either Metals, PFAS, or HABs data.
main_chars <- characteristic_df %>% filter(Proxies_Grp != "COMMON") %>% pull(CharacteristicName)

# For each HUC04, determine how many sites are going to be queried
# Then download the data for each basin
drb_query_fn <- basin_char_site_summary(drb_huc4s, "drb", main_chars)
drb_data_fns <- fetch_wqp_data("drb", drb_query_fn, main_chars)

irl_query_fn <- basin_char_site_summary(irl_huc4s, "irl", main_chars)
irl_data_fns <- fetch_wqp_data("irl", irl_query_fn, main_chars)

# UCO was timing out for some reason, so had to split into two different calls
uco_query_fn <- "0_inventory_exploration/uco_main_query_summary.rds"
if(!file.exists(uco_query_fn)) {
  uco_query_fn1 <- basin_char_site_summary(uco_huc4s[1], "uco1", main_chars)
  uco_query_fn2 <- basin_char_site_summary(uco_huc4s[2], "uco2", main_chars)
  saveRDS(purrr::map(c(uco_query_fn1, uco_query_fn2), function(fn) {
    # One had character and one had numeric.
    readRDS(fn) %>% force_columns_to_be_character("HorizontalAccuracyMeasure.MeasureValue")
  }) %>% bind_rows(), file = uco_query_fn)
}
# Now pull data based on all of the sites
uco_data_fns <- fetch_wqp_data("uco", uco_query_fn, main_chars)

##### Merge the data into one file per basin #####

###### > fxns ######

read_and_combine_output <- function(fns, basin_nm) {
  fn_out <- sprintf("0_inventory_exploration/all_proxy_grp_data_%s.feather", basin_nm)
  if(!file.exists(fn_out)) {
    purrr::map(fns, function(x) {
      read_feather(x) %>% 
        # One file in HABs used "_" instead of NA which caused the full column to be character instead of numeric
        mutate(ActivityTopDepthHeightMeasure.MeasureValue = ifelse(ActivityTopDepthHeightMeasure.MeasureValue == "_", NA, ActivityTopDepthHeightMeasure.MeasureValue)) %>%
        force_columns_to_be_character(
          "SampleCollectionMethod.MethodIdentifier",
          "ResultAnalyticalMethod.MethodIdentifier",
          "ResultAnalyticalMethod.MethodName",
          "ResultMeasureValue", # Need to account for < | >
          "ProjectIdentifier",
          "LaboratoryName",
          "ActivityBottomDepthHeightMeasure.MeasureValue"
        )
    }) %>% bind_rows() %>% 
      write_feather(fn_out)
  }
  return(fn_out)
}

###### > use fxns ######

# Merge all of the basin data into one file per basin!
drb_data_main_fn <- read_and_combine_output(drb_data_fns, "drb")
irl_data_main_fn <- read_and_combine_output(irl_data_fns, "irl")
uco_data_main_fn <- read_and_combine_output(uco_data_fns, "uco")

##### Using the sites and data pulled for the Proxy groups, grab COMMONs data #####

###### > fxns ######

pull_and_merge_matching_commons_data <- function(basin_main_data_fn, basin_query_fn, basin_nm, commons_chars_df) {
  
  basin_data_all_fn <- sprintf("0_inventory_exploration/%s_data_all.feather", basin_nm)
  
  if(!file.exists(basin_data_all_fn)) {
    basin_data_main <- read_feather(basin_main_data_fn)
    
    # I looked at the histogram for all of the dates and there is really nothing prior to 1940, so we are 
    #   going to pull COMMONs data only from 1940 onward to try limit how much we are getting.
    #     hist(as.numeric(basin_data_main$ActivityStartDate))
    
    basin_data_commons_fn <- fetch_wqp_data(basin_file_nm = sprintf("%s_commons", basin_nm), 
                                            basin_query_fn, commons_chars_df$CharacteristicName, min_date = "1940-01-01")
    
    # Merge all of the basin data into one file per basin!
    basin_data_commons_all_fn <- read_and_combine_output(basin_data_commons_fn, sprintf("%s_commons", basin_nm))
    
    # Then load that data
    basin_data_commons <- read_feather(basin_data_commons_all_fn)
    
    # Make a column that is a list of all the commons data available for each day-site combo
    basin_commons_chars_col <- basin_data_commons %>% 
      left_join(commons_chars_df, by = "CharacteristicName") %>% 
      group_by(MonitoringLocationIdentifier, ActivityStartDate) %>% 
      summarize(Commons_Characteristics_Available = paste(sort(unique(CharacteristicName)), collapse = " | "),
                Commons_CharacteristicsGeneric_Available = paste(sort(unique(GenericName)), collapse = " | "))
    
    # Then join with actual Proxies group data to keep only specific dates!
    basin_data_main %>% 
      left_join(basin_commons_chars_col) %>% 
      write_feather(basin_data_all_fn)
  }
  
  return(basin_data_all_fn)
}

###### > use fxns ######

commons_chars_df <- characteristic_df %>% filter(Proxies_Grp == "COMMON")
drb_data_all_fn <- pull_and_merge_matching_commons_data(drb_data_main_fn, drb_query_fn, "drb", commons_chars_df)
irl_data_all_fn <- pull_and_merge_matching_commons_data(irl_data_main_fn, irl_query_fn, "irl", commons_chars_df)
uco_data_all_fn <- pull_and_merge_matching_commons_data(uco_data_main_fn, uco_query_fn, "uco", commons_chars_df)

##### Adding in site info #####

###### > fxns ######

fetch_site_info_data <- function(basin_data_fn, basin_nm) {
  basin_site_info_fn <- sprintf("0_inventory_exploration/%s_site_metadata.feather", basin_nm)
  if(!file.exists(basin_site_info_fn)) {
    basin_sites <- read_feather(basin_data_fn) %>% pull(MonitoringLocationIdentifier) %>% unique()
    whatWQPsites(siteid=basin_sites) %>% 
      write_feather(basin_site_info_fn)
  }
  
  return(basin_site_info_fn)
}

###### > use fxns ######

drb_site_fn <- fetch_site_info_data(drb_data_all_fn, "drb")
irl_site_fn <- fetch_site_info_data(irl_data_all_fn, "irl")
uco_site_fn <- fetch_site_info_data(uco_data_all_fn, "uco")

##### Apply Jenny's cleaning criteria #####

###### > fxns ######

apply_cleaning_criteria <- function(basin_data_fn, basin_site_fn, basin_nm, characteristic_df) {
  
  basin_cleaned_fn <- sprintf("0_inventory_exploration/%s_data_ready.feather", basin_nm)
  
  # First things, merge in the site metadata data
  basin_data <- add_site_metadata(basin_data_fn, basin_site_fn) 
  
  if(!file.exists(basin_cleaned_fn)) {
    
    basin_data_clean <- basin_data %>% 
      
      # Create FLAG for true missing results
      mutate(flag_missingResult = (is.na(ResultMeasureValue) & 
                                     is.na(DetectionQuantitationLimitMeasure.MeasureValue)) | 
               grepl("*Not Reported|Not Reported|not reported", ResultDetectionConditionText) |
               stringr::str_detect(tidyr::replace_na(ResultCommentText, ''), "not collected|not analyzed")) %>% 
      
      # Harmonize to generic fractions  
      mutate(ResultSampleFractionText = 
               ifelse(ResultSampleFractionText %in% c("Dissolved", "Filtered, lab", "Filtered, field"), 
                      yes = "filtered",
                      no = ifelse(ResultSampleFractionText %in% c("Total", "Total Recoverable", "Total Recovrble", "Recoverable"), 
                                  yes = "unfiltered", 
                                  no = ifelse(ResultSampleFractionText %in% c("Suspended"), 
                                              yes = "particulate",
                                              no = ifelse(is.na(ResultSampleFractionText) | ResultSampleFractionText %in% 
                                                            c("NA", "None", "Pot. Dissolved", "Filterable", "Non-filterable", "Non-Filterable (Particle)"),
                                                          yes = "unknown",
                                                          no = ResultSampleFractionText))))) %>%
      # Create FLAG if fraction is unknown (thus ambiguous)
      mutate(flag_fracAmbiguous = ResultSampleFractionText == "unknown") %>% 
      
      # Flag records w/ zero values and not enough information to resolve <-- meaning ResultMeasureValue was zero but DetectionQuantitationLimitMeasure.MeasureValue was also zero or missing. 
      mutate(flag_resultZero = flag_missingResult | 
               ((ResultMeasureValue == 0 & is.na(DetectionQuantitationLimitMeasure.MeasureValue)) | 
                  (is.na(ResultMeasureValue) & DetectionQuantitationLimitMeasure.MeasureValue == 0) | 
                  (ResultMeasureValue == 0 & DetectionQuantitationLimitMeasure.MeasureValue == 0))) %>% 
      
      # Update resultVal, resultUnits, and remarkCode for non-detects
      update_cols_for_non_detects() %>% 
      
      # Now remove missing data & 0s
      filter(!flag_resultZero) %>% 
      
      # Filter data based on Proxy group specific needs:
      filter_habs_data(characteristic_df) %>%
      filter_pfas_data(characteristic_df) %>%
      filter_metals_data(characteristic_df) %>%
      # Keep "fraction" separate for metals, but not the others
      mutate(Fraction = ifelse(CharacteristicName %in% 
                                 pull(filter(characteristic_df, Proxies_Grp == "METALS"), CharacteristicName), 
                               ResultSampleFractionText, "ALL")) %>% 
      
      # Remove duplicate data (Jenny was using "collectingOrganization", but that wasn't in this data)
      group_by(OrganizationIdentifier, MonitoringLocationIdentifier, ActivityStartDate, ActivityStartTime.Time, CharacteristicName, ResultSampleFractionText) %>% 
      mutate(n_duplicated = n()) %>% 
      filter(n_duplicated == 1) %>%
      select(-n_duplicated) %>%
      ungroup() 
    
    write_feather(basin_data_clean, basin_cleaned_fn)
    
  } else {
    basin_data_clean <- read_feather(basin_cleaned_fn)
  }
  
  # How much data got filtered out? 97% for DRB?!
  message(sprintf("During cleaning process, %s%% of the %s data was filtered out. %s observations remain.", 
                  round((nrow(basin_data) - nrow(basin_data_clean)) / nrow(basin_data) * 100, 2), 
                  basin_nm, prettyNum(nrow(basin_data_clean), ",")))
  
  return(basin_cleaned_fn)
}

add_site_metadata <- function(basin_data_fn, basin_site_fn) {
  read_feather(basin_site_fn) %>% 
    select(MonitoringLocationIdentifier, MonitoringLocationTypeName) %>% 
    right_join(read_feather(basin_data_fn), by = "MonitoringLocationIdentifier")
}

checkBelowDetect <- function(df, true_output, false_output) {  
  # From J. Murphy
  ifelse(is.na(df$ResultMeasureValue) & 
           !is.na(df$DetectionQuantitationLimitMeasure.MeasureValue) &
           df$ResultDetectionConditionText %in% 
           c("*Non-detect", "Not Detected", "Present Below Quantification Limit"),
         true_output, false_output)
}

checkAboveDetect <- function(df, true_output, false_output) {  
  # From J. Murphy
  ifelse(is.na(df$ResultMeasureValue) & 
           !is.na(df$DetectionQuantitationLimitMeasure.MeasureValue) &
           df$ResultDetectionConditionText %in% 
           c("Present Above Quantification Limit"),
         true_output, false_output)
}

update_cols_for_non_detects <- function(df) {
  df %>% 
    # Update for Below detection limit
    mutate( 
      # Create new resultVal column: contains result value or detection level if censored
      resultVal = checkBelowDetect(., DetectionQuantitationLimitMeasure.MeasureValue, ResultMeasureValue),
      # Create new units column: update with detection limit units when appropriate
      resultUnits = checkBelowDetect(., DetectionQuantitationLimitMeasure.MeasureUnitCode, ResultMeasure.MeasureUnitCode),
      # Create new remark column: if censored result, flag with <
      remarkCd = checkBelowDetect(., "<", "NA")) %>% 
    # Update for Above detection limit
    mutate( 
      # Create new resultVal column: contains result value or detection level if greater than comment is present
      resultVal = checkAboveDetect(., DetectionQuantitationLimitMeasure.MeasureValue, resultVal),
      # Create new units column: update with detection limit units when appropriate
      resultUnits = checkAboveDetect(., DetectionQuantitationLimitMeasure.MeasureUnitCode, resultUnits),
      # Populate remark code column with ">" for these results
      remarkCd = checkAboveDetect(., ">", remarkCd))
}

filter_habs_data <- function(df, chars_df) {
  habs_chars <- chars_df %>% filter(Proxies_Grp == "HABS") %>% pull(CharacteristicName)
  mutate(df, bad_habs = CharacteristicName %in% habs_chars & 
           (# Keep only surface water sites
             grepl("Well", MonitoringLocationTypeName) 
           )) %>% 
    filter(!bad_habs) %>% 
    select(-bad_habs)
}

filter_pfas_data <- function(df, chars_df) {
  pfas_chars <- chars_df %>% filter(Proxies_Grp == "PFAS") %>% pull(CharacteristicName)
  mutate(df, bad_pfas = CharacteristicName %in% pfas_chars & FALSE) %>% 
    filter(!bad_pfas) %>% 
    select(-bad_pfas)
}

filter_metals_data <- function(df, chars_df) {
  metals_chars <- chars_df %>% filter(Proxies_Grp == "METALS") %>% pull(CharacteristicName)
  mutate(df, bad_metals = CharacteristicName %in% metals_chars & 
           (# Keep only surface water sites
             grepl("Well", MonitoringLocationTypeName) |
             # Remove "Bed Sediment" samples
             ActivityMediaName == "Bed Sediment" | ResultSampleFractionText == "Bed Sediment"
           )) %>% 
    filter(!bad_metals) %>% 
    select(-bad_metals)
}

###### > use fxns ######

drb_data_clean_fn <- apply_cleaning_criteria(drb_data_all_fn, drb_site_fn, "drb", characteristic_df)
irl_data_clean_fn <- apply_cleaning_criteria(irl_data_all_fn, irl_site_fn, "irl", characteristic_df)
uco_data_clean_fn <- apply_cleaning_criteria(uco_data_all_fn, uco_site_fn, "uco", characteristic_df)

##### Summarize into tables, figures, and maps #####

###### > fxns ######

add_proxy_category <- function(df, characteristic_df) {
  df %>% 
    mutate(Proxy_Category = ifelse(CharacteristicName %in% pull(filter(characteristic_df, Proxies_Grp == "HABS"), CharacteristicName),
                                   "HABS", 
                                   ifelse(CharacteristicName %in% pull(filter(characteristic_df, Proxies_Grp == "PFAS"), CharacteristicName),
                                          "PFAS",
                                          ifelse(CharacteristicName %in% pull(filter(characteristic_df, Proxies_Grp == "METALS"), CharacteristicName),
                                                 "METALS", NA))))
}

# Helper fxns for determining how much support data is available

# Add column with T/F for whether that characteristic group had data for that observation
# The column name will be the generic group name
add_generic_test_columns <- function(df, generic_grps) {
  for(grp_nm in generic_grps) {
    df <- df %>% mutate("{grp_nm}" := grp_nm %in% GenericAvailableList)
  }
  return(df)
}

# Add column with T/F for whether that characteristic existed for that observation
# The column name will be the characteristic name
add_char_test_columns <- function(df, characteristic_nms) {
  for(char_nm in characteristic_nms) {
    df <- df %>% mutate("{char_nm}" := char_nm %in% CharacteristicName)
  }
  return(df)
}

string_to_list <- function(x, sep) {
  strsplit(x, " | ", fixed = TRUE)
}

###### > use fxns ######

drb_data <- read_feather(drb_data_clean_fn) %>% mutate(Basin = "DRB")
irl_data <- read_feather(irl_data_clean_fn) %>% mutate(Basin = "IRL")
uco_data <- read_feather(uco_data_clean_fn) %>% mutate(Basin = "UCO")

all_basin_data <- drb_data %>% bind_rows(irl_data)%>% bind_rows(uco_data) 

all_basin_commons_data <- all_basin_data %>% 
  add_proxy_category(characteristic_df) %>% 
  filter(Proxy_Category == "COMMONS")

all_basin_data_summary_long <- all_basin_data %>% 
  group_by(Basin, CharacteristicName, Fraction) %>% 
  summarize(USGS_Count = sum(ProviderName == "NWIS"),
            Other_Count = sum(ProviderName != "NWIS"),
            Unique_Site_Count = length(unique(MonitoringLocationIdentifier)),
            Observation_Count = n(),
            NonDetect_Count = sum(grepl(">|<", remarkCd)),
            Earliest_Date = min(ActivityStartDate)) %>% 
  add_proxy_category(characteristic_df) %>% 
  mutate(Percent_Observations_NonDetect = round(NonDetect_Count/Observation_Count*100)) %>% 
  select(Proxy_Category, -NonDetect_Count, everything()) %>% 
  arrange(Proxy_Category, Basin, CharacteristicName, Fraction)

all_basin_data_summary_wide <- all_basin_data_summary_long %>% 
  pivot_wider(names_from = "Basin", values_from = c(-"CharacteristicName", -"Fraction", -"Proxy_Category", -"Basin")) %>% 
  # Replace missing numeric (not dates) with 0s
  mutate_if(is.numeric, ~replace_na(., 0))

# Summarize supporting data by characteristic name
commons_chars_names <- filter(characteristic_df, Proxies_Grp == "COMMON") %>% 
  pull(CharacteristicName) %>% unique()

# This takes ~ 
available_supporting_data_chars <- all_basin_data %>% #
  add_proxy_category(characteristic_df) %>% 
  select(Basin, MonitoringLocationIdentifier, ActivityStartDate, Proxy_Category, Commons_Characteristics_Available) %>% 
  # For every observation (rows), add a T/F for each of the common characteristics to 
  #   say whether or not there was data present
  rowwise() %>% 
  mutate(CharacteristicName = string_to_list(Commons_Characteristics_Available, " | ")) %>% 
  add_char_test_columns(commons_chars_names) %>% 
  ungroup() %>% # stop "rowwise"
  select(Basin, MonitoringLocationIdentifier, Proxy_Category, one_of(commons_chars_names)) %>% 
  # Now that we have a column for each of the commons groups made of T/F saying whether that data
  #   was present for a given observation, summarize into percent data with co-located supporting data
  pivot_longer(cols = one_of(commons_chars_names), 
               names_to = "CharacteristicName", values_to = "Present") %>% 
  group_by(Basin, CharacteristicName) %>% 
  summarize(Count = sum(Present)) %>% 
  pivot_wider(names_from = Basin, values_from = Count)

basin_chars_summary <- all_basin_data %>%
  # Filter out commons and add in after
  add_proxy_category(characteristic_df) %>% 
  filter(Proxy_Category != "COMMON") %>% 
  group_by(Basin, CharacteristicName) %>% 
  summarize(Count = n())%>% 
  pivot_wider(names_from = Basin, values_from = Count) %>% 
  bind_rows(available_supporting_data_chars2)
parameters_returned <- characteristic_df %>% 
  left_join(basin_chars_summary, by = "CharacteristicName") %>% 
  rename(`Proxy Group` = Proxies_Grp, `WQP Parameter Name` = CharacteristicName) %>% 
  mutate(across(where(is.numeric), ~replace_na(.x, 0)))

# Summarize supporting data by generic group
commons_generic_grps <- filter(characteristic_df, Proxies_Grp == "COMMON") %>% 
  pull(GenericName) %>% unique()

# This takes ~4 min to run
available_supporting_data_generics <- all_basin_data %>% #
  add_proxy_category(characteristic_df) %>% 
  select(Basin, MonitoringLocationIdentifier, ActivityStartDate, Proxy_Category, Commons_CharacteristicsGeneric_Available) %>% 
  # For every observation (rows), add a T/F for each of the common generic categories to 
  #   say whether or not there was data present
  rowwise() %>% 
  mutate(GenericAvailableList = string_to_list(Commons_CharacteristicsGeneric_Available, " | ")) %>% 
  add_generic_test_columns(commons_generic_grps) %>% 
  ungroup() %>% # stop "rowwise"
  select(Basin, MonitoringLocationIdentifier, Proxy_Category, all_of(commons_generic_grps)) %>% 
  # Now that we have a column for each of the commons groups made of T/F saying whether that data
  #   was present for a given observation, summarize into percent data with co-located supporting data
  pivot_longer(cols = all_of(commons_generic_grps), 
               names_to = "Generic Name", values_to = "Present") %>% 
  group_by(Basin, Proxy_Category, `Generic Name`) %>% 
  summarize(`Percent data co-collected` = round(sum(Present) / n() * 100)) %>% 
  unite("Basin Contaminant", Basin, Proxy_Category, sep = " ") %>% 
  pivot_wider(names_from = `Basin Contaminant`, values_from = `Percent data co-collected`)

# Summarize how much PFAS data is GW vs SW?
# PFAS is the only category where GW wasn't filtered out.
summarize_site_type <- function(df) {
  df %>% 
    group_by(Basin, CharacteristicName, MonitoringLocationTypeName) %>% 
    summarize(Count = n()) %>%
    pivot_wider(names_from = MonitoringLocationTypeName, values_from = Count) %>% 
    rename(`WQP Parameter Name` = CharacteristicName)
}

pfas_by_site_type <- all_basin_data %>% 
  add_proxy_category(characteristic_df) %>% 
  filter(Proxy_Category == "PFAS") %>%
  summarize_site_type()
habs_by_site_type <- all_basin_data %>% 
  add_proxy_category(characteristic_df) %>% 
  filter(Proxy_Category == "HABS") %>%
  summarize_site_type()
metals_by_site_type <- all_basin_data %>% 
  add_proxy_category(characteristic_df) %>% 
  filter(Proxy_Category == "METALS") %>%
  summarize_site_type()

# Only includes characteristicNames that pass the `which_characteristics_exist` test
#   or don't have "DROP" in the "GenericName" column
supporting_data_queried <- characteristic_df %>% 
  filter(Proxies_Grp == "COMMON") %>% 
  select(`WQP Parameter Name` = CharacteristicName,
         `Generic Name` = GenericName)

squish <- function(x) paste(x, collapse = " & ")
basin_details <- tibble(
  `IWP Basin Name` = c("Delaware River Basin", "Illinois River Basin", "Upper Colorado River Basin"),
  `Basin Abbreviation` = c("DRB", "IRL", "UCO"),
  `Basin HUC04` = c(drb_huc4s, squish(irl_huc4s), squish(uco_huc4s))
)

write_xlsx(path="0_inventory_exploration/Data.3c - Discrete Data.xlsx", list(
  `Parameters by basin` = all_basin_data_summary_wide,
  `Parameters queried` = parameters_returned,
  `Supporting data availability` = available_supporting_data_generics,
  `Supporting data queried` = supporting_data_queried,
  `IWP basin details` = basin_details,
  `PFAS parameters by site type` = pfas_by_site_type,
  `HABS parameters by site type` = habs_by_site_type,
  `METALS parameters by site type` = metals_by_site_type
))

##### Summarize available data by location #####

# Get locations
site_ids <- unique(all_basin_data$MonitoringLocationIdentifier)
site_locations <- whatWQPsites(siteid = site_ids) %>% 
  select(MonitoringLocationIdentifier, MonitoringLocationName,
         LongitudeMeasure, LatitudeMeasure)

site_summary <- all_basin_data %>% 
  add_proxy_category(characteristic_df) %>% 
  group_by(Basin, Proxy_Category, MonitoringLocationIdentifier) %>% 
  summarize(Count_Obs = n()) %>% 
  arrange(desc(Count_Obs)) %>% 
  left_join(site_locations, by = "MonitoringLocationIdentifier") %>% 
  select(Basin, Proxy_Category, MonitoringLocationIdentifier, MonitoringLocationName,
         LongitudeMeasure, LatitudeMeasure, Count_Obs)

write_xlsx(path = "0_inventory_exploration/Data.3c - Site Summaries.xlsx",
           list(`Observation counts by site` = site_summary))

##### THINGS TO DO ######

# TODO: 
#   1. VERIFY WHY THERE WAS NO PFAS DATA FROM NWIS (SEEMS SUSPICIOUS)
