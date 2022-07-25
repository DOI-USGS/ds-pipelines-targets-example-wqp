# Source the functions that will be used to build the targets in p3_targets_list
source("3_harmonize/src/format_columns.R")
source("3_harmonize/src/clean_wqp_data.R")
source("3_harmonize/src/clean_conductivity_data.R")
source("3_harmonize/src/clean_temperature_data.R")

p3_targets_list <- list(
  
  # All columns in p2_wqp_data_aoi are of class character. Coerce select columns
  # back to numeric, but first retain original entries in new columns ending in 
  # "_original". The default option is to format "ResultMeasureValue" and 
  # "DetectionQuantitationLimitMeasure.MeasureValue" to numeric, but additional
  # variables can be added using the `vars_to_numeric` argument in format_columns().
  # By default, format_columns() will retain all columns, but undesired variables 
  # can also be dropped from the WQP dataset using the optional `drop_vars` argument. 
  tar_target(
    p3_wqp_data_aoi_formatted,
    format_columns(p2_wqp_data_aoi)
  ),
  
  # Harmonize WQP data
  tar_target(
    p3_wqp_data_aoi_clean,
    clean_wqp_data(p3_wqp_data_aoi_formatted, p1_char_names_crosswalk)
  )

)
