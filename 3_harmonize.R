# Source the functions that will be used to build the targets in p3_targets_list
source("3_harmonize/src/format_columns.R")

p3_targets_list <- list(
  
  # All columns in p2_wqp_data_aoi are of class character. Coerce select columns
  # back to numeric, and save an output file that contains any entries that could 
  # not be parsed as numeric. The default option is to format "ResultMeasureValue" 
  # and "DetectionQuantitationLimitMeasure.MeasureValue" to numeric, but additional
  # variables can be added using the `vars_to_numeric` argument in format_columns().
  # For any columns parsed to numeric, the original entries are retained in columns
  # ending in "_original." By default, format_columns() will retain all columns, 
  # but undesired variables can also be dropped from the WQP dataset using the 
  # optional `drop_vars` argument. 
  tar_target(
    p3_wqp_data_aoi_formatted,
    format_columns(p2_wqp_data_aoi, save_dir = "3_harmonize/out")
  )

)
