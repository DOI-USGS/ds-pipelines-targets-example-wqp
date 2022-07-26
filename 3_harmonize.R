# Source the functions that will be used to build the targets in p3_targets_list
source("3_harmonize/src/format_columns.R")
source("3_harmonize/src/clean_wqp_data.R")
source("3_harmonize/src/clean_conductivity_data.R")
source("3_harmonize/src/clean_temperature_data.R")

p3_targets_list <- list(
  
  # All columns in p2_wqp_data_aoi are of class character. Coerce select 
  # columns back to numeric, but first retain original entries in new columns
  # ending in "_original". The default option is to format "ResultMeasureValue"
  # and "DetectionQuantitationLimitMeasure.MeasureValue" to numeric, but 
  # additional variables can be added using the `vars_to_numeric` argument in 
  # format_columns(). By default, format_columns() will retain all columns, but
  # undesired variables can also be dropped from the WQP dataset using the 
  # optional `drop_vars` argument. 
  tar_target(
    p3_wqp_data_aoi_formatted,
    format_columns(p2_wqp_data_aoi)
  ),
  
  # Harmonize WQP data by uniting diverse characteristic names under more
  # commonly-used water quality parameter names, flagging missing records,
  # and flagging duplicate records. Duplicated rows are identified using 
  # the argument `duplicate_definition`. By default, a record will be 
  # considered duplicated if it shares the same organization, site id, date,
  # time, characteristic name and sample fraction, although a different 
  # vector of column names can be passed to `clean_wqp_data()` below. By 
  # default, duplicated rows are flagged and omitted from the dataset. To 
  # retain duplicate rows, set the argument `remove_duplicated_rows` to FALSE. 
  tar_target(
    p3_wqp_data_aoi_clean,
    clean_wqp_data(p3_wqp_data_aoi_formatted, p1_char_names_crosswalk)
  ),
  
  # Create a table that defines parameter-specific data cleaning functions.
  # Cleaning functions should be defined within a named list where the name
  # of each list element is the function name.
  tar_target(
    p3_wqp_param_cleaning_info,
    tibble(
      parameter = c('conductivity', 'temperature'),
      cleaning_fxn = list(clean_conductivity_data = clean_conductivity_data,
                          clean_temperature_data = clean_temperature_data)
    )
  ),
  
  # Group the WQP data by parameter group in preparation for parameter-specific
  # data cleaning steps.
  tar_target(
    p3_wqp_data_aoi_clean_grp,
    p3_wqp_data_aoi_clean %>%
      left_join(p3_wqp_param_cleaning_info, by = "parameter") %>% 
      group_by(parameter) %>%
      tar_group(),
    iteration = "group"
  ),
  
  # Harmonize WQP data by applying parameter-specific data cleaning steps,
  # including harmonizing units where possible. `p3_wqp_param_cleaning_info` 
  # is a {targets} dependency, so changes to any of the parameter-specific 
  # cleaning functions will trigger a rebuild of only those branches that 
  # correspond to the group of data impacted by the change.
  tar_target(
    p3_wqp_data_aoi_clean_param,
    {
      # Decide which function to use
      fxn_to_use <- p3_wqp_data_aoi_clean_grp %>%
        pull(cleaning_fxn) %>%
        names() %>% 
        unique()
      
      # If applicable, apply parameter-specific cleaning function
      if(length(fxn_to_use) > 0){
        do.call(fxn_to_use, list(wqp_data = p3_wqp_data_aoi_clean_grp))
      } else {.}
    },
    map(p3_wqp_data_aoi_clean_grp)
  )

)

