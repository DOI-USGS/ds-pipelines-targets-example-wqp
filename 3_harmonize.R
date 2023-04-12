# Source the functions that will be used to build the targets in p3_targets_list
source("3_harmonize/src/format_columns.R")
source("3_harmonize/src/clean_wqp_data.R")
source("3_harmonize/src/clean_conductivity_data.R")
source("3_harmonize/src/clean_temperature_data.R")
source("3_harmonize/src/summarize_wqp_records.R")

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
  # commonly-used water quality parameter names, flagging missing records, and
  # omitting duplicate records. For the purposes of this pipeline, duplicated 
  # records are those that are exactly duplicated across all columns. To retain 
  # duplicate rows, set the argument `remove_duplicated_records` to FALSE.
  tar_target(
    p3_wqp_data_aoi_clean,
    clean_wqp_data(wqp_data = p3_wqp_data_aoi_formatted, 
                   char_names_crosswalk = p1_char_names_crosswalk,
                   commenttext_missing = c('analysis lost', 'not analyzed', 
                                           'not recorded', 'not collected', 
                                           'no measurement taken'),
                   remove_duplicated_records = TRUE)
  ),
  
  # Create a table that defines parameter-specific data cleaning functions.
  # Cleaning functions should be defined within a named list where the name
  # of each list element is the function name.
  tar_target(
    p3_wqp_param_cleaning_info,
    tibble(
      parameter = c('conductivity', 'temperature'),
      cleaning_fxn = c(clean_conductivity_data, clean_temperature_data))
  ),
  
  # Group the WQP data by parameter group in preparation for parameter-specific
  # data cleaning steps.
  tar_target(
    p3_wqp_data_aoi_clean_grp,
    p3_wqp_data_aoi_clean %>%
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
      fxn_to_use <- p3_wqp_param_cleaning_info %>%
        filter(parameter == unique(p3_wqp_data_aoi_clean_grp$parameter)) %>%
        pull(cleaning_fxn) %>%
        {.[[1]]}
      
      # If applicable, apply parameter-specific cleaning function
      if(length(fxn_to_use) > 0){
        do.call(fxn_to_use, list(wqp_data = p3_wqp_data_aoi_clean_grp))
      } else {.}
    },
    map(p3_wqp_data_aoi_clean_grp)
  ),
  
  # Summarize the number of records associated with each parameter,
  # characteristic name, and harmonized units. The harmonized dataset
  # can be summarized using any combination of columns by passing a
  # different vector of column names in `grouping_cols`.
  tar_target(
    p3_wqp_records_summary_csv,
    summarize_wqp_records(p3_wqp_data_aoi_clean_param, 
                          grouping_cols = c('parameter', 
                                            'CharacteristicName',
                                            'ResultMeasure.MeasureUnitCode'),
                          "3_harmonize/log/wqp_records_summary.csv"),
    format = "file"
  ),
  
  # Save output file containing the harmonized data. The code below can be edited
  # to save the output data to a different file format, but note that a "file"
  # target expects a character string to be returned when the target is built. 
  # This target currently represents the output of the pipeline although more 
  # steps can be added using `p3_wqp_data_aoi_clean_param` as a dependency to 
  # downstream targets.
  tar_target(
    p3_wqp_data_aoi_clean_param_rds,
    {
      fileout <- "3_harmonize/out/harmonized_wqp_data.rds"
      saveRDS(p3_wqp_data_aoi_clean_param, fileout)
      fileout
    }, 
    format = "file"
  )

)

