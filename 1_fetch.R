p1_targets_list <- list(

# Get common parameter groups and WQP CharacteristicNames
tar_target(
  p1_wqp_params_yml,
  '1_fetch/cfg/wqp_codes.yml',
  format = "file"
),

tar_target(
  p1_wqp_params,
  yaml::read_yaml(p1_wqp_params_yml) 
),

# Get a vector of WQP characteristicNames to match parameter groups of interest
tar_target(
  p1_charNames,
  p1_wqp_params[names(p1_wqp_params) %in% param_groups_select]
)


)
