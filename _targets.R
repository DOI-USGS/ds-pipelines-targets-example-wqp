library(targets)

options(tidyverse.quiet = TRUE)
tar_option_set(packages = c('tidyverse', 'lubridate', 'dataRetrieval'))

source("1_fetch.R")

# Define the temporal extent of our data pull
start_date <- '2015-01-01'
end_date <- '2015-12-31'

# The spatial extent of our data pull is the Delaware River Basin (DRB), so 
# here we will define the minor HUCs (hydrologic unit codes) that make up the 
# DRB to pass to data download functions in dataRetrieval. 
# The DRB is represented by subregion '0204' (https://water.usgs.gov/GIS/huc_name.html)
drb_huc8s <- c('02040101','02040102','02040103','02040104','02040105','02040106',
               '02040201','02040202','02040203','02040204','02040205','02040206','02040207')

# Define which parameter groups (and CharacteristicNames) to return from WQP 
# options for parameter groups are represented in first level of 1_fetch/cfg/wqp_codes.yml
param_groups_select <- c('nitrate','conductivity')


# Return the complete list of targets
c(p1_targets_list)


