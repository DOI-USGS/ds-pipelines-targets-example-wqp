library(targets)

options(tidyverse.quiet = TRUE)
tar_option_set(packages = c('tidyverse', 'lubridate', 'dataRetrieval', 
                            'sf', 'xml2', 'units', 'retry', 'MESS'))

source("1_fetch.R")

# Define the temporal extent of our data pull
# set start_date or end_date to "" to query the earliest or latest available date
start_date <- "2000-01-01"
end_date <- "2020-12-31" 

# Define which parameter groups (and CharacteristicNames) to return from WQP 
# options for parameter groups are represented in first level of 1_fetch/cfg/wqp_codes.yml
param_groups_select <- c('temperature','conductivity')

# Specify coordinates that define the spatial area of interest
# lat/lon are referenced to WGS84
coords_lon <- c(-77.063, -75.333, -75.437)
coords_lat <- c(40.547, 41.029, 39.880)

# Specify arguments to WQP queries
# see https://www.waterqualitydata.us/webservices_documentation for more information 
wqp_args <- list(sampleMedia = c("Water","water"),
                 siteType = "Stream",
                 # return sites with at least one data record
                 minresults = 1, 
                 startDateLo = start_date,
                 startDateHi = end_date)

# [Optional] variable that can be edited to force rebuild of the entire data pipeline,
# including inventory, download, and harmonization steps. Leaving as is will use the
# pipeline's built-in behavior to only re-inventory and re-download data for subsets
# of the data that have changed. 
last_forced_build <- "2022-07-01"

# Return the complete list of targets
c(p1_targets_list)


