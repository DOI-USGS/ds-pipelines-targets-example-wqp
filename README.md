# ds-pipelines-targets-example-wqp
An example targets pipeline for pulling data from the Water Quality Portal (WQP)  

## Getting started
To run the pipeline, check that you've installed the `targets` package in R and then run the following lines:  

```r
#install.packages("targets")
library(targets)
tar_make()
```

## Basic pipeline structure

This pipeline is divided into three phases that divide the workflow:  
  
1) **Inventory** what sites and records are available in the WQP  
2) **Download** the inventoried data  
3) Clean or **harmonize** the downloaded data to prepare the dataset for further analysis  

Each phase of the pipeline contains `log` and `out` directories where different types of files are saved during the pipeline build. In this workflow we refer to `log` files as files that are meant to track pipeline changes and help the user understand the outcomes from the pipeline build. It can be helpful to "commit" or check these files into version control because later diffs would show which files were updated, making it easier to keep track of data changes over time. Additional pipeline metadata can be accessed using `tar_meta()`. 


## Customizing the WQP pipeline

The `targets` package (https://books.ropensci.org/targets/) provides data pipeline tools that allow us to take advantage of modular functions, dependency tracking, and an automated workflow to pull data from the WQP. The basic ingredient of a `targets` workflow is a target script file named `_targets.R` that is used to define and configure all of the pipeline connections. This WQP pipeline is structured so that various inputs - including the date range, spatial extent, parameters of interest, and/or specific arguments to pass along to WQP queries - can all be modified within the `_targets.R` file.


### Defining the spatial area of interest
In this workflow we define our spatial area of interest from a set of coordinates that we define in the `_targets.R` file. These coordinates represent the vertices of a simple triangle polygon that covers the spatial extent of our WQP data pull. 

We could also use existing watershed boundaries or another polygon from an external data source to define our area of interest. For example, we could replace the targets [`p1_AOI`](https://github.com/USGS-R/ds-pipelines-targets-example-wqp/blob/99a90c159bcebc4d5ac2e90fbc85734547217a4a/1_inventory.R#L40-L44) and [`p1_AOI_sf`](https://github.com/USGS-R/ds-pipelines-targets-example-wqp/blob/99a90c159bcebc4d5ac2e90fbc85734547217a4a/1_inventory.R#L47-L52) with targets that download and read in an external shapefile:  

```r
# Download a shapefile containing the Delaware River Basin boundaries
# We changed the storage format for this target to format = "file" so that tar_make() will
# track this target and automatically re-run any downstream targets if the zip file changes. 
# A file target must return a character vector indicating the path of local files and/or
# directories. Below, we include all of the code needed to build the target between {} and 
# return the variable fileout to satisfy the format = "file" requirements. Running the 
# command tar_load(p1_shp_zip) should display the string used to define fileout.
  tar_target(
    p1_shp_zip,
    {
      # mode is a character string indicating the mode used to write the file; see 
      # ??utils::download.file for details.
      fileout <- "1_inventory/out/drbbnd.zip"
      utils::download.file("https://www.state.nj.us/drbc/library/documents/GIS/drbbnd.zip",
                  destfile = fileout, 
                  mode = "wb", quiet = TRUE)
      fileout
    },
    format = "file"
  ),

# Unzip the shapefile and read in as an sf polygon object
  tar_target(
    p1_AOI_sf,
    {
      savedir <- tools::file_path_sans_ext(p1_shp_zip)
      unzip(zipfile = p1_shp_zip, exdir = savedir, overwrite = TRUE)
      sf::st_read(paste0(savedir,"/drb_bnd_arc.shp"), quiet = TRUE) %>%
        sf::st_cast(.,"POLYGON")
    }
  ),
```

### Changing the parameter list
This workflow comes with a configuration file containing common water quality parameter groups and associated WQP characteristic names ("1_inventory/cfg_wqp_codes.yml"). This configuration file is meant to provide a starting place for an analysis and does not represent a definitive list of characteristic names. The yml file can be edited to omit certain characteristic names and include others, to change top-level parameter names, or to customize parameter groupings. 

### Changing the date range
Customize the temporal extent of the WQP data pull by editing the variables `start_date` and `end_date` in `_targets.R`. Queries can accept `start_date` and `end_date` in `"YYYY-MM-DD"` format, or can be set to `""` to request the earliest or latest available dates, respectively. 

### Forcing a rebuild of the data inventory
`targets` automates workflows by only re-running code when necessary, and skips over targets that are considered up to date. A target is considered out of date if its upstream dependencies have changed, which will trigger a rebuild of that target. The pipeline's built-in behavior is to only re-inventory WQP data for subsets of the input data that have changed. For example, if the area of interest were expanded and now spans more grid cells, `targets` would build the branches of `p1_wqp_inventory` associated with the new grids, but would not rebuild existing branches that were built previously. Efficient rebuilds save time by avoiding situations where we rerun the same code many times, either because we lost track of whether a code step has already been run, or because we made a small change to the input data.

If you want to _force_ a rebuild of the entire data inventory, however, we outline two ways to do that. First, you can delete metadata records and "invalidate" a target using `tar_invalidate()`. Invalidating a target will trigger a fresh build of that target, and will also cause all downstream targets to appear out of date, leading `targets` to rebuild those downstream targets as well. 

```r
tar_invalidate(p1_wqp_inventory)
```

Second, you could add an additional dependency to `p1_wqp_inventory` (e.g., `last_forced_build` below). In the example code below, `last_forced_build` is defined with the other pipeline variables in `_targets.R` and if updated, `p1_wqp_inventory` would be considered out of date, triggering a rebuild of the inventory target. 

```r
# In _targets.R:

# [Optional] variable that can be edited to force rebuild of the data inventory.
last_forced_build <- "2022-07-01"
```

```r
# In 1_inventory.R:
    
  p1_wqp_inventory,
  {
    # inventory_wqp() requires grid and char_names as inputs, but users can 
    # also pass additional arguments to WQP, e.g. sampleMedia or siteType, using 
    # wqp_args. See documentation in 1_inventory/src/get_wqp_inventory.R for further
    # details. Below, wqp_args and last_forced_build are dependencies that get
    # defined in _targets.R. 
    last_forced_build
    inventory_wqp(grid = p1_global_grid_aoi,
                  char_names = p1_char_names,
                  wqp_args = wqp_args)
    },
    pattern = cross(p1_global_grid_aoi, p1_char_names),
    error = "continue"
  ),
```
If using this `last_forced_build` option, be aware that downstream pipeline steps, including data download and harmonization, would get rebuilt only IF the inventory outputs change compared to their previous state. Therefore, using and editing this variable does not guarantee that updated values will be downloaded from WQP if the inventory, including site ids and number of records, has not changed from the previous build.

### Harmonizing water quality data
We currently include a few select data cleaning steps in the `3_harmonize` phase of the pipeline. The harmonization steps included here are not comprehensive and are meant to highlight a few common data cleaning routines and show how they could be implemented within this pipeline. 

The first harmonization step is to format columns, which includes converting select columns to class `numeric` and optionally, dropping undesired columns from the downloaded dataset. WQP variables intended to represent numeric values will occasionally contain non-numeric values (e.g. when `"*Non-detect"` appears in column `ResultMeasureValue`). The original entries are retained in a separate column for reference and non-numeric values are replaced with `NA`. If you want to see the unexpected, non-numeric values that are converted to `NA` for columns that we formatted as numeric, you can quickly do that after the pipeline has run using code like the example using `ResultMeasureValue` below:  

```r
library(dplyr)
tar_load(p3_wqp_data_aoi_formatted)
p3_wqp_data_aoi_formatted %>%
  dplyr::filter(is.na(ResultMeasureValue),
                !is.na(ResultMeasureValue_original)) %>%
  dplyr::pull(ResultMeasureValue_original)

```

The data cleaning functions included in this pipeline can be categorized as general cleaning functions that are applicable to all WQP data records, or parameter-specific cleaning functions that apply to individual parameter groups. An example of a parameter-specific cleaning step is to harmonize temperature units to a consistent value such as degrees Celsius. General data cleaning functions get applied when building the `p3_wqp_data_aoi_clean` target and parameter-specific cleaning functions are applied in `p3_wqp_data_aoi_clean_param`. The pipeline currently contains relatively simple functions for harmonizing conductivity and temperature data. The conductivity or temperature functions can be edited to accommodate project-specific needs or preferences for data harmonization. To include steps for harmonizing another parameter group, you can create a new function and add it to the list of functions in `p3_wqp_param_cleaning_info`. For example, we could add harmonization steps specific to nitrate in the example below:

```r
# In a file named clean_nitrate_data.R within the 3_harmonize/src directory:

clean_nitrate_data <- function(wqp_data){
 wqp_data_out <- wqp_data %>%
    # Convert units from ug/L to mg/L
    mutate(ResultMeasureValue = if_else(!is.na(ResultMeasureValue) & 
                                          grepl("ug/l", ResultMeasure.MeasureUnitCode, ignore.case = TRUE),
                                        (ResultMeasureValue * 0.001), ResultMeasureValue),
           ResultMeasure.MeasureUnitCode = if_else(!is.na(ResultMeasureValue) & 
                                          grepl("ug/l", ResultMeasure.MeasureUnitCode, ignore.case = TRUE),
                                          "mg/L", ResultMeasure.MeasureUnitCode)) 
  
  return(wqp_data_out)
}

```

```r
# In 3_harmonize.R:

# Remember to add the new nitrate-specific cleaning function to the source calls at the top of 3_harmonize.R
source("3_harmonize/src/clean_nitrate_data.R")

...

# Add the new function to the list of parameter-specific cleaning functions in p3_wqp_param_cleaning_info
  tar_target(
    p3_wqp_param_cleaning_info,
    tibble(
      parameter = c('conductivity', 'temperature', 'nitrate'),
      cleaning_fxn = c(clean_conductivity_data, clean_temperature_data, clean_nitrate_data)
    )
  ),


```

If the pipeline has been run previously, only the nitrate data will be impacted by this addition and the `targets` branches corresponding to the conductivity and temperature data subsets will be skipped over when building `p3_wqp_data_aoi_clean_param`.


## Comments on pipeline design 
This data pipeline is built around the central idea that smaller queries to the WQP are more likely to succeed and therefore, most workflows that pull WQP data would benefit from dividing larger requests into smaller ones. There are many different ways we could have gone about grouping or "chunking" data queries. We use `targets` ["branching"](https://books.ropensci.org/targets/dynamic.html) capabilities to apply (or _map_) our data inventory and download functions over discrete spatial units represented by grids that overlap our area of interest. Another valid approach would have been to generate `targets` branches over units of time, which might work well for applications where we have a defined spatial extent and just want to update the data from time to time. In this pipeline we opted to divide our queries by spatial units so that the pipeline can be readily scaled to the area of interest and because different contributors may add data to WQP at different lags, making it difficult to know when older data are considered "current."

In addition to querying WQP across spatial units, we further split data queries to make calls to the WQP API more manageable. Sites are inventoried separately for each requested characteristic name and then recombined into a single data frame in `p1_wqp_inventory`. Then, prior to actually downloading the data, we bin the sites into distinct download groups for each grid cell and characteristic name such that the total number of records in a given download group does not exceed some maximum threshold (defaults to 250,000 records per group). Finally, we map our data download function over each group to download and recombine the data in `p1_wqp_data_aoi`. 

Our intent was to make this data pipeline scalable to larger requests that include more characteristic names, a broader spatial extent, or longer time periods. The data splits described here result in more reasonably-sized queries AND allow us to take advantage of `targets` dependency tracking to efficiently build or update the data pipeline. For example, we do not have to re-download _all_ of the data records from WQP just because we added one additional characteristic name or because a couple new sites were recently added to WQP and detected in our inventory. `targets` will only update those data subsets that become "outdated" by an upstream change. 

One final note about pipeline design - we chose to inventory the WQP by bounding boxes. There are other [valid inputs](https://www.waterqualitydata.us/webservices_documentation/) that can be used to query WQP with the functions `whatWQPdata()` and `readWQPdata()` from the [`dataRetrieval`](https://cran.r-project.org/web/packages/dataRetrieval/vignettes/dataRetrieval.html) R package, including HUC8 identifier (`huc`), state code (`statecode`), etc. There may be considerations for either approach. Querying by bounding box, as we do here, will not find any sites that are missing latitude and longitude parameters.



## Acknowledgements
The data harmonization steps included in this pipeline build off of code and ideas for cleaning WQP data developed by [Jennifer Murphy](https://www.usgs.gov/staff-profiles/jennifer-murphy) and [Megan Shoda](https://www.usgs.gov/staff-profiles/megan-shoda).


<br>  





