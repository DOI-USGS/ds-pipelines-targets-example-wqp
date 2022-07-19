# ds-pipelines-targets-example-wqp
An example targets pipeline for pulling data from the Water Quality Portal (WQP)  

## Getting started
To run the pipeline, check that you've installed the `targets` package in R and then run the following lines:  

```r
#install.packages("targets")
library(targets)
tar_make()
```

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

# Unzip the shapefile and read in as an sf object
tar_target(
    p1_shp_sf,
    {
      savedir <- tools::file_path_sans_ext(p1_shp_zip)
      unzip(zipfile = p1_shp_zip, exdir = savedir, overwrite = TRUE)
      sf::st_read(paste0(savedir,"/drb_bnd_arc.shp"), quiet = TRUE)
    }
)
```

## Comments on pipeline design 
This data pipeline is built around the central idea that smaller queries to the WQP are more likely to succeed and therefore, most workflows that pull WQP data would benefit from dividing larger requests into smaller ones. There are many different ways we could have gone about grouping or "chunking" data queries. We use `targets` ["branching"](https://books.ropensci.org/targets/dynamic.html) capabilities to apply (or _map_) our data inventory and download functions over discrete spatial units represented by grids that overlap our area of interest. Another valid approach would have been to generate `targets` branches over units of time, which might work well for applications where we have a defined spatial extent and just want to update the data from time to time. In this pipeline we opted to divide our queries by spatial units so that the pipeline can be readily scaled to the area of interest and because different contributors may add data to WQP at different lags, making it difficult to know when older data are considered "current."

In addition to querying WQP across spatial units, we further split data queries to make calls to the WQP API more manageable. Sites are inventoried separately for each requested characteristic name and then recombined into a single data frame in `p1_wqp_inventory`. Then, prior to actually downloading the data, we bin the sites into distinct download groups such that the total number of records in a given download group does not exceed some maximum threshold (defaults to 250,000 records per group). Finally, we map our data download function over each group and unique characteristic name to download and recombine the data in `p1_wqp_data_aoi`. 

Our intent was to make this data pipeline scalable to larger requests that include more characteristic names, a broader spatial extent, or longer time periods. The data splits described here result in more reasonably-sized queries AND allow us to take advantage of `targets` dependency tracking to efficiently build or update the data pipeline. For example, we do not have to re-download _all_ of the data records from WQP just because we added one additional characteristic name or because a couple new sites were recently added to WQP and detected in our inventory. `targets` will only update those data subsets that become "outdated" by an upstream change. 

One final note about pipeline design - we chose to inventory the WQP by bounding boxes. There are other [valid inputs](https://www.waterqualitydata.us/webservices_documentation/) that can be used to query WQP with the functions `whatWQPdata()` and `readWQPdata()` from the [`dataRetrieval`](https://cran.r-project.org/web/packages/dataRetrieval/vignettes/dataRetrieval.html) R package, including HUC8 identifier (`huc`), state code (`statecode`), etc. There may be considerations for either approach. Querying by bounding box, as we do here, will not find any sites that are missing latitude and longitude parameters. 


<br>  





