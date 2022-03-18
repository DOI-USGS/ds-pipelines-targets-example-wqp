# ds-pipelines-targets-example-wqp
An example targets pipeline for pulling data from the Water Quality Portal (WQP)

## Defining the spatial area of interest
In this workflow we define our spatial area of interest from a set of coordinates that we define in the `_targets.R` file. These coordinates represent the vertices of a simple triangle polygon that represents the spatial extent of our WQP data pull. 

We could also use existing watershed boundaries or another polygon from an external data source to define our area of interest. For example, we could replace the targets `p1_AOI` and `p1_AOI_sf` with targets that download and read in an external shapefile:

```
# Download a shapefile containing the Delaware River Basin boundaries
# We changed the storage format for this target to format = "file" so that tar_make() will
# track this target and automatically re-run any downstream targets if the zip file changes. 
# A file target must return a character vector indicating the path of local files and/or
# directories. To return a character vector with the local file path, we call download_function(),
# which is a wrapper function for utils::download.file that returns the character vector that 
# is used for the fileout argument.
tar_target(
  p1_shp_zip,
  download_file("https://www.state.nj.us/drbc/library/documents/GIS/drbbnd.zip",
                fileout = "1_fetch/out/drbbnd.zip", 
                mode = "wb", quiet = TRUE),
  format = "file"
),

# Unzip the shapefile and read in as an sf object
tar_target(
  p1_shp_sf,
  {
    unzip(zipfile = p1_shp_zip, exdir = "scratch/drbbnd", overwrite = TRUE)
    sf::st_read("scratch/drbbnd/drb_bnd_arc.shp", quiet = TRUE)
  }
)
  
```

