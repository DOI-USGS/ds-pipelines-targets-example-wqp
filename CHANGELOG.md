## Unreleased
 * Add documentation that describes how to adapt the template pipeline for large
 data pulls
 * Prepare the repository for migration to DOI-USGS GitHub organization
 * Add targets to download the site metadata
 * Add timeout settings and retry handling to data download step to make large
 data pulls more fault-tolerant
 * Minor changes to make inventory step robust to empty queries (e.g. if the 
 user-specified area of interest includes grid cells that do not contain WQP 
 data)

## v0.1.0 
 * Initial release of WQP template pipeline
