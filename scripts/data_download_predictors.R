## This is an example of an R script that would be sourced from the main
## markdown script.
##
## Script: 01_download_data.R
## This is script 01 of 06 

## Purpose: To download and store raw data files that can be downloaded 
## directly from source from within R.  
​
## This script includes a function "getfile.function" that takes a URL address
## for a file to be downloaded, along with the path for the raw data directory,
## and saves it in proper path, then unzips it if necessary.
​
## This script is sourced from the main markdown script called "XX.Rmd"
​
## **NOTE**
## This script would ideally only be run once at the outset, then once more
## before manuscript submission to check reproducibility.
​
## Required packages: "here"
​
library(here)
require(hwsdr) # for soil data
library(terra)
library(sf)
​
#################################
# Function for download and unzip
#################################
​
# This is the main workhorse function
​
getfile.function <- function(urlcode, rawdata_path){
  
  ## STEPS ##
  
  # 1. Check if the appropriate destination sub-directory exists, and if not
  # create it.
  # 2. Download file and overwrite any local file with same name
  # 3. Give an error message if the download failed
  # 4. Checks if the downloaded file is a "zip" file, and if so, unzip in
  # approrpiate sub-directory
  
  # designate the sub-directory name and path for this dataset
  # based on the downloaded file name
  subdir_name <- strsplit(basename(urlcode), "\\.")[[1]][1]
  
  destination_directory_path <- paste(rawdata_path, subdir_name, sep = "/")
  
  # Check if destination directory exists, and if not, create
  
  if (!file.exists(destination_directory_path)) {
    # If not, create the directory
    dir.create(destination_directory_path)
    # comment these "cat" lines if you don't want messages
cat(paste0("Directory ", sQuote(subdir_name), " created.\n"))
} else {
  cat(paste0("Directory ", sQuote(subdir_name), " already exists.\n"))
}

# Destination file path to save the downloaded file
# the "basename" function extracts the filename at the end of a path
destination_file <- paste(destination_directory_path, basename(urlcode), sep = "/")

# Download the file using download.file with overwrite
download_status <- download.file(urlcode, destfile = destination_file, mode = "wb", overwrite = TRUE)

# Check the return value and print a corresponding message if failed
if (download_status == 0) {
  cat("File downloaded successfully. Saved at:", destination_file, "\n")
} else {
  cat("Error downloading the file. Check the URL and try again.\n")
}

# If the downloaded file is a "zip" file, then unzip in same directory

if (grepl("\\.zip$", tolower(destination_file))) {
  unzip(destination_file, 
        exdir = destination_directory_path, overwrite = TRUE)
} else { }

# done download and unzip
}
​
#################################
​
## Provide the directory path for directory where all rawdata are stored
​
rawdata_path <- here("hannah", "rawdata")
​
### CATEGORICAL LAYERS
​
## The following geospatial data layers are sourced from the CEC website
## http://www.cec.org/north-american-environmental-atlas
​
# Watersheds
watersheds.url <- "http://www.cec.org/files/atlas_layers/0_reference/0_04_watersheds/watersheds_shapefile.zip"
​
getfile.function(watersheds.url, rawdata_path)
​
# Climate zones
climatezones.url <- "http://www.cec.org/files/atlas_layers/5_climate/5_01_climate_zones/na_climatezones_shapefile.zip"
​
getfile.function(climatezones.url, rawdata_path)
​
# Anthropogenic biomes
anthbiomes.url <- "http://www.cec.org/files/atlas_layers/4_human_influence/4_02_anthropogenic_biomes_2008/anthropogenicbiomes_2008_tif.zip"
​
getfile.function(anthbiomes.url, rawdata_path)
​
# Protected areas **NOTE VERY LARGE FILE > 2GB
protected.url <- "http://www.cec.org/files/atlas_layers/1_terrestrial_ecosystems/1_02_protected_areas_2021/protectedareas_2021_shapefile.zip"
​
getfile.function(protected.url, rawdata_path)
​
# Landcover **NOTE VERY LARGE FILE > 3GB
landcover.url <- "http://www.cec.org/files/atlas_layers/1_terrestrial_ecosystems/1_01_0_land_cover_2020_30m/land_cover_2020_30m_tif.zip"
​
getfile.function(landcover.url, rawdata_path)
​
#### NUMERIC LAYERS
​
## The following are from the Worldclim website (https://www.worldclim.org/data/index.html)
​
# Average temperature ** VERY LARGE FILE > 4GB 
​
temp_avg.url <- "https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_30s_tavg.zip"
​
getfile.function(temp_avg.url, rawdata_path)
​
# Bioclim layers ** EXTREMELY large file > 10GB
​
bioclim.url <- "https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_30s_bio.zip"
​
getfile.function(bioclim.url, rawdata_path)
​
# Elevation
​
elevation.url <- "https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_30s_elev.zip"
​
getfile.function(elevation.url, rawdata_path)
​
#########
## SOILS
#########
​
# Soil data require different approach: we use the "hwsdr" package to interface
# directly with the Harmonized World Soil Database 'HWSD' web services 
# (https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=1247)
​
# package info: https://github.com/bluegreen-labs/hwsdr
​
# Also, we can use a bounding box from another geospatial layer (e.g. shapefile)
# to limit the geographic extent of the downloaded raster data
​
# OPTIONAL: we can import the shapefile of the study region
​
study_region <- sf::st_read(here("hannah", "processed_data", "continental_divide_buffer_boundary.shp"), query = "bbox", quiet = TRUE)
​
# Alternatively we just provide a bounding box vector
​
# Topsoil pH
​
# 0.5 degree resolution
​
# create directory if doesn't exist
​
varname <- "topsoil_ph"
destdir <- paste(rawdata_path, varname, sep = "/")
if (!file.exists(destdir)) {
  # If not, create the directory
  dir.create(destdir)
  # comment these "cat" lines if you don't want messages
  cat(paste0("Directory ", sQuote(destdir), " created.\n"))
} else {
  cat(paste0("Directory ", sQuote(destdir), " already exists.\n"))
}
​
# Note that this creates a file called "HWSD.tif", which we'll need to rename
​
## ** LARGE FILE
​
ws_subset(
  location = sf::st_bbox(study_region),
  param = "T_PH_H2O",
  layer = "D1",
  path = paste(rawdata_path, "topsoil_ph", sep = "/"),
  internal = FALSE
)
​
# Rename files
​
# List all files in the directory
all_files <- list.files(destdir, full.names = TRUE)
​
# Find files containing the string "HWSD"
matching_files <- all_files[grep("HWSD", all_files)]
​
# Rename matching files by replacing "HWSD" with varname
for (old_path in matching_files) {
  new_path <- gsub("HWSD", "topsoil_ph", old_path)
  file.rename(old_path, new_path)
  cat("Renamed:", old_path, "to", new_path, "\n")
}

# Soil temperature
​
# 0_5 cm beneath the surface:
soil_temp_0_5.url <- "https://zenodo.org/records/7134169/SBIO4_0_5cm_Temperature_Seasonality.tif"
​
getfile.function(soil_temp_0_5.url, rawdata_path)

# 5_15 cm beneath the surface:
soil_temp_5_15.url <- "https://zenodo.org/records/7134169/SBIO4_5_15cm_Temperature_Seasonality.tif"
​
getfile.function(soil_temp_5_15.url, rawdata_path)
