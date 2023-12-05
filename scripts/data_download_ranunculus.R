# dir.create("data/")
# dir.create("scripts/")
library(tidyverse)
library(geodata)
library(bcmaps)
library(bcdata)
library(terra)
library(raster)
library(rgbif)
library(CoordinateCleaner)
# note: cannot load rgdal and terra at the same time 
  # if using project function from terra (call terra::project)

## Extent ##

# read in the boundaries for British Columbia (BC)
bc_extent <- bcmaps::bc_bbox(class = ("raster"), crs = "EPSG:4326")
# create a SpatExtent for BC based on the bc_extent object
# cannot coerce Extent object to SpatRaster
bc_spatextent <- ext(bc_extent)
# now create a regular SpatRaster for bc_extent
bc_extent_rast <- rast(bc_spatextent)
 

## Occurrence Data ##

# set up GBIF credentials
# install.packages("usethis")
usethis::edit_r_environ()

# download occurrence data for Ranunculus glaberrimus
rgbif::occ_download(
  pred("hasGeospatialIssue", FALSE), 
  pred("hasCoordinate", TRUE), 
  pred("basisOfRecord", "HUMAN_OBSERVATION"),
  pred("country", "CA"),
  pred("taxonKey", 3033299), 
  format = "SIMPLE_CSV")

# to check status of download:
occ_download_wait('0019946-231120084113126')

# to access download when it's finished
ran_occ_download <- occ_download_get('0019946-231120084113126') %>%
  occ_download_import()

# Download Info:
# Username: hpilat
# E-mail: hannahepilat@gmail.com
# Format: SIMPLE_CSV
# Download key: 0019946-231120084113126
# Created: 2023-12-05T18:28:14.357+00:00
# Citation Info:  
 # Please always cite the download DOI when using this data.
# https://www.gbif.org/citation-guidelines
# DOI: 10.15468/dl.cwqmqu
# Citation:
 #  GBIF Occurrence Download https://doi.org/10.15468/dl.cwqmqu Accessed from R 
    # via rgbif (https://github.com/ropensci/rgbif) on 2023-12-05

## Predictor Data ##

# read in BEC map from bcmaps
bc_bec <- bcmaps::bec(ask = interactive(), force = FALSE)

# elevation data for Canada
elevation_canada <- geodata::elevation_30s(country = "CAN", 
                                           path = "C:\\Users\\PilatH\\OneDrive - AGR-AGR\\Documents\\ranunculus_sdm\\data", 
                                           na.rm = TRUE)

# read in landcover data for North America
lndcvr_na <- rast("data/CAN_NALCMS_landcover_2020_30m.tif")

# soil temperature data:
soil_temp_0_5 <- rast("data/SBIO4_Temperature_Seasonality_0_5cm.tif")
soil_temp_5_15 <- rast("data/SBIO4_Temperature_Seasonality_5_15cm.tif")

# world soil pH data: 
# had an error with geodata package, said server was down for maintenance
# https://files.isric.org/soilgrids/latest/data_aggregated/1000m/phh2o/

soil_phh2o_0_5 <- rast("data/phh2o_0-5cm_mean_1000.tif")

soil_phh2o_5_15 <- rast("data/phh2o_5-15cm_mean_1000.tif")

# soil_phh2o_0_5 <- soil_world(var = "phh2o", depth = 5, stat = "mean", 
                             # path = "C:\\Users\\PilatH\\OneDrive - AGR-AGR\\Documents\\ranunculus_sdm\\data", 
                             # na.rm = TRUE)

# soil_phh2o_5_15 <- soil_world(var = "phh2o", depth = 15, stat = "mean", 
                             # path = "C:\\Users\\PilatH\\OneDrive - AGR-AGR\\Documents\\ranunculus_sdm\\data", 
                             # na.rm = TRUE)


# unified North American soil data
# download.file("https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=1242", 
              # paste("C:\\Users\\PilatH\\OneDrive - AGR-AGR\\Documents\\ranunculus_sdm\\data\\",
                   # "unified_north_american_soil.tif", sep = ""), mode = "wb")

# error with file type?
# unified_soil <- rast("data/unified_north_american_soil.tif")

# climate data, monthly averages from worldclim
# average monthly temperature
tavg_canada <- geodata::worldclim_country(country = "Canada", 
                                          var = "tavg", 
                                          path = "C:\\Users\\PilatH\\OneDrive - AGR-AGR\\Documents\\ranunculus_sdm\\data", 
                                          version = "2.1", 
                                          na.rm = TRUE)

# total precipitation
prec_canada <- geodata::worldclim_country(country = "Canada", 
                                          var = "prec", 
                                          path = "C:\\Users\\PilatH\\OneDrive - AGR-AGR\\Documents\\ranunculus_sdm\\data", 
                                          version = "2.1", 
                                          na.rm = TRUE)

# monthly snowpack, NetCDF file requires special software to download?
# snowpack_canada <- rast("data/")

# future climate predictions Shared Socioeconomic Pathway 126 (no climate policy?)
cmip6_2021_2040_126 <- geodata::cmip6_tile(lon, lat, model, ssp = "126", time = "2021-2040", 
                              var = , res = 2.5, path = )
cmip6_2041_2060_126 <- geodata::cmip6_tile(lon, lat, model, ssp = "126", time = "2041-2060", 
                              var = , res = 2.5, path = )
cmip6_2061_2080_126 <- geodata::cmip6_tile(lon, lat, model, ssp = "126", time = "2061-2080", 
                              var = , res = 2.5, path = )

# future climate predictions Shared Socioeconomic Pathway 245 (low/mid)
cmip6_2021_2040_245 <- geodata::cmip6_tile(lon, lat, model, ssp = "245", time = "2021-2040", 
                              var = , res = 2.5, path = )
cmip6_2041_2060_245 <- geodata::cmip6_tile(lon, lat, model, ssp = "245", time = "2041-2060", 
                              var = , res = 2.5, path = )
cmip6_2061_2080_245 <- geodata::cmip6_tile(lon, lat, model, ssp = "245", time = "2061-2080", 
                              var = , res = 2.5, path = )

# future climate predictions Shared Socioeconomic Pathway 370 (mid/high)
cmip6_2021_2040_370 <- geodata::cmip6_tile(lon, lat, model, ssp = "370", time = "2021-2040", 
                              var = , res = 2.5, path = )
cmip6_2041_2060_370 <- geodata::cmip6_tile(lon, lat, model, ssp = "370", time = "2041-2060", 
                              var = , res = 2.5, path = )
cmip6_2061_2080_370 <- geodata::cmip6_tile(lon, lat, model, ssp = "370", time = "2061-2080", 
                              var = , res = 2.5, path = )

# future climate predictions Shared Socioeconomic Pathway 545 (high)
cmip6_2021_2040_545 <- geodata::cmip6_tile(lon, lat, model, ssp = "545", time = "2021-2040", 
                              var = , res = 2.5, path = )
cmip6_2041_2060_545 <- geodata::cmip6_tile(lon, lat, model, ssp = "545", time = "2041-2060", 
                              var = , res = 2.5, path = )
cmip6_2061_2080_545 <- geodata::cmip6_tile(lon, lat, model, ssp = "545", time = "2061-2080", 
                              var = , res = 2.5, path = )

# future land use change data
# SSP1, RCP19 from years 2020 to 2100
lulc_2020_SSP1_RCP19 <- rast("data/")
lulc_2025_SSP1_RCP19 <-
lulc_2030_SSP1_RCP19 <- 
lulc_2035_SSP1_RCP19 <-
lulc_2040_SSP1_RCP19 <- 
lulc_2045_SSP1_RCP19 <- 
lulc_2050_SSP1_RCP19 <- 
lulc_2055_SSP1_RCP19 <- 
lulc_2060_SSP1_RCP19 <- 
lulc_2065_SSP1_RCP19 <- 
lulc_2070_SSP1_RCP19 <- 
lulc_2075_SSP1_RCP19 <- 
lulc_2080_SSP1_RCP19 <- 
lulc_2085_SSP1_RCP19 <- 
lulc_2090_SSP1_RCP19 <- 
lulc_2095_SSP1_RCP19 <- 
lulc_2100_SSP1_RCP19 <- 
