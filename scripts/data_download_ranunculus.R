# dir.create("data/")
# dir.create("data/raw")
# dir.create("scripts/")
library(tidyverse)
library(geodata) # won't work when on AAFC VPN
library(terra)
library(raster)
library(sf)
library(rgbif)
# library(CoordinateCleaner)
# install.packages('C:/Users/PilatH/OneDrive - AGR-AGR/Desktop/ClimateNAr_1.2.0.zip', repos=NULL, type='source')
# library(ClimateNAr)
# note: cannot load rgdal and terra at the same time 
  # if using project function from terra (call terra::project)


## Occurrence Data ##


# set up GBIF credentials
# install.packages("usethis")
# usethis::edit_r_environ()

# download occurrence data for Ranunculus glaberrimus
# within North America, between 1950-2024
rgbif::occ_download(
  pred("hasGeospatialIssue", FALSE), 
  pred("hasCoordinate", TRUE),
  pred("continent", "north_america"),
  pred("year", "1940,2024"),
  pred("taxonKey", 3033299), 
  format = "SIMPLE_CSV")

# to check status of download:
occ_download_wait('0067361-231120084113126')

# to access download when it's finished
ran_occ_download <- occ_download_get(key = '0067361-231120084113126', 
                                     path = "data/raw/") %>%
  occ_download_import(key = '0067361-231120084113126', 
                      path = "data/raw/")

# Download Info:
# Username: hpilat
# E-mail: hannahepilat@gmail.com
# Format: SIMPLE_CSV
# Download key: 0067361-231120084113126
# Created: 2024-01-21T21:44:46.652+00:00
# Citation Info:  
#  Please always cite the download DOI when using this data.
# https://www.gbif.org/citation-guidelines
# DOI: 10.15468/dl.3f98w4
# Citation:
#  GBIF Occurrence Download https://doi.org/10.15468/dl.3f98w4 Accessed from R via rgbif (https://github.com/ropensci/rgbif) on 2024-01-21


## Predictor Data ##


# elevation data for North America
elevation_na <- rast("data/raw/northamerica_elevation_cec_2023.tif")

# read in landcover data for North America
lndcvr_na <- rast("data/raw/NA_NALCMS_landcover_2020_30m.tif")

# soil temperature data:
soil_temp_0_5 <- rast("data/raw/SBIO4_0_5cm_Temperature_Seasonality.tif")
soil_temp_5_15 <- rast("data/raw/SBIO4_5_15cm_Temperature_Seasonality.tif")

# soil pH data:
soil_phh2o_0_5 <- geodata::soil_world(var = "phh2o", depth = 5, stat = "mean", 
                             path = "C:\\Users\\PilatH\\OneDrive - AGR-AGR\\Documents\\ranunculus_sdm\\data\\raw", 
                             na.rm = TRUE)

soil_phh2o_5_15 <- geodata::soil_world(var = "phh2o", depth = 15, stat = "mean", 
                              path = "C:\\Users\\PilatH\\OneDrive - AGR-AGR\\Documents\\ranunculus_sdm\\data\\raw", 
                              na.rm = TRUE)


# import global WorldClim average temperature
temp_avg_global <- geodata::worldclim_global(var = "tavg", 
                                             res = "2.5", 
                                             path = "data/raw/",
                                             version = "2.1")

# import global WorldClim precipitation data
precip_global <- geodata::worldclim_global(var = "prec", 
                                           res = "2.5", 
                                           path = "data/raw/", 
                                           version = "2.1")

# read in anthropogenic biome data
anth_biome <- rast("data/raw/anthromes_EqArea.tif")


# read in watersheds data
watersheds_sf <- read_sf("data/raw/watersheds_shapefile/Watersheds_Shapefile/NA_Watersheds/data/watershed_p_v2.shp")
watersheds_vect <- vect(watersheds_sf)

# read in protected areas data
# IUCN categories:
protect_area_IUCN_sf <- read_sf("data/raw/CEC_NA_2021_terrestrial_IUCN_categories.shp")
protect_area_IUCN_vect <- vect(protect_area_IUCN_sf)

# OECMs - what does this mean?
protect_area_OECM_sf <- read_sf("data/raw/CEC_NA_2021_terrestrial_OECMs.shp")
protect_area_OECM_vect <- vect(protect_area_OECM_sf)

# read in North American Climate Zones data
climate_zones_sf <- read_sf("data/raw/North_America_Climate_Zones.shp")
climate_zones_vect <- vect(climate_zones_sf)



# monthly snowpack, NetCDF file requires special software to download?
# snowpack_canada <- rast("data/")

# unified North American soil data
# download.file("https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=1242", 
# paste("C:\\Users\\PilatH\\OneDrive - AGR-AGR\\Documents\\ranunculus_sdm\\data\\",
# "unified_north_american_soil.tif", sep = ""), mode = "wb")

# error with file type?
# unified_soil <- rast("data/unified_north_american_soil.tif")

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
