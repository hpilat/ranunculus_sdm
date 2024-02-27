# dir.create("data/")
# dir.create("data/raw")
# dir.create("scripts/")

library(tidyverse)
library(geodata) # won't work when on AAFC VPN
library(terra)
library(raster)
library(sf)
library(rgbif)
# library(CoordinateCleaner) unsure if I'll be using this
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


## Predictor Data ##

# Present Climate Data: highest resolution from geodata is 0.5 minutes of a degree 
# tidysdm tutorial uses pastclim to access WorldClim data, but only low resolution
  # datasets are available
# attempt to download higher resolution WorldClim data for Canada and USA:
# using worldclim_country worked for Canada, but no values for USA
# seems to be a common and unresolved issue for other geodata users
# can also use 30 degree tiles, but would have to use several to cover our extent
worldclim_tile1 <- geodata::worldclim_tile(var = "bio", # all 19 bioclimatic variables
                                           res = 0.5, # minutes of a degree
                                           path = "data/raw/",
                                           lon = -150,
                                           lat = 50,
                                           version = 2.1)
# tile1 covers -120 to -150 lon and 30 to 60 lat

worldclim_tile2 <- geodata::worldclim_tile(var = "bio", # all 19 bioclimatic variables
                                           res = 0.5, # minutes of a degree
                                           path = "data/raw/",
                                           lon = -102.5,
                                           lat = 50,
                                           version = 2.1)
# tile2 covers -120 to -90 lon, and 30 to 60 lat

# now we need two more tiles to cover the top 10 degrees lat we're missing

worldclim_tile3 <- geodata::worldclim_tile(var = "bio", # all 19 bioclimatic variables
                                           res = 0.5, # minutes of a degree
                                           path = "data/raw/",
                                           lon = -150,
                                           lat = 80,
                                           version = 2.1)

worldclim_tile4 <- geodata::worldclim_tile(var = "bio", # all 19 bioclimatic variables
                                           res = 0.5, # minutes of a degree
                                           path = "data/raw/",
                                           lon = -102.5,
                                           lat = 80,
                                           version = 2.1)


# merge the worldclim tiles together
worldclim_1_2 <- terra::merge(worldclim_tile1, worldclim_tile2, first = TRUE)
worldclim_3_4 <- terra::merge(worldclim_tile3, worldclim_tile4, first = TRUE)
worldclim_tiles <- terra::merge(worldclim_1_2, worldclim_3_4, first = TRUE)

# write to file for reuse in 03_data_prep_ranunculus
writeRaster(worldclim_tiles, filename = "data/processed/worldclim_tiles_combined.tif")


# Future Climate Data: highest resolution from geodata is 2.5 minutes of a degree

cmip_tile1 <- geodata::cmip6_tile(model = "HadGEM3-GC31-LL", 
                                  ssp = "126", 
                                  time = "2061-2080", 
                                  var = "bioc", 
                                  res = 2.5, 
                                  path = "data/raw/", 
                                  lon = -150, 
                                  lat = 50)

# tile1 covers -120 to -150 lon and 30 to 60 lat



# read in anthropogenic biome data
anth_biome <- rast("data/raw/anthromes_EqArea.tif")

# read in North American Climate Zones data
climate_zones_sf <- read_sf("data/raw/North_America_Climate_Zones.shp")
climate_zones_vect <- vect(climate_zones_sf)

# elevation data for North America
elevation_na <- rast("data/raw/northamerica_elevation_cec_2023.tif")

# read in landcover data for North America
lndcvr_na <- rast("data/raw/NA_NALCMS_landcover_2020_30m.tif")

# read in protected areas data
# IUCN categories:
protect_area_IUCN_sf <- read_sf("data/raw/CEC_NA_2021_terrestrial_IUCN_categories.shp")
protect_area_IUCN_vect <- vect(protect_area_IUCN_sf)

# OECMs - what does this mean?
protect_area_OECM_sf <- read_sf("data/raw/CEC_NA_2021_terrestrial_OECMs.shp")
protect_area_OECM_vect <- vect(protect_area_OECM_sf)

# soil pH data:
soil_phh2o_0_5 <- geodata::soil_world(var = "phh2o", depth = 5, stat = "mean", 
                                      path = "C:\\Users\\PilatH\\OneDrive - AGR-AGR\\Documents\\ranunculus_sdm\\data\\raw", 
                                      na.rm = TRUE)

soil_phh2o_5_15 <- geodata::soil_world(var = "phh2o", depth = 15, stat = "mean", 
                                       path = "C:\\Users\\PilatH\\OneDrive - AGR-AGR\\Documents\\ranunculus_sdm\\data\\raw", 
                                       na.rm = TRUE)

# IF geodata package isn't working:
# soil_phh2o_0_5 <- rast("data/raw/soil_world/phh2o_0-5cm_mean_30s.tif")
# soil_phh2o_5_15 <- rast("data/raw/soil_world/phh2o_5-15cm_mean_30s.tif")

# soil temperature data:
soil_temp_0_5 <- rast("data/raw/SBIO4_0_5cm_Temperature_Seasonality.tif")
soil_temp_5_15 <- rast("data/raw/SBIO4_5_15cm_Temperature_Seasonality.tif")

# read in watersheds data
watersheds_sf <- read_sf("data/raw/watersheds_shapefile/Watersheds_Shapefile/NA_Watersheds/data/watershed_p_v2.shp")
watersheds_vect <- vect(watersheds_sf)

## code below not relevant currently, but saving just in case

# import global WorldClim average temperature
# temp_avg_global <- geodata::worldclim_global(var = "tavg", 
#  res = "2.5", 
#  path = "data/raw/",
#  version = "2.1")

# import global WorldClim precipitation data
# precip_global <- geodata::worldclim_global(var = "prec", 
#  res = "2.5", 
#  path = "data/raw/", 
#  version = "2.1")


# future climate predictions Shared Socioeconomic Pathway 126 (no climate policy?)
# cmip6_2021_2040_126 <- geodata::cmip6_tile(lon, lat, model, ssp = "126", time = "2021-2040", 
                           #   var = , res = 2.5, path = )
# cmip6_2041_2060_126 <- geodata::cmip6_tile(lon, lat, model, ssp = "126", time = "2041-2060", 
                           #   var = , res = 2.5, path = )
# cmip6_2061_2080_126 <- geodata::cmip6_tile(lon, lat, model, ssp = "126", time = "2061-2080", 
                           #   var = , res = 2.5, path = )

# future climate predictions Shared Socioeconomic Pathway 245 (low/mid)
# cmip6_2021_2040_245 <- geodata::cmip6_tile(lon, lat, model, ssp = "245", time = "2021-2040", 
                           #   var = , res = 2.5, path = )
# cmip6_2041_2060_245 <- geodata::cmip6_tile(lon, lat, model, ssp = "245", time = "2041-2060", 
                           #   var = , res = 2.5, path = )
# cmip6_2061_2080_245 <- geodata::cmip6_tile(lon, lat, model, ssp = "245", time = "2061-2080", 
                           #   var = , res = 2.5, path = )

# future climate predictions Shared Socioeconomic Pathway 370 (mid/high)
# cmip6_2021_2040_370 <- geodata::cmip6_tile(lon, lat, model, ssp = "370", time = "2021-2040", 
                           #   var = , res = 2.5, path = )
# cmip6_2041_2060_370 <- geodata::cmip6_tile(lon, lat, model, ssp = "370", time = "2041-2060", 
                           #   var = , res = 2.5, path = )
# cmip6_2061_2080_370 <- geodata::cmip6_tile(lon, lat, model, ssp = "370", time = "2061-2080", 
                           #   var = , res = 2.5, path = )

# future climate predictions Shared Socioeconomic Pathway 545 (high)
# cmip6_2021_2040_545 <- geodata::cmip6_tile(lon, lat, model, ssp = "545", time = "2021-2040", 
                           #   var = , res = 2.5, path = )
# cmip6_2041_2060_545 <- geodata::cmip6_tile(lon, lat, model, ssp = "545", time = "2041-2060", 
                           #   var = , res = 2.5, path = )
# cmip6_2061_2080_545 <- geodata::cmip6_tile(lon, lat, model, ssp = "545", time = "2061-2080", 
                           #   var = , res = 2.5, path = )


