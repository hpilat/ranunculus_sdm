# This script prepares spatial extent, occurrence records, and predictor data
  # for input into the tidysdm pipeline
# Please first run these scripts in the following order:
  # 01_data_download_ranunculus.R
  # 02_continental_divide.Rmd
  # 03_extents.R

library(tidyverse)
library(sf)
library(terra)

### Spatial Extents ##
# required in order for projection code to run for tidysdm_ranunculus_multirast.R

# North American extent (west coast to continental divide)
# new geographic extent created in continental_divide.Rmd
# read in shapefile so we can calculate the area
na_bound_sf <- read_sf("data/raw/continental_divide_buffer_boundary.shp")
plot(na_bound_sf)
crs(na_bound_sf) # WGS84

# vectorize the na_bound sf object so it can be rasterized
# need this empty raster as a base layer for tidysdm pipeline
na_bound_vect <- vect(na_bound_sf)

# create an empty raster based on study extent in order to rasterize na_bound
# to use as a basemap for TidySDM
temprast <- rast(na_bound_vect, ncols = 12247, nrows = 8024)
na_bound_rast <- rasterize(na_bound_vect, temprast)

# write empty raster to file
na_bound_rast <- writeRaster(na_bound_rast, filename = "data/processed/na_bound_rast.tif", overwrite = TRUE)

# read in na_bound_rast
na_bound_rast <- rast("data/processed/na_bound_rast.tif")


# when testing out the code, we found R would crash with the original extent
# create new bounds for reduced extent
# first need to bring in occurrence data
# select only the relevant columns: ID column, longitude, latitude
ran_occ <- dplyr::select(ran_occ_download, gbifID, 
                         decimalLongitude, decimalLatitude) # dataframe

# create a SpatVector object for the occurrence data
ran_occ_vect <- vect(ran_occ, geom = c("decimalLongitude", "decimalLatitude"), 
                     crs = "EPSG:4326", keepgeom = FALSE)

# cast coordinates into an sf object and set its CRS to WGS84
ran_occ_sf <- st_as_sf(ran_occ, coords = c("decimalLongitude", "decimalLatitude"))
# set CRS to WGS84
st_crs(ran_occ_sf) <- 4326

xlims <- c(ext(ran_occ_sf)$xmin - 2, ext(ran_occ_sf)$xmax + 2)
ylims <- c(ext(ran_occ_sf)$ymin - 2, ext(ran_occ_sf)$ymax + 15)
# additional space north to account for potential northward shifts

# xlims <- c(-130, -102.5)
# ylims <- c(30, 70)

# now crop and mask all layers:
extent.test <- terra::ext(xlims, ylims)
na_bound_rast <- crop(na_bound_rast, extent.test)
na_bound_vect <- crop(na_bound_vect, extent.test)
na_bound_sf <- st_crop(na_bound_sf, extent.test)
ran_occ_sf <- st_crop(ran_occ_sf, extent.test)

# write na_bound_rast to file for reuse
writeRaster(na_bound_rast, filename = "data/processed/na_bound_rast_new.tif", overwrite = TRUE)

# write na_bound_vect to file for use in tidysdm as a mask
writeVector(na_bound_vect, filename = "data/processed/na_bound_vect.shp", overwrite = TRUE)
plot(na_bound_vect)

# read in na_bound_vect
na_bound_vect <- vect("data/processed/na_bound_vect.shp")

# now use na_bound_masked as a mask for ran_occ_vect
ran_occ_vect_masked <- mask(ran_occ_vect, na_bound_vect)

# convert to sf object for use in modelling scripts
ran_occ_sf <- st_as_sf(ran_occ_vect_masked)

# write ran_occ_sf to file for reuse
st_write(ran_occ_sf, dsn = "data/processed/ran_occ_sf.shp", append = FALSE)


# study area calculations:
# read in sf object with new bounds:
# reproject CRS to BC Albers (equal area projection, EPSG:3005) for calculating area
na_bound_area <- st_transform(na_bound_sf, "EPSG:3005")
na_bound_area <- st_set_crs(na_bound_sf, "EPSG:3005")
# calculate study area, in m^2 (default)
na_bound_area <- st_area(na_bound_sf) # 3.83e+12 m^2
# convert from m^2 to km^2
na_bound_area <- st_area(na_bound_sf)/1000000
na_bound_area <- units::set_units(st_area(na_bound_sf), km^2) # 3 831 703  km^2


# Skeetchestn territory:
# SNRC provided shapefile of Skeetchestn traditional territory
# Read in Skeetchestn territory shapefile as sf object first, so we can calculate
# the area in km^2
skeetch_sf <- read_sf("data/raw/SkeetchestnTT_2020/SkeetchestnTT_2020.shp")
plot(skeetch_sf)
crs(skeetch_sf) # BC Albers, NAD83
skeetch_area <- st_area(skeetch_sf) # 7e+09 m^2
# convert from m^2 to km^2
skeetch_area <- st_area(skeetch_sf)/1000000
skeetch_area <- units::set_units(st_area(skeetch_sf), km^2) #6996 km^2

# Vectorize this shapefile so it can be used to mask model outputs from larger extent
# to view skeetch territory
skeetch_vect <- vect("data/raw/SkeetchestnTT_2020/SkeetchestnTT_2020.shp")
# reproject to WGS84
skeetch_vect <- project(skeetch_vect, "EPSG:4326")


## Predictor Data ##


## WorldClim data, for use in Bioclim model:

# read in WorldClim tiles raster:
worldclim_tiles <- rast("data/processed/worldclim_tiles_combined.tif")
worldclim_tiles # CRS and resolution match what we need
# crop the extent of the land mask to match our study's extent
worldclim_cropped <- crop(worldclim_tiles, na_bound_vect)
# mask to the polygon
worldclim_masked <- mask(worldclim_cropped, na_bound_vect)

# write to file for reuse in 05_tidysdm_bioclim_30s.R
writeRaster(worldclim_masked, filename = "data/processed/worldclim_masked.tif", overwrite = TRUE)

## Numeric Rasters:


# reproject soil temperature SpatRasters to WGS84
soil_temp_0_5 <- terra::project(soil_temp_0_5, "EPSG:4326",
                                method = "bilinear")
soil_temp_5_15 <- terra::project(soil_temp_5_15, "EPSG:4326",
                                 method = "bilinear")

# crop soil temperature SpatRaster to North American extent
soil_temp_0_5 <- crop(soil_temp_0_5, na_bound_vect)
soil_temp_5_15 <- crop(soil_temp_5_15, na_bound_vect)


# write processed data to file for faster computation
writeRaster(soil_temp_0_5, filename = "data/processed/soil_temp_0_5.tif", overwrite = TRUE)
writeRaster(soil_temp_5_15, filename = "data/processed/soil_temp_5_15.tif", overwrite = TRUE)

# read in soil temperature data from file:
soil_temp_0_5 <- rast("data/processed/soil_temp_0_5.tif")
soil_temp_5_15 <- rast("data/processed/soil_temp_5_15.tif")
# had issues with soil_temp_5_15 saving to file, need to crop again
soil_temp_5_15 <- crop(soil_temp_5_15, na_bound_vect)


# reproject soil pH SpatRasters to WGS84
soil_phh2o_0_5 <- terra::project(soil_phh2o_0_5, "EPSG:4326",
                                 method = "bilinear")
soil_phh2o_5_15 <- terra::project(soil_phh2o_5_15, "EPSG:4326",
                                 method = "bilinear")

# crop pH SpatRaster to North American extent
soil_phh2o_0_5 <- crop(soil_phh2o_0_5, na_bound_vect)
soil_phh2o_5_15 <- crop(soil_phh2o_5_15, na_bound_vect)

# write processed soil pH data to file for faster computation
writeRaster(soil_phh2o_0_5, filename = "data/processed/soil_phh2o_0_5.tif", overwrite = TRUE)
writeRaster(soil_phh2o_5_15, filename = "data/processed/soil_phh2o_5_15.tif", overwrite = TRUE)

# read in soil pH data from file
soil_phh2o_0_5 <- rast("data/processed/soil_phh2o_0_5.tif")
soil_phh2o_5_15 <- rast("data/processed/soil_phh2o_5_15.tif")

# reproject elevation data to WGS84
elevation_na <- terra::project(elevation_na, "EPSG:4326", method = "bilinear")

# resample elevation_na to change resolution
elevation_na <- terra::resample(elevation_na, soil_temp_0_5)

# crop elevation data to study area extent
elevation_na <- crop(elevation_na, na_bound_vect)

# write elevation_na to file for easier reuse
writeRaster(elevation_na, filename = "data/processed/elevation_na.tif", overwrite = TRUE)

# read in elevation data
elevation_na <- rast("data/processed/elevation_na.tif")


## Categorical Rasters


# aggregate landcover data so it can be reprojected and cropped
lndcvr_na_agg <- aggregate(lndcvr_na, fact = 15)

# write aggregated landcover data to file for easier reuse
lndcvr_na_agg <-writeRaster(lndcvr_na_agg, 
                            filename = "data/processed/lndcvr-north-america_agg.tif", 
                            overwrite = TRUE)

# create SpatRaster of aggregated landcover data from new file
lndcvr_na_agg <- rast("data/processed/lndcvr-north-america_agg.tif")

# reproject landcover North America data to WGS84
lndcvr_na_agg <- terra::project(lndcvr_na_agg, "EPSG:4326", method = "near")

# crop landcover North America data to BC's extent
lndcvr_na <- crop(lndcvr_na_agg, na_bound_vect)

# resample landcover data to change resolution
lndcvr_na <- resample(lndcvr_na, soil_temp_0_5)

# create file of processed landcover data for faster re-use
writeRaster(lndcvr_na, filename = "data/processed/lndcvr_na.tif", overwrite = TRUE)

# read in processed landcover data from new file created above
lndcvr_na <- rast("data/processed/lndcvr_na.tif")

# reproject anthropogenic biomes data to WGS84
anth_biome <- project(anth_biome, "EPSG:4326")

# resample anth_biome to change resolution
anth_biome <- resample(anth_biome, soil_temp_0_5)

# crop anth_biome to study extent
anth_biome <- crop(anth_biome, na_bound_vect)

# write anth_biome to file for faster computation
writeRaster(anth_biome, filename = "data/processed/anth_biome.tif", overwrite = TRUE)

# read in processed anth_biome raster
anth_biome <- rast("data/processed/anth_biome.tif")

# create an empty raster template for shapefiles (currently vector objects)
# create a temporary raster with number of columns and rows from other rasters
dim(lndcvr_na)
temprast <- rast(climate_zones_vect, ncols = 12247, nrows = 8024)

# create raster from SpatVector and structure of temporary raster
climate_zones <- rasterize(climate_zones_vect, temprast, field = "Climate")
climate_zones
plot(climate_zones)

# need to reproject, resample, and crop to match specs of other rasters
climate_zones <- project(climate_zones, "EPSG:4326")
climate_zones<- resample(climate_zones, lndcvr_na)
climate_zones <- crop(climate_zones, na_bound_vect)

# write to file for faster computation
writeRaster(climate_zones, filename = "data/processed/climate_zones.tif", overwrite = TRUE)

# read in climate_zones data from file
climate_zones <- rast("data/processed/climate_zones.tif")

# repeat steps above for ecoregions
ecoregions <- rasterize(ecoregions_vect, temprast, field = "NameL3_En")
ecoregions <- project(ecoregions, "EPSG:4326")
ecoregions <- resample(ecoregions, lndcvr_na)
ecoregions <- crop(ecoregions, na_bound_vect)

# write to file for faster computation
writeRaster(ecoregions, filename = "data/processed/ecoregions.tif", overwrite = TRUE)

# read in ecoregions raster
ecoregions <- rast("data/processed/ecoregions.tif")

# repeat above steps for protected areas data
# IUCN categories
protect_area_IUCN <- rasterize(protect_area_IUCN_vect, temprast, field = "TYPE_PA")
protect_area_IUCN <- project(protect_area_IUCN, "EPSG:4326")
protect_area_IUCN <- resample(protect_area_IUCN, lndcvr_na)
protect_area_IUCN <- crop(protect_area_IUCN, na_bound_vect)
plot(protect_area_IUCN)

# write to file for faster computation
writeRaster(protect_area_IUCN, filename = "data/processed/protect_area_IUCN.tif", overwrite = TRUE)

# read in protected areas IUCN data from file
protect_area_IUCN <- rast("data/processed/protect_area_IUCN.tif")

# OECMs
protect_area_OECM <- rasterize(protect_area_OECM_vect, temprast, field = "TYPE_PA")
protect_area_OECM <- project(protect_area_OECM, "EPSG:4326")
protect_area_OECM <- resample(protect_area_OECM, lndcvr_na)
protect_area_OECM <- crop(protect_area_OECM, na_bound_vect)
plot(protect_area_OECM)

# write to file for faster computation
writeRaster(protect_area_OECM, filename = "data/processed/protect_area_OECM.tif", overwrite = TRUE)

# read in protected areas IUCN data from file
protect_area_OECM <- rast("data/processed/protect_area_OECM.tif")

# watersheds data was not behaving like the rest, so used different approach
watersheds_vect$NAW4_numeric <- as.numeric(as.factor(as.character(watersheds_vect$NAW4_EN)))
watersheds <- rasterize(watersheds_vect, temprast, field = "NAW4_numeric")
watersheds <- project(watersheds, "EPSG:4326")
watersheds <- resample(watersheds, lndcvr_na)
watersheds <- crop(watersheds, na_bound_vect)
plot(watersheds)
# need to crop and mask to new extent (na_bound_vect)

# write to file for faster computation
writeRaster(watersheds, filename = "data/processed/watersheds.tif", overwrite = TRUE)

# read in watersheds data from file
watersheds <- rast("data/processed/watersheds.tif")

# tidysdm requires rasters to be numeric, so convert categorical rasters
# so categories are coded as numbers:
anth_biome <-as.numeric(anth_biome, index = 1:nlevels(anth_biome))
climate_zones <- as.numeric(climate_zones, index = 1:nlevels(climate_zones))
ecoregions <- as.numeric(ecoregions, index = 1:nlevels(ecoregions))
protect_area_IUCN <- as.numeric(protect_area_IUCN, index = 1:nlevels(protect_area_IUCN))
protect_area_OECM <- as.numeric(protect_area_OECM, index = 1:nlevels(protect_area_OECM))
# watersheds <- as.numeric(watersheds, index = 1:nlevels(watersheds))
  # ^ doesn't work, all values NA, but above code worked for watersheds

# need to change the names of the columns/layers to match our objects
  # tidysdm requirement
names(anth_biome) <- "anth_biome"
names(climate_zones) <- "climate_zones"
names(ecoregions) <- "ecoregions"
names(elevation_na) <- "elevation_na"
names(lndcvr_na) <- "lndcvr_na"
names(protect_area_IUCN) <- "protect_area_IUCN"
names(protect_area_OECM) <- "protect_area_OECM"
names(soil_phh2o_0_5) <- "soil_phh2o_0_5"
names(soil_phh2o_5_15) <- "soil_phh2o_5_15"
names(soil_temp_0_5) <- "soil_temp_0_5"
names(soil_temp_5_15) <- "soil_temp_5_15"
names(watersheds) <- "watersheds"

# now create a multilayer spatraster 
  # note: annotated out protected areas rasters because they introduce too many
  # NA values (too many rows need to be removed from predictors_multi when protected areas are included)
predictors_multi <- c(anth_biome, 
                      climate_zones, 
                      ecoregions, 
                      elevation_na, 
                      lndcvr_na, 
                      # protect_area_IUCN, introduces lots of NA values
                      # protect_area_OECM, introduces lots of NA values
                      soil_phh2o_0_5, 
                      soil_phh2o_5_15, 
                      soil_temp_0_5, 
                      soil_temp_5_15)# ,  
                      watersheds)

# mask the multilayer raster so the values outside of na_bound are NA
predictors_multi <- mask(predictors_multi, na_bound_vect)

# write the multilayer raster to file for faster computation
writeRaster(predictors_multi, 
            filename = "data/processed/predictors_multi.tif", 
            overwrite = TRUE)

# read in multilayer raster
predictors_multi <- rast("data/processed/predictors_multi.tif")


## below code may not be necessary, but keeping just in case ##

# tested out the below code to get categorical rasters to be numeric
# climate_zones_levels <- levels(climate_zones)
# cats(climate_zones)
# climate_zones <- dplyr::select(climate_zones)
# climate_zones_df <- as.data.frame(climate_zones, row.names = NULL, xy = TRUE)
# levels(climate_zones)
# activeCat(climate_zones) <- 1:nlevels(climate_zones)
# readout of categories and # assigned to them


# may need to remove NA values from layers in multilayer raster
# entire multilayer raster:
# for(i in 1:11){
# predictors_multirast[!is.na(predictors_multirast[,,i])]
# }
# summary(predictors_multirast)
# ^ code ran with no errors but summary(predictors_multirast) shows there are still NAs

# try assigning NAs to -9999 for predictors_multi_num just to see if it'll work in tidysdm code
# downside is -9999 skews summary stats badly
# for(i in 1:11){
  # predictors_multi_num[is.na(predictors_multi_num[,,i])] <- -9999
  # } 
# ^ too large to process, caused R to abort

# try assigning -9999 to NA values in just one layer
# anth_biome_num[is.na(anth_biome_num)] <- -9999
# summary(anth_biome_num)
# ^ it worked, try for other layers

# climate_zones_num[is.na(climate_zones_num)] <- -9999
# summary(climate_zones_num)

# elevation_na[is.na(elevation_na)] <- -9999
# summary(elevation_na)

# lndcvr_na[is.na(lndcvr_na)] <- -9999
# summary(lndcvr_na)

# protect_area_IUCN_num[is.na(protect_area_IUCN_num)] <- -9999
# summary(protect_area_IUCN_num)

# protect_area_OECM_num[is.na(protect_area_OECM_num)] <- -9999
# summary(protect_area_OECM_num)

# soil_phh2o_0_5[is.na(soil_phh2o_0_5)] <- -9999
# summary(soil_phh2o_0_5)

# soil_phh2o_5_15[is.na(soil_phh2o_5_15)] <- -9999
# summary(soil_phh2o_5_15)

# soil_temp_0_5[is.na(soil_temp_0_5)] <- -9999
# summary(soil_temp_0_5)

# soil_temp_5_15[is.na(soil_temp_5_15)] <- -9999
# summary(soil_temp_5_15)

# watersheds_num[is.na(watersheds_num)] <- -9999
# summary(watersheds_num)

# now run the multilayer spatraster code again to see if there are still NAs: 
# predictors_multi_num <- c(anth_biome_num, 
                          # climate_zones_num, 
                          # elevation_na, 
                          # lndcvr_na, 
                          # protect_area_IUCN_num, 
                          # protect_area_OECM_num, 
                          # soil_phh2o_0_5, 
                          # soil_phh2o_5_15, 
                          # soil_temp_0_5, 
                          # soil_temp_5_15,  
                          # watersheds_num)
# summary(predictors_multi_num)
# still NAs, but plug into tidysdm code anyway to see if changing names of layers helped with issue
  # update: changing layer names helped with violin plots
  # but still need to get rid of NA values somehow

# predictors_multi_no_na <- rast(predictors_multi_no_na)
  # looks like no NA values (yay)


# since there are multiple layers in precip and tavg_mar_jun,
# may need to use below code:
# precip:
# for(i in 1:12){
 # precip[is.na(precip[,,i])] <- -9999
# }

#tavg_mar_jun
# for(i in 1:4){
 # tavg_mar_jun[is.na(tavg_mar_jun[,,i])] <- -9999
#}

                    
                    