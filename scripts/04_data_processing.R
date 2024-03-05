# This script prepares spatial extent, occurrence records, and predictor data
  # for input into the tidysdm pipeline
# Please first run these scripts in the following order:
  # 01_data_download_ranunculus.R
  # 02_continental_divide.Rmd
  # 03_cropped_extent.R

# goal is to have raster files in WGS84, with 0.0083 resolution, 
# cropped and masked to na_bound_vect
# order of operations is: 1- reproject to WGS84, 2- resample to 0.0083 resolution, 
  # and 3- crop to na_bound_vect
# landcover data is very large and needed to be aggregated first in order to do 
  # any of the above operations
# tidysdm also requires layer names to match object names, so we'll change those too
# tidysdm requires categorical rasters be coded as numeric, 
    # so we'll code categorical raster values to numeric


library(tidyverse)
library(sf)
library(terra)


# import extent files (created in 03_cropped_extent.R)
na_bound_vect <- vect("data/extents/na_bound_vect.shp")
na_bound_rast <- rast("data/extents/na_bound_rast.tif")

# import files (already downloaded in 01_data_download.R)
# raw files are appended with _na or _global, cropped files are not
ran_occ_vect <- vect("data/extents/ran_occ_sf.shp")
worldclim_present_na <- rast("data/processed/worldclim_tiles_combined.tif")
worldclim_future_na <- rast("data/raw/wc2.1_30s_bioc_HadGEM3-GC31-LL_ssp126_2081-2100.tif")
anth_biome_na <- rast("data/raw/anthromes_EqArea.tif")
climate_zones_na <- vect("data/raw/North_America_Climate_Zones.shp")
ecoregions_na <- vect("data/raw/na_terrestrial_ecoregions_v2_level_iii_shapefile/NA_Terrestrial_Ecoregions_v2_Level_III_Shapefile/NA_TerrestrialEcoregions_LIII/data/NA_Terrestrial_Ecoregions_v2_level3.shp")
elevation_na <- rast("data/raw/northamerica_elevation_cec_2023.tif")
lndcvr_na <- rast("data/raw/NA_NALCMS_landcover_2020_30m.tif")
soil_phh2o_0_5_global <- rast("data/raw/soil_phh2o_0_5.tif")
soil_phh2o_5_15_global <- rast("data/raw/soil_phh2o_5_15.tif")
soil_temp_0_5_global <- rast("data/raw/SBIO4_0_5cm_Temperature_Seasonality.tif")
soil_temp_5_15_global <- rast("data/raw/SBIO4_5_15cm_Temperature_Seasonality.tif")
watersheds_na <- vect("data/raw/watersheds_shapefile/Watersheds_Shapefile/NA_Watersheds/data/watershed_p_v2.shp")



### Occurrence Data ##

ran_occ_vect # WGS84, cropped in 03_cropped_extent.R
# Use na_bound_vect as a mask for ran_occ_vect
# already in WGS84 so we can mask at this point
# so observations outside of our study area are set to NA
ran_occ_masked <- mask(ran_occ_vect, na_bound_vect)

# write to file, ready for tidysdm so write to processed folder
writeVector(ran_occ_masked, "data/processed/ran_occ_masked.shp", overwrite = TRUE)



## Predictor Data ##



## WorldClim data, for use in Bioclim model:
# start with WorldClim data to get our desired resolution and dimensions for all
  # other rasters


# Present Data:
worldclim_present_na # CRS and resolution match what we need
# crop the extent of the land mask to match our study's extent
worldclim_present_cropped <- crop(worldclim_present_na, na_bound_vect)
# mask to the study area polygon
worldclim_present_masked <- mask(worldclim_present_cropped, na_bound_vect)

# change layer names to simpler name
names(worldclim_present_masked) <- c("bio01", "bio02", "bio03", "bio04", "bio05",
                                     "bio06", "bio07", "bio08", "bio09", "bio10", 
                                     "bio11", "bio12", "bio13", "bio14", "bio15", 
                                     "bio16", "bio17", "bio18", "bio19")

# write to file for reuse in 05_tidysdm_bioclim_30s.R
writeRaster(worldclim_present_masked, filename = "data/processed/worldclim_present_masked.tif", overwrite = TRUE)


# Future Data:

worldclim_future_na # correct resolution and CRS, need to crop
worldclim_future_cropped <- crop(worldclim_future_na, na_bound_vect)
# now mask so values outside na_bound_vect are set to NA
worldclim_future_masked <- mask(worldclim_future_cropped, na_bound_vect)

# change layer names to simpler names, and to match present climate dataset (above)
names(worldclim_future_masked) <- c("bio01", "bio02", "bio03", "bio04", "bio05",
                                     "bio06", "bio07", "bio08", "bio09", "bio10", 
                                     "bio11", "bio12", "bio13", "bio14", "bio15", 
                                     "bio16", "bio17", "bio18", "bio19")

# write to file for reuse in 05_tidysdm_bioclim_30s
writeRaster(worldclim_future_masked, filename = "data/processed/worldclim_future_masked.tif", overwrite = TRUE)

# now that we have a layer with our goal resolution and extent, 
  # can resample our empty raster (created in 03_cropped_extent.R) 
  # to have the correct resolution
na_bound_rast <- resample(na_bound_rast, worldclim_present_masked)
na_bound_rast
# write to file
writeRaster(na_bound_rast, filename = "data/extents/na_bound_rast.tif", overwrite = TRUE)


# Informed Data (for use in Informed Model)


## Numeric Rasters:


## Soil Temperature

soil_temp_0_5_global
soil_temp_5_15_global
# correct CRS and resolution, just need to crop

# crop soil temperature SpatRaster to North American extent
soil_temp_0_5 <- crop(soil_temp_0_5_global, na_bound_vect)
soil_temp_5_15 <- crop(soil_temp_5_15_global, na_bound_vect)

# change the layer names to match the object name
names(soil_temp_0_5) <- "soil_temp_0_5"
names(soil_temp_5_15) <- "soil_temp_5_15"

# write processed data to file for faster computation
writeRaster(soil_temp_0_5, filename = "data/processed/soil_temp_0_5.tif", overwrite = TRUE)
writeRaster(soil_temp_5_15, filename = "data/processed/soil_temp_5_15.tif", overwrite = TRUE)


# Soil pH

soil_phh2o_0_5_global
soil_phh2o_5_15_global
# correct CRS and resolution, just need to crop

# crop pH SpatRaster to North American extent
soil_phh2o_0_5 <- crop(soil_phh2o_0_5_global, na_bound_vect)
soil_phh2o_5_15 <- crop(soil_phh2o_5_15_global, na_bound_vect)

# change the layer names to match the object name
names(soil_phh2o_0_5) <- "soil_phh2o_0_5"
names(soil_phh2o_5_15) <- "soil_phh2o_5_15"

# write processed soil pH data to file for faster computation
writeRaster(soil_phh2o_0_5, filename = "data/processed/soil_phh2o_0_5.tif", overwrite = TRUE)
writeRaster(soil_phh2o_5_15, filename = "data/processed/soil_phh2o_5_15.tif", overwrite = TRUE)


# Elevation:

elevation_na
# incorrect CRS and resolution

# reproject elevation data to WGS84
elevation_naWGS <- terra::project(elevation_na, "EPSG:4326", method = "bilinear")

# resample elevation_na to change resolution
# use na_bound_rast from earlier as template, as it has our target specs
elevation_na <- terra::resample(elevation_naWGS, na_bound_rast)

# crop elevation data to study area extent
elevation <- crop(elevation_na, na_bound_vect)

# change the layer name to match object name
names(elevation) <- "elevation"

# write elevation to file for easier reuse
writeRaster(elevation, filename = "data/processed/elevation.tif", overwrite = TRUE)



## Categorical Rasters


# Landcover:

lndcvr_na # wrong CRS, resolution
# this is a huge file, so we need to aggregate the cells before 
  # it can be reprojected and cropped
# selected a factor of 15 - reduce # of cells by 15 times

lndcvr_na_agg <- aggregate(lndcvr_na, fact = 15)

# write aggregated landcover data to file for easier reuse
lndcvr_na_agg <-writeRaster(lndcvr_na_agg, 
                            filename = "data/processed/lndcvr-north-america_agg.tif", 
                            overwrite = TRUE)

# read in aggregated landcover data from new file
lndcvr_na_agg <- rast("data/processed/lndcvr-north-america_agg.tif")

# reproject landcover North America data to WGS84
lndcvr_na_agg <- terra::project(lndcvr_na_agg, "EPSG:4326", method = "near")

# resample landcover data to change resolution
landcover <- resample(lndcvr_na_agg, na_bound_rast)

# crop landcover North America data to study extent
landcover <- crop(landcover, na_bound_vect)

# change layer name to match object name
names(landcover) <- "landcover"

# create file of processed landcover data for re-use
writeRaster(landcover, filename = "data/processed/landcover.tif", overwrite = TRUE)


# Anthropogenic Biomes

anth_biome_na
# wrong CRS and resolution

# reproject anthropogenic biomes data to WGS84
anth_biomeWGS <- project(anth_biome_na, "EPSG:4326")

# resample anth_biome to change resolution
anth_biome <- resample(anth_biomeWGS, na_bound_rast)

# crop anth_biome to study extent
anth_biome <- crop(anth_biome, na_bound_vect)

# code categorical values as numeric:
anth_biome <-as.numeric(anth_biome, index = 1:nlevels(anth_biome))

# change layer name to match object name
names(anth_biome) <- "anth_biome"

# write anth_biome to file for reuse
writeRaster(anth_biome, filename = "data/processed/anth_biome.tif", overwrite = TRUE)


# Categorical Data from Shapefiles:
# need to create a temporary raster so the Shapefiles can take on its structure
# tried using na_bound_rast which worked, except for changing categorical values to numeric

dim(landcover)
temprast <- rast(climate_zones_na, ncols = 3265, nrows = 4109)

# Climate Zones
# code categories as numeric
climate_zones_na$Climate_numeric <- as.numeric(as.factor(as.character(climate_zones_na$Climate)))
# create raster from SpatVector and structure of na_bound_rast
climate_zones <- rasterize(climate_zones_na, temprast, field = "Climate_numeric")
climate_zones
# reproject to WGS84 lat/lon
climate_zones <- project(climate_zones, "EPSG:4326")
climate_zones <- resample(climate_zones, na_bound_rast)
climate_zones <- crop(climate_zones, na_bound_vect)
# change the name to match the object
names(climate_zones) <- "climate_zones"

# write to file for reuse
writeRaster(climate_zones, filename = "data/processed/climate_zones.tif", overwrite = TRUE)

# Ecoregions
# repeat steps above for ecoregions
# select "NameL3_En" column, names in English
# code categories as numeric
ecoregions_na$NameL3_En <- as.numeric(as.factor(as.character(ecoregions_na$NameL3_En)))
# create raster from SpatVector and structure of na_bound_rast
ecoregions <- rasterize(ecoregions_na, temprast, field = "NameL3_En")
ecoregions
# reproject to WGS84 lat/lon
ecoregions <- project(ecoregions, "EPSG:4326")
ecoregions <- resample(ecoregions, na_bound_rast)
ecoregions <- crop(ecoregions, na_bound_vect)
# change the name to match the object
names(ecoregions) <- "ecoregions"

# write to file for reuse
writeRaster(climate_zones, filename = "data/processed/climate_zones.tif", overwrite = TRUE)


# Watersheds
# repeat above steps for watersheds raster
# watersheds data was not behaving like the rest, so used different approach
watersheds_na$NAW4_numeric <- as.numeric(as.factor(as.character(watersheds_na$NAW4_EN)))
# create raster from SpatVector and structure of na_bound_rast
watersheds <- rasterize(watersheds_na, temprast, field = "NAW4_numeric")
watersheds
# reproject to WGS84 lat/lon
watersheds <- project(watersheds, "EPSG:4326")
watersheds <- resample(watersheds, na_bound_rast)
watersheds <- crop(watersheds, na_bound_vect)
# change the name to match the object
names(watersheds) <- "watersheds"
watersheds

# now create a multilayer SpatRaster with all the above rasters

predictors_multi <- c(anth_biome, 
                      climate_zones, 
                      ecoregions, 
                      elevation, 
                      landcover,
                      soil_phh2o_0_5, 
                      soil_phh2o_5_15, 
                      soil_temp_0_5, 
                      soil_temp_5_15,  
                      watersheds)

# mask the multilayer raster so the values outside of na_bound are NA
predictors_multi <- mask(predictors_multi, na_bound_vect)

# write the multilayer raster to file for reuse
writeRaster(predictors_multi, 
            filename = "data/processed/predictors_multi.tif", 
            overwrite = TRUE)
