## Occurrence Data ##

# clean the occurrence records using CoordinateCleaner package?
# record-level tests

# cl_coord_ran <- clean_coordinates(x = ran_occ_download, lon = "decimalLongitude", 
                                 #  lat = "decimalLatitude",
                                 #  species = "scientificName")

# select only the relevant columns (ID column, longitude, latitude)?
ran_occ <- dplyr::select(ran_occ_download, gbifID, 
                         decimalLongitude, decimalLatitude) # dataframe

# create a SpatVector object for the occurrence data?
ran_occ_vect <- vect(ran_occ, geom = c("decimalLongitude", "decimalLatitude"), 
                     crs = "EPSG:4326", keepgeom = FALSE)

# crop the SpatVector to the extent of the study area
ran_occ <- crop(ran_occ_vect, na_bound)
ran_occ

# ran_occ_bc_sf <- st_as_sf(ran_occ_bc, 
                          # coords = c("decimalLongitude", "decimalLatitude"))
# ran_occ_bc_sf

## Predictor Data ##

# crop soil temperature SpatRaster to North American extent
soil_temp_0_5 <- crop(soil_temp_0_5, na_bound)
soil_temp_5_15 <- crop(soil_temp_5_15, na_bound)

# reproject soil temperature SpatRasters to WGS84
soil_temp_0_5 <- terra::project(soil_temp_0_5, "EPSG:4326",
                                   method = "bilinear")
soil_temp_5_15 <- terra::project(soil_temp_5_15, "EPSG:4326",
                                    method = "bilinear")

# reproject soil pH SpatRasters to WGS84
soil_phh2o_0_5 <- terra::project(soil_temp_0_5, "EPSG:4326",
                                 method = "bilinear")
soil_phh2o_5_15 <- terra::project(soil_temp_5_15, "EPSG:4326",
                                  method = "bilinear")
# crop pH SpatRaster to North American extent
soil_phh2o_0_5 <- crop(soil_phh2o_0_5, na_bound)
soil_phh2o_5_15 <- crop(soil_phh2o_5_15, na_bound)

# reproject elevation data to WGS84
# elevation_na <- terra::project(elevation_na, "EPSG:4326", method = "bilinear")

# resample elevation_na to change resolution
# elevation_na <- terra::resample(elevation_na, soil_temp_0_5)

# crop elevation data to British Columbia extent
# elevation_na <- crop(elevation_na, na_bound)

# write elevation_na to file for easier reuse
# elevation_na <- writeRaster(elevation_na, filename = "data/elevation_na.tif")

# read in elevation data
elevation_na <- rast("data/elevation_na.tif")

# select March to June for average temperatures (relevant to growing season)
# tavg_mar_jun <- temp_avg_global[[3:6]]

# reproject temp data to WGS84
# tavg_mar_jun <- project(tavg_mar_jun, "EPSG:4326", method = "bilinear")

# resample temperature data to change resolution
# tavg_mar_jun <- resample(tavg_mar_jun, soil_temp_0_5)

# crop temperature data to match North America extent
# tavg_mar_jun <- crop(tavg_mar_jun, na_bound)

# write temperature data to file for faster future computation
# tavg_mar_jun <- writeRaster(tavg_mar_jun, filename = "data/tavg_mar_jun.tif")

# read in temperature data
tavg_mar_jun <- rast("data/tavg_mar_jun.tif")

# resample precipitation data to change resolution
# precip_global <- resample(precip_global, soil_temp_0_5)

# crop precipitation data to British Columbia extent
# precip <- crop(precip_global, na_bound)

# write precipitation data to file for faster future computation
# precip <- writeRaster(precip, filename = "data/precipitation.tif")

# read in precipitation data
precip <- rast("data/precipitation.tif")

# aggregate landcover data so it can be reprojected and cropped
# lndcvr_na_agg <- aggregate(lndcvr_na, fact = 15)

# write aggregated landcover data to file for easier reuse
# lndcvr_na_agg <-writeRaster(lndcvr_na_agg, 
                                   # filename = "data/lndcvr-north-america_agg.tif", 
                                   # overwrite = TRUE)

# create SpatRaster of aggregated landcover data from new file
# lndcvr_na_agg <- rast("data/lndcvr-north-america_agg.tif")

# reproject landcover North America data to WGS84
# lndcvr_na_agg <- terra::project(lndcvr_na_agg, "EPSG:4326", method = "near")

# crop landcover North America data to BC's extent
# lndcvr_na <- crop(lndcvr_na_agg, na_bound)

# resample landcover BC data to change resolution
# lndcvr_na <- resample(lndcvr_na, soil_temp_0_5)

# create file of processed landcover data for faster re-use
# lndcvr_na <- writeRaster(lndcvr_na, "data/lndcvr_na.tif", overwrite = TRUE)

# import processed landcover data from new file created above
lndcvr_na <- rast("data/lndcvr_na.tif")

# reproject anthropogenic biomes data to WGS84
anth_biome <- project(anth_biome, "EPSG:4326")

# resample anth_biome to change resolution
anth_biome <- resample(anth_biome, soil_temp_0_5)



#### code below this point doesn't really work ####
  #### it runs but doesn't produce what I need, which is rasters with no NA values, 
       #### but matching extents so they can be combined in a multiraster ####



# note: watersheds raster has no values

# reproject watersheds data to WGS84
watersheds <- project(watersheds, "EPSG:4326")

# resample watersheds data to change resolution
watersheds <- resample(watersheds, soil_temp_0_5)

# crop watersheds data to study extent
watersheds <- crop(watersheds, na_bound)

# create a multilayer raster of the predictor variables
predictors_multirast <- rast(c(anth_biome, 
                               elevation_na,
                               lndcvr_na, 
                               precip, 
                               soil_phh2o_0_5,
                               soil_phh2o_5_15,
                               soil_temp_0_5, 
                               soil_temp_5_15,
                               tavg_mar_jun, 
                              # watersheds)) # needs some work

# multiraster doesn't hold any values - likely need to remove NA values from 
    # individual predictor rasters


# mask all layers so values outside of na_bound are NA

# can also try: mask(r, !is.na(r))

anth_biome <- mask(anth_biome, na_bound)
elevation_na <- mask(elevation_na, na_bound)
lndcvr_na <- mask(lndcvr_na, na_bound)
precip <- mask(precip, na_bound)
soil_phh2o_0_5 <- mask(soil_phh2o_0_5, na_bound)
soil_phh2o_5_15 <- mask(soil_phh2o_5_15, na_bound)
soil_temp_0_5 <- mask(soil_temp_0_5, na_bound)
soil_temp_5_15 <- mask(soil_temp_5_15, na_bound)
tavg_mar_jun <- mask(tavg_mar_jun, na_bound)
watersheds <- mask(watersheds, na_bound)

# trim NA values from outside of boundary

#### this might be where extents are shifting ####

# anth_biome <- trim(anth_biome, padding = 0, value = NA)
# elevation_na <- trim(elevation_na, padding = 0, value = NA)
# lndcvr_na <- trim(lndcvr_na, padding = 0, value = NA)
# precip <- trim(precip, padding = 0, value = NA)
# soil_phh2o_0_5 <- trim(soil_phh2o_0_5, padding = 0, value = NA)
# soil_phh2o_5_15 <- trim(soil_phh2o_5_15, padding = 0, value = NA)
# soil_temp_0_5 <- trim(soil_temp_0_5, padding = 0, value = NA)
# soil_temp_5_15 <- trim(soil_temp_5_15, padding = 0, value = NA)
# tavg_mar_jun <- trim(tavg_mar_jun, padding = 0, value = NA)


# set all remaining NA values to -9999? (predictors cannot have NA values)
# used different values as the rasters are continuous and the colour scale gets
  # messed right up with -9999 (skewed heavily toward -9999)
anth_biome[is.na(anth_biome)] <- -10
elevation_na[is.na(elevation_na)] <- -500
lndcvr_na[is.na(lndcvr_na)] <- -10
precip[is.na(precip)] <- -10
soil_phh2o_0_5[is.na(soil_phh2o_5_15)] <- -10
soil_phh2o_5_15[is.na(soil_phh2o_5_15)] <- -10
soil_temp_0_5[is.na(soil_temp_0_5)] <- -100
soil_temp_5_15[is.na(soil_temp_5_15)] <- -100
tavg_mar_jun[is.na(tavg_mar_jun)] <- -200

# since there are multiple layers in precip and tavg_mar_jun,
  # may need to use below code:
# precip:
for(i in 1:12){
  precip[is.na(precip[,,i])] <- -10
}

#tavg_mar_jun
for(i in 1:4){
  tavg_mar_jun[is.na(tavg_mar_jun[,,i])] <- -200
}

# extents moved around a bit - need to crop to smallest extent (bc_bec or lndcvr_bc)
elevation_bc <- crop(elevation_bc, bc_bound)
soil_temp_0_5_bc <- crop(soil_temp_0_5_bc, bc_bound)
soil_temp_5_15_bc <- crop(soil_temp_5_15_bc, bc_bound)
soil_phh2o_0_5_bc <- crop(soil_phh2o_0_5_bc, bc_bound)
soil_phh2o_5_15_bc <- crop(soil_phh2o_5_15_bc, bc_bound)
prec_bc <- crop(prec_bc, bc_bound)
tavg_bc <- crop(tavg_bc, bc_bound)
lndcvr_bc <- crop(lndcvr_bc, bc_bound)


# write rasters to file for faster computation
elevation_bc <- writeRaster(elevation_bc, filename = "data/elevation_bc.tif")
soil_temp_0_5_bc <- writeRaster(soil_temp_0_5_bc, 
                                filename = "data/soil_temp_0_5_bc.tif")
soil_temp_5_15_bc <- writeRaster(soil_temp_5_15_bc, 
                               filename = "data/soil_temp_5_15_bc.tif")
soil_phh2o_0_5_bc <- writeRaster(soil_phh2o_0_5_bc, 
                                 filename = "data/soil_phh2o_0_5_bc.tif")
soil_phh2o_5_15_bc <- writeRaster(soil_phh2o_5_15_bc,
                                  filename = "data/soil_phh2o_5_15_bc.tif")
prec_bc <- writeRaster(prec_bc, filename = "data/prec_bc.tif")
bc_bec <- writeRaster(bc_bec, filename = "data/bc_bec.tif", overwrite = TRUE)
tavg_bc <- writeRaster(tavg_bc, filename = "data/tavg_bc.tif")
lndcvr_bc <- writeRaster(lndcvr_bc, filename = "data/lndcvr_bc.tif", overwrite = TRUE)


## Multilayer Raster ##

# create a multilayer raster of the predictor variables
# new error: says extents don't match
predictors_multirast <- rast(c(elevation_na,
                               soil_temp_0_5, 
                               soil_temp_5_15,
                               soil_phh2o_0_5,
                               soil_phh2o_5_15,
                               precip,
                               anth_biome,
                               tavg_mar_jun,
                               lndcvr_na))
predictors_multirast <- writeRaster(predictors_multirast, filename = "data/predictors_multirast.tif")


                    
                    