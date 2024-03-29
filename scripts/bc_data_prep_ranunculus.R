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

# crop the SpatVector to the extent of British Columbia
ran_occ_bc <- crop(ran_occ_vect, bc_bound)
ran_occ_bc

ran_occ_bc_sf <- st_as_sf(ran_occ_bc, 
                          coords = c("decimalLongitude", "decimalLatitude"))
ran_occ_bc_sf

## Predictor Data ##

# crop soil temperature SpatRaster to British Columbia extent
soil_temp_0_5_bc <- crop(soil_temp_0_5, bc_bound)
soil_temp_5_15_bc <- crop(soil_temp_5_15, bc_bound)

# reproject soil temperature SpatRasters to WGS84
soil_temp_0_5_bc <- terra::project(soil_temp_0_5_bc, "EPSG:4326",
                                   method = "bilinear")
soil_temp_5_15_bc <- terra::project(soil_temp_5_15_bc, "EPSG:4326",
                                    method = "bilinear")

# crop pH SpatRaster to British Columbia extent
soil_phh2o_0_5_bc <- crop(soil_phh2o_0_5, bc_bound)
soil_phh2o_5_15_bc <- crop(soil_phh2o_5_15, bc_bound)

# reproject soil pH SpatRasters to WGS84
soil_phh2o_0_5_bc <- terra::project(soil_temp_0_5_bc, "EPSG:4326",
                                    method = "bilinear")
soil_phh2o_5_15_bc <- terra::project(soil_temp_5_15_bc, "EPSG:4326",
                                     method = "bilinear")

# create a SpatRaster of the BEC data
# start by turning bc_bec into a SpatVector object
bc_bec_vec <- vect(bc_bec)

# calculate number of rows (Y direction) and columns (X) for raster 
# using 1km resolution (1000)
numcols <- as.vector(ceiling((st_bbox(bc_bec)$xmax - st_bbox(bc_bec)$xmin)/1000))
numrows <- as.vector(ceiling((st_bbox(bc_bec)$ymax - st_bbox(bc_bec)$ymin)/1000))

# create a temporary raster with number of columns and rows from above
bec_temprast <- rast(bc_bec_vec, ncols = numcols, nrows = numrows)

# create raster from SpatVector and structure of temporary raster
# select "ZONE" layer from BEC data
bc_bec <- rasterize(bc_bec_vec, bec_temprast, "ZONE")
plot(bc_bec)

# reproject BEC data to WGS84
# bc_bec <- terra::project(bc_bec, "EPSG:4326", method = "near")

# crop BEC data to match BC extent?
# bc_bec <- crop(bc_bec, bc_bound)

# resample BEC data to match resolution of other rasters
# bc_bec <- resample(bc_bec, soil_temp_0_5_bc)

# write new BEC data to raster for easier future use
# bc_bec <- writeRaster(bc_bec, "data/bc_bec.tif", overwrite = TRUE)

# import bc_bec data from new file
bc_bec <- rast("data/bc_bec.tif")

# crop elevation data to British Columbia extent
elevation_bc <- crop(elevation_canada, bc_bound)

# reproject elevation data to WGS84
elevation_bc <- terra::project(elevation_bc, "EPSG:4326", method = "bilinear")

# select March to June for average temperatures (relevant to growing season)
tavg_canada_mar_jun <- tavg_canada[[3:6]]

tavg_canada_mar_jun <- project(tavg_canada_mar_jun, "EPSG:4326", method = "bilinear")

# OR: 
# aggregate tavg_canada raster so it can be cropped?
# by a factor of 3 to go from 5040 columns to closer to 1404 (size of bc_extent_rast)
# agg_tavg_canada <- aggregate(tavg_canada, fact = 3)
# tavg_bc <- crop(tavg_canada, bc_extent_rast)
# resample average monthly temperature data
# tavg_bc <- resample(agg_tavg_canada, soil_temp_0_5_bc)

# crop average monthly temperature data to British Columbia extent
tavg_bc <- crop(tavg_canada_mar_jun, bc_bound)

# crop precipitation data to British Columbia extent
prec_bc <- crop(prec_canada, bc_bound)

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
# lndcvr_bc <- crop(lndcvr_na_agg, bc_bound)

# resample landcover BC data to change resolution
# lndcvr_bc <- resample(lndcvr_bc, soil_temp_0_5_bc)

# create file of BC landcover data for easier/faster re-use
# lndcvr_bc <- writeRaster(lndcvr_bc, "data/lndcvr_bc.tif", overwrite = TRUE)

# import landcover BC data from new file created above
lndcvr_bc <- rast("data/lndcvr_bc.tif")

# mask all layers so values outside of bc_bound are NA
elevation_bc <- mask(elevation_bc, bc_bound)
soil_temp_0_5_bc <- mask(soil_temp_0_5_bc, bc_bound)
soil_temp_5_15_bc <- mask(soil_temp_5_15_bc, bc_bound)
soil_phh2o_0_5_bc <- mask(soil_phh2o_0_5_bc, bc_bound)
soil_phh2o_5_15_bc <- mask(soil_phh2o_5_15_bc, bc_bound)
prec_bc <- mask(prec_bc, bc_bound)
bc_bec <- mask(bc_bec, bc_bound)
tavg_bc <- mask(tavg_bc, bc_bound)
lndcvr_bc <- mask(lndcvr_bc, bc_bound)

# trim NA values from outside of BC boundary
elevation_bc <- trim(elevation_bc, padding = 0, value = NA)
soil_temp_0_5_bc <- trim(soil_temp_0_5_bc, padding = 0, value = NA)
soil_temp_5_15_bc <- trim(soil_temp_5_15_bc)
soil_phh2o_0_5_bc <- trim(soil_phh2o_0_5_bc, padding = 0, value = NA)
soil_phh2o_5_15_bc <- trim(soil_phh2o_5_15_bc, padding = 0, value = NA)
prec_bc <- trim(prec_bc, padding = 0, value = NA)
bc_bec <- trim(bc_bec, padding = 0, value = NA)
tavg_bc <- trim(tavg_bc, padding = 0, value = NA)
lndcvr_bc <- trim(lndcvr_bc, padding = 0, value = NA)

# set all remaining NA values to -9999 (predictors cannot have NA values)
elevation_bc[is.na(elevation_bc)] <- -500
soil_temp_0_5_bc[is.na(soil_temp_0_5_bc)] <- -100
soil_temp_5_15_bc[is.na(soil_temp_5_15_bc)] <- -100
soil_phh2o_0_5_bc[is.na(soil_phh2o_5_15_bc)] <- -10
soil_phh2o_5_15_bc[is.na(soil_phh2o_5_15_bc)] <- -10
bc_bec[is.na(bc_bec)] <- -1
lndcvr_bc[is.na(lndcvr_bc)] <- -1

# prec_bc:
for(i in 1:12){
  prec_bc[is.na(prec_bc[,,i])] <- -100
}

#tavg_bc
for(i in 1:4){
  tavg_bc[is.na(tavg_bc[,,i])] <- -100
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
predictors_multirast <- rast(c(elevation_bc,
                               soil_temp_0_5_bc, 
                               soil_temp_5_15_bc,
                               soil_phh2o_0_5_bc,
                               soil_phh2o_5_15_bc,
                               prec_bc,
                               bc_bec,
                               tavg_bc,
                               lndcvr_bc))
predictors_multirast <- writeRaster(predictors_multirast, filename = "data/predictors_multirast.tif")



