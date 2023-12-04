## Extent ##

# create a SpatExtent for BC based on the bc_extent object
  # cannot coerce Extent object to SpatRaster
bc_spatextent <- ext(bc_extent)
# now create a regular SpatRaster for bc_extent
bc_extent_rast <- rast(bc_spatextent)

## Occurrence Data ##

# clean the occurrence records using CoordinateCleaner package
# record-level tests

cl_coord_ran <- clean_coordinates(x = ran_occ, lon = "longitude", 
                                  lat = "latitude",
                                  species = "scientific_name")

# select only the relevant columns (species, longitude, latitude)?
ran_occ <- dplyr::select(ran_occ, gbifID, occurrenceStatus, longitude, latitude)

# create a SpatVector object for the occurrence data
ran_occ_vect <- vect(ran_occ, geom = c("longitude", "latitude"), 
                     crs = "EPSG:4326", keepgeom = FALSE)

# crop the SpatVector to the extent of British Columbia
ran_occ_bc <- crop(ran_occ_vect, bc_extent_rast)
crs(ran_occ_bc)

## Predictor Data ##

# crop soil temperature SpatRaster to British Columbia extent
soil_temp_0_5_bc <- crop(soil_temp_0_5, bc_extent_rast)
soil_temp_5_15_bc <- crop(soil_temp_5_15, bc_extent_rast)

# reproject soil temperature SpatRasters to WGS84
soil_temp_0_5_bc <- terra::project(soil_temp_0_5_bc, "EPSG:4326",
                                   method = "bilinear")
soil_temp_5_15_bc <- terra::project(soil_temp_5_15_bc, "EPSG:4326",
                                    method = "bilinear")

# remove NA values
na.omit(soil_temp_0_5_bc)
na.omit(soil_temp_5_15_bc)

# set NAs from soil temperature data to -9999?
# soil_temp_0_5_bc[is.na(soil_temp_0_5_bc[])] <- 9999
# soil_temp_5_15_bc[is.na(soil_temp_5_15_bc[])] <- 9999

# crop pH SpatRaster to British Columbia extent
soil_phh2o_0_5_bc <- crop(soil_phh2o_0_5, bc_extent_rast)
soil_phh2o_5_15_bc <- crop(soil_phh2o_5_15, bc_extent_rast)

# reproject soil pH SpatRasters to WGS84
soil_phh2o_0_5_bc <- terra::project(soil_temp_0_5_bc, "EPSG:4326",
                                   method = "bilinear")
soil_phh2o_5_15_bc <- terra::project(soil_temp_5_15_bc, "EPSG:4326",
                                    method = "bilinear")

# remove NA values
na.omit(soil_phh2o_0_5_bc)
na.omit(soil_phh2o_5_15_bc)

# set NAs from soil temperature data to -9999?
# soil_temp_0_5_bc[is.na(soil_temp_0_5_bc[])] <- -9999
# soil_temp_5_15_bc[is.na(soil_temp_5_15_bc[])] <- -9999 

# create a SpatRaster of the BEC data
bc_bec <- rast(bc_bec)

# reproject BEC data to WGS84
bc_bec <- terra::project(bc_bec, "EPSG:4326", method = "near")

# crop BEC data to BC extent
bc_bec <- crop(bc_bec, bc_extent_rast)

# resample BEC data to match resolution of other rasters
bc_bec <- resample(bc_bec, soil_temp_0_5_bc)

# crop elevation data to British Columbia extent
elevation_bc <- crop(elevation_canada, bc_extent_rast)

# reproject elevation data to WGS84
elevation_bc <- terra::project(elevation_bc, "EPSG:4326", method = "bilinear")

# remove NA values
na.omit(elevation_bc)

# aggregate tavg_canada raster so it can be cropped
# by a factor of 3 to go from 5040 columns to closer to 1404 (size of bc_extent_rast)
aggregated_tavg_canada <- aggregate(tavg_canada, fact = 3)

# crop average monthly temperature data to British Columbia extent
tavg_bc <- crop(aggregated_tavg_canada, bc_extent_rast)

# resample average monthly temperature data
tavg_bc <- resample(aggregated_tavg_canada, soil_temp_0_5_bc)

# remove NA values
na.omit(tavg_bc)

# crop precipitation data to British Columbia extent
prec_bc <- crop(prec_canada, bc_extent_rast)

# remove NA values
na.omit(prec_bc)

# aggregate landcover data so it can be reprojected and cropped
lndcvr_na_agg <- aggregate(lndcvr_na, fact = 15)

# reproject landcover North America data to WGS84
lndcvr_na_agg <- terra::project(lndcvr_na_agg, "EPSG:4326", method = "near")

# crop landcover North America data to BC's extent
lndcvr_bc <- crop(lndcvr_na_agg, bc_extent_rast)

# resample landcover BC data to change resolution
lndcvr_bc <- resample(lndcvr_bc, soil_temp_0_5_bc)

# remove NA values
na.omit(lndcvr_bc)

## Multilayer Raster ##

# create a multilayer raster of the predictor variables
predictors_multirast <- rast(c(elevation_bc,
                               soil_temp_0_5_bc, 
                               soil_temp_5_15_bc,
                               soil_phh2o_0_5_bc,
                               soil_phh2o_5_15_bc,
                               prec_bc,
                               bc_bec,
                               tavg_bc,
                               lndcvr_bc))

# bc_bec has all NA values, tavg_bc has all 0 values
# flexsdm requires environmental raster consist of all continuous variables
pred_rast <- c(elevation_bc,
                    soil_temp_0_5_bc, 
                    soil_temp_5_15_bc, 
                    soil_phh2o_0_5_bc, 
                    soil_phh2o_5_15_bc)

                    