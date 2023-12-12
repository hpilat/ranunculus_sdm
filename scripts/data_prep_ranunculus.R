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
ran_occ_bc <- crop(ran_occ_vect, bc_extent_rast)
ran_occ_bc


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
# start by turning bc_bec into a SpatVector object
# bc_bec_vec <- vect(bc_bec)

# calculate number of rows (Y direction) and columns (X) for raster 
# using 1km resolution (1000)
# numcols <- as.vector(ceiling((st_bbox(bc_bec)$xmax - st_bbox(bc_bec)$xmin)/1000))
# numrows <- as.vector(ceiling((st_bbox(bc_bec)$ymax - st_bbox(bc_bec)$ymin)/1000))

# create a temporary raster with number of columns and rows from above
# bec_temprast <- rast(bc_bec_vec, ncols = numcols, nrows = numrows)

# create raster from SpatVector and structure of temporary raster
# select "ZONE" layer from BEC data
# bc_bec <- rasterize(bc_bec_vec, bec_temprast, "ZONE")
# plot(bc_bec)

# reproject BEC data to WGS84
# bc_bec <- terra::project(bc_bec, "EPSG:4326", method = "near")

# crop BEC data to match BC extent?
# bc_bec <- crop(bc_bec, bc_extent_rast)

# resample BEC data to match resolution of other rasters
# bc_bec <- resample(bc_bec, soil_temp_0_5_bc)

# write new BEC data to raster for easier future use
# bc_bec <- writeRaster(bc_bec, "data/bc_bec.tif", overwrite = TRUE)

# import bc_bec data from new file
bc_bec <- rast("data/bc_bec.tif")

# crop elevation data to British Columbia extent
elevation_bc <- crop(elevation_canada, bc_extent_rast)

# reproject elevation data to WGS84
elevation_bc <- terra::project(elevation_bc, "EPSG:4326", method = "bilinear")

# remove NA values
na.omit(elevation_bc)

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
tavg_bc <- crop(tavg_canada_mar_jun, bc_extent_rast)

# remove NA values
na.omit(tavg_bc)

# crop precipitation data to British Columbia extent
prec_bc <- crop(prec_canada, bc_extent_rast)

# remove NA values
na.omit(prec_bc)

# aggregate landcover data so it can be reprojected and cropped
# lndcvr_na_agg <- aggregate(lndcvr_na, fact = 15)

# write aggregated landcover data to file for easier reuse
# lndcvr-north-america_agg <-writeRaster(lndcvr_na_agg, 
                                     #  filename = "data/lndcvr-north-america_agg.tif", 
                                     #  overwrite = TRUE)

# create SpatRaster of aggregated landcover data from new file
# lndcvr_na_agg <- rast("data/lndcvr-north-america_agg.tif")

# reproject landcover North America data to WGS84
# lndcvr_na_agg <- terra::project(lndcvr_na_agg, "EPSG:4326", method = "near")

# crop landcover North America data to BC's extent
# lndcvr_bc <- crop(lndcvr_na_agg, bc_extent_rast)

# resample landcover BC data to change resolution
# lndcvr_bc <- resample(lndcvr_bc, soil_temp_0_5_bc)

# remove NA values
# na.omit(lndcvr_bc)

# create file of BC landcover data for easier/faster re-use
# lndcvr_bc <- writeRaster(lndcvr_bc, "data/lndcvr_bc.tif", overwrite = TRUE)

# import landcover BC data from new file created above
lndcvr_bc <- rast("data/lndcvr_bc.tif")


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



# flexsdm requires environmental raster consisting of all continuous variables
# pred_rast <- c(elevation_bc,
              # soil_temp_0_5_bc,
              # soil_temp_5_15_bc,
              # soil_phh2o_0_5_bc, 
              # soil_phh2o_5_15_bc)

                    