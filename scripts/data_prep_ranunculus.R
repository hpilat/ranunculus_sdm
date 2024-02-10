## Spatial Extent ##


# Skeetchestn territory:
# SNRC provided shapefile of Skeetchestn traditional territory
# Vectorize this shapefile so it can be rasterized
skeetch_vect <- vect("data/raw/SkeetchestnTT_2020/SkeetchestnTT_2020.shp")
# reproject to WGS84
skeetch_vect <- project(skeetch_vect, "EPSG:4326")
precip_skeetch <- crop(precip, skeetch_vect)
# dimensions of precip_skeetch are nrow = 132 and ncol = 142, which we'll use 
    # for setting the dimensions of the temporary raster
    # this may change when increasing the resolution for Skeetch-specific data
precip_skeetch <- mask(precip_skeetch, skeetch_vect)


# Create an empty template raster for inputting as a basemap into tidysdm
# Unsure what # of columns to use here
temprast <- rast(skeetch_vect, ncols = 142, nrows = 132)
skeetch_rast <- rasterize(skeetch_vect, temprast)
# will use higher resolution data for Skeetch extent

# reproject skeetch_rast to WGS 84
skeetch_rast <- project(skeetch_rast, "EPSG:4326")

# resample skeetch_rast to different resolution
skeetch_rast <- resample(skeetch_rast, soil_temp_0_5)

# mask skeetch_rast so all cells outside of boundary are NA
skeetch_rast <- mask(skeetch_rast, skeetch_vect)



# North American extent (west coast to continental divide)
# new geographic extent created in continental_divide.Rmd

na_bound <- read_sf("data/raw/continental_divide_buffer_boundary.shp")
na_bound <- vect(na_bound)
# na_extent <- ext(na_bound)
# create an empty raster based on study extent in order to rasterize na_bound
  # to use as a basemap for TidySDM
# temprast <- rast(na_bound, ncols = 12247, nrows = 8024)
# na_bound_rast <- rasterize(na_bound, temprast)

# write empty raster to file
# na_bound_rast <- writeRaster(na_bound_rast, filename = "data/na_bound_rast.tif")

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

ran_occ_sf <- st_as_sf(ran_occ, 
                       coords = c("decimalLongitude", "decimalLatitude"))
ran_occ_sf

## Predictor Data ##

## Numeric Rasters:

# crop soil temperature SpatRaster to North American extent
# soil_temp_0_5 <- crop(soil_temp_0_5, na_bound)
# soil_temp_5_15 <- crop(soil_temp_5_15, na_bound)

# reproject soil temperature SpatRasters to WGS84
# soil_temp_0_5 <- terra::project(soil_temp_0_5, "EPSG:4326",
                                # method = "bilinear")
# soil_temp_5_15 <- terra::project(soil_temp_5_15, "EPSG:4326",
                                # method = "bilinear")

# write processed data to file for faster computation
# soil_temp_0_5 <- writeRaster(soil_temp_0_5, filename = "data/processed/soil_temp_0_5.tif")
# soil_temp_5_15 <- writeRaster(soil_temp_5_15, filename = "data/processed/soil_temp_5_15cm.tif")

# read in soil temperature data from file:
soil_temp_0_5 <- rast("data/processed/soil_temp_0_5.tif")
soil_temp_5_15 <- rast("data/processed/soil_temp_5_15.tif")


# reproject soil pH SpatRasters to WGS84
# soil_phh2o_0_5 <- terra::project(soil_phh2o_0_5, "EPSG:4326",
                                 # method = "bilinear")
# soil_phh2o_5_15 <- terra::project(soil_phh2o_5_15, "EPSG:4326",
                                 # method = "bilinear")

# crop pH SpatRaster to North American extent
# soil_phh2o_0_5 <- crop(soil_phh2o_0_5, na_bound)
# soil_phh2o_5_15 <- crop(soil_phh2o_5_15, na_bound)

# write processed soil pH data to file for faster computation
# soil_phh2o_0_5 <- writeRaster(soil_phh2o_0_5, filename = "data/processed/soil_phh2o_0_5.tif")
# soil_phh2o_5_15 <- writeRaster(soil_phh2o_5_15, filename = "data/processed/soil_phh2o_5_15.tif")

# read in soil pH data from file
soil_phh2o_0_5 <- rast("data/processed/soil_phh2o_0_5.tif")
soil_phh2o_5_15 <- rast("data/processed/soil_phh2o_5_15.tif")

# reproject elevation data to WGS84
# elevation_na <- terra::project(elevation_na, "EPSG:4326", method = "bilinear")

# resample elevation_na to change resolution
# elevation_na <- terra::resample(elevation_na, soil_temp_0_5)

# crop elevation data to study area extent
# elevation_na <- crop(elevation_na, na_bound)

# write elevation_na to file for easier reuse
# elevation_na <- writeRaster(elevation_na, filename = "data/processed/elevation_na.tif", 
                          #  overwrite = TRUE)

# read in elevation data
elevation_na <- rast("data/processed/elevation_na.tif")

# select March to June for average temperatures (relevant to growing season)
# tavg_mar_jun <- temp_avg_global[[3:6]]

# reproject temp data to WGS84
# tavg_mar_jun <- project(tavg_mar_jun, "EPSG:4326", method = "bilinear")

# resample temperature data to change resolution
# tavg_mar_jun <- resample(tavg_mar_jun, soil_temp_0_5)

# crop temperature data to match North America extent
# tavg_mar_jun <- crop(tavg_mar_jun, na_bound)

# write temperature data to file for faster future computation
# tavg_mar_jun <- writeRaster(tavg_mar_jun, filename = "data/processed/tavg_mar_jun.tif")

# read in temperature data
# tavg_mar_jun <- rast("data/processed/tavg_mar_jun.tif")

# resample precipitation data to change resolution
# precip_global <- resample(precip_global, soil_temp_0_5)

# crop precipitation data to British Columbia extent
# precip <- crop(precip_global, na_bound)

# write precipitation data to file for faster future computation
# precip <- writeRaster(precip, filename = "data/processed/precipitation.tif")

# read in precipitation data
# precip <- rast("data/processed/precipitation.tif")


## Categorical Rasters


# aggregate landcover data so it can be reprojected and cropped
# lndcvr_na_agg <- aggregate(lndcvr_na, fact = 15)

# write aggregated landcover data to file for easier reuse
# lndcvr_na_agg <-writeRaster(lndcvr_na_agg, 
                                   # filename = "data/processed/lndcvr-north-america_agg.tif", 
                                   # overwrite = TRUE)

# create SpatRaster of aggregated landcover data from new file
# lndcvr_na_agg <- rast("data/processed/lndcvr-north-america_agg.tif")

# reproject landcover North America data to WGS84
# lndcvr_na_agg <- terra::project(lndcvr_na_agg, "EPSG:4326", method = "near")

# crop landcover North America data to BC's extent
# lndcvr_na <- crop(lndcvr_na_agg, na_bound)

# resample landcover BC data to change resolution
# lndcvr_na <- resample(lndcvr_na, soil_temp_0_5)

# create a list of the category names in lndcvr_na
# lndcvr_cat <- c("Temperate or sub-polar needleleaf forest", 
               #  "Sub-polar or taiga needleleaf forest", 
               #  "Tropical or sub-tropical broadleaf evergreen forest", 
               #  "Tropical or sub-tropical broadleaf deciduous forest", 
               #  "Temperate or sub-polar broadleaf deciduous forest", 
               #  "Mixed forest", 
               #  "Tropical or sub-tropical shrubland", 
               #  "Temperate or sub-polar shrubland", 
               #  "Tropical or sub-tropical grassland", 
               #  "Temperate or sub-polar grassland", 
               #  "Sub-polar or polar shrubland-lichen-moss", 
               #  "Sub-polar or polar grassland-lichen-moss", 
               #  "Sub-polar or polar barren-lichen-moss", 
               #  "Wetland", 
               #  "Cropland", 
               #  "Barren lands", 
               #  "Urban and built-up", 
               #  "Water", 
               #  "Snow and ice")

# lndcvr_char <- sample(lndcvr_cat, 19, replace = FALSE)
# lndcvr_fact <- factor(lndcvr_char, levels = lndcvr_cat)
# lndcvr_n_america <- rast(nrows = 8024,
                       #  ncol = 12247,
                       #  xmin = -179.25, 
                       #  xmax = -77.19167, 
                       #  ymin = 5.383333, 
                       #  ymax = 72.25,
                       #  vals = lndcvr_fact) 


# create file of processed landcover data for faster re-use
# lndcvr_na <- writeRaster(lndcvr_na, "data/processed/lndcvr_na.tif", overwrite = TRUE)

# import processed landcover data from new file created above
lndcvr_na <- rast("data/processed/lndcvr_na.tif")

# reproject anthropogenic biomes data to WGS84
# anth_biome <- project(anth_biome, "EPSG:4326")

# resample anth_biome to change resolution
# anth_biome <- resample(anth_biome, soil_temp_0_5)

# crop anth_biome to study extent
# anth_biome <- crop(anth_biome, na_bound)

# write anth_biome to file for faster computation
# anth_biome <- writeRaster(anth_biome, filename = "data/processed/anth_biome.tif")

# read in processed anth_biome raster
anth_biome <- rast("data/processed/anth_biome.tif")

# create an empty raster template for shapefiles (currently vector objects)
# create a temporary raster with number of columns and rows from other rasters
# dim(lndcvr_na)
# temprast <- rast(climate_zones_vect, ncols = 12247, nrows = 8024)

# create raster from SpatVector and structure of temporary raster
# select "ZONE" layer from BEC data
# climate_zones <- rasterize(climate_zones_vect, temprast, field = "Climate")
# plot(climate_zones)

# climate_zones <- project(climate_zones, "EPSG:4326")
# climate_zones<- resample(climate_zones, lndcvr_na)
# climate_zones <- crop(climate_zones, na_bound)

# write to file for faster computation
# climate_zones <- writeRaster(climate_zones, filename = "data/processed/climate_zones.tif")

# read in climate_zones data from file
climate_zones <- rast("data/processed/climate_zones.tif")

# repeat above steps for protected areas data
# IUCN categories
# protect_area_IUCN <- rasterize(protect_area_IUCN_vect, temprast, field = "TYPE_PA")
# protect_area_IUCN <- project(protect_area_IUCN, "EPSG:4326")
# protect_area_IUCN <- resample(protect_area_IUCN, lndcvr_na)
# protect_area_IUCN <- crop(protect_area_IUCN, na_bound)
# plot(protect_area_IUCN)

# write to file for faster computation
# protect_area_IUCN <- writeRaster(protect_area_IUCN, filename = "data/processed/protect_area_IUCN.tif")

# read in protected areas IUCN data from file
protect_area_IUCN <- rast("data/processed/protect_area_IUCN.tif")

# OECMs
# protect_area_OECM <- rasterize(protect_area_OECM_vect, temprast, field = "TYPE_PA")
# protect_area_OECM <- project(protect_area_OECM, "EPSG:4326")
# protect_area_OECM <- resample(protect_area_OECM, lndcvr_na)
# protect_area_OECM <- crop(protect_area_OECM, na_bound)
# plot(protect_area_OECM)

# write to file for faster computation
# protect_area_OECM <- writeRaster(protect_area_OECM, filename = "data/processed/protect_area_OECM.tif")

# read in protected areas IUCN data from file
protect_area_OECM <- rast("data/processed/protect_area_OECM.tif")

# repeat above steps for watershed data
# watersheds <- rasterize(watersheds_vect, temprast, field = "NAW4_EN")
# watersheds <- project(watersheds, "EPSG:4326")
# watersheds <- resample(watersheds, lndcvr_na)
# watersheds <- crop(watersheds, na_bound)
# plot(watersheds)

# write to file for faster computation
# watersheds <- writeRaster(watersheds, filename = "data/processed/watersheds.tif")

# read in protected areas IUCN data from file
watersheds <- rast("data/processed/watersheds.tif")


# create a multilayer raster of the predictor variables
# can do up to 16 different layers 
  # therefore, likely have to leave out precip and tavg_mar_jun, since they have
  # 12 and 4 layers, respectively
predictors_multirast <- c(anth_biome, 
                          climate_zones, 
                          elevation_na, 
                          lndcvr_na, 
                          # precip, 
                          protect_area_IUCN, 
                          protect_area_OECM, 
                          soil_phh2o_0_5, 
                          soil_phh2o_5_15, 
                          soil_temp_0_5, 
                          soil_temp_5_15, 
                          # tavg_mar_jun, 
                          watersheds)

# mask the multilayer raster so the values outside of na_bound are NA
predictors_multirast <- mask(predictors_multirast, na_bound)


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

                    
                    