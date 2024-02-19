# New (reduced) extent
# required in order for projection code to run for tidysdm_ranunculus_multirast.R

# North American extent (west coast to continental divide)
# new geographic extent created in continental_divide.Rmd
# read in shapefile so we can calculate the area
na_bound_sf <- read_sf("data/raw/continental_divide_buffer_boundary.shp")
plot(na_bound_sf)
crs(na_bound_sf) # WGS84

xlims <- c(ext(ran_occ_sf)$xmin - 2, ext(ran_occ_sf)$xmax + 2)
ylims <- c(ext(ran_occ_sf)$ymin - 2, ext(ran_occ_sf)$ymax + 15)
# additional space north to account for potential northward shifts

# xlims <- c(-130, -102.5)
# ylims <- c(30, 70)

# now crop and mask all layers:
extent.test <- terra::ext(xlims, ylims)
predictors_multi <- crop(predictors_multi, extent.test)
ran_occ_sf <- st_crop(ran_occ_sf, extent.test)
na_bound <- crop(na_bound, extent.test)
na_bound_rast <- crop(na_bound_rast, extent.test)
na_bound_sf <- st_crop(na_bound_sf, extent.test)

# mask the multiraster to the extent (all values outside na_bound set to NA)
predictors_multi <- mask(predictors_multi, na_bound)

# vectorize na_bound_sf so we can use it as a mask
na_bound_vect <- vect(na_bound_sf)
na_bound <- mask(na_bound, na_bound_vect)
# turn back into sf object so we can compute area
na_bound_sf_masked <- st_as_sf(na_bound)

# study area calculations:

# reproject CRS to BC Albers (equal area projection, EPSG:3005) for calculating area
na_bound_area <- st_transform(na_bound_sf_masked, "EPSG:3005")
na_bound_area <- st_set_crs(na_bound_sf_masked, "EPSG:3005")
# calculate study area, in m^2 (default)
na_bound_area <- st_area(na_bound_sf_masked) # 3.9e+12 m^2
# convert from m^2 to km^2
na_bound_area <- st_area(na_bound_sf_masked)/1000000
na_bound_area <- units::set_units(st_area(na_bound_sf_masked), km^2) # 3 898 033 km^2


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



