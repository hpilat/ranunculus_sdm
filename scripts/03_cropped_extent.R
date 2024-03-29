# This script prepares our spatial extent for input into the tidysdm pipeline
# Please first run these scripts in the following order:
# 01_data_download_ranunculus.R
# 02_continental_divide.Rmd


library(tidyverse)
library(sf)
library(terra)

### Spatial Extents ###

# original extent created in 02_continental_divide.R
# original extent = too large of files for input, huge RAM requirements
# have to reduce our spatial extent from west coast of North America
# new spatial extent covers occurrence records + buffer
# required in order for projection code to run for tidysdm_ranunculus_multirast.R



### North American extent (original, west coast to continental divide) ###



# Geographic extent created in continental_divide.Rmd
# read in shapefile so we can reduce the extent
na_bound_vect <- vect("data/extents/continental_divide_buffer_boundary.shp")

# read in same file as an sf object so we can calculate our study area
na_bound_sf <- read_sf("data/extents/continental_divide_buffer_boundary.shp")

# created an empty raster based on study extent to use as a basemap for TidySDM, 
  # to be cropped down below
temprast <- rast(na_bound_vect, ncols = 12247, nrows = 8024)
na_bound_rast <- rasterize(na_bound_vect, temprast)

# create new bounds for reduced extent, based on our realized niche area
# first need to bring in occurrence data
ran_occ_download <- read.csv(file = "data/raw/ran_occ_download.csv")

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

# extend limits with 2 degree buffer in all directions
  # and additional space north to account for potential northward shifts
xlims <- c(ext(ran_occ_sf)$xmin - 2, ext(ran_occ_sf)$xmax + 2)
ylims <- c(ext(ran_occ_sf)$ymin - 2, ext(ran_occ_sf)$ymax + 15)

# xlims <- c(-130, -102.5)
# ylims <- c(30, 70)

# now crop all layers:
extent.test <- terra::ext(xlims, ylims)
na_bound_rast <- crop(na_bound_rast, extent.test)
na_bound_vect <- crop(na_bound_vect, extent.test)
na_bound_sf <- st_crop(na_bound_sf, extent.test)
ran_occ_sf <- st_crop(ran_occ_sf, extent.test)

# write na_bound_rast to file for reuse
writeRaster(na_bound_rast, filename = "data/extents/na_bound_rast.tif", overwrite = TRUE)

# write na_bound_vect to file for use in tidysdm as a mask
writeVector(na_bound_vect, filename = "data/extents/na_bound_vect.shp", overwrite = TRUE)

# write sf object to file for calculating area in tidysdm
st_write(na_bound_sf, dsn = "data/extents/na_bound_sf.shp", append = FALSE)

# write ran_occ_sf to file
st_write(ran_occ_sf, dsn = "data/extents/ran_occ_sf.shp", append = FALSE)



### Skeetchestn territory: ###



# SNRC provided shapefile of Skeetchestn traditional territory
# Read in Skeetchestn territory shapefile
skeetch_sf <- st_read("data/raw/SkeetchestnTT_2020/SkeetchestnTT_2020.shp")
plot(skeetch_sf)

# maps from tidysdm were cutting off part of the Territory boundary
# need to expand extent by a small amount
# expand extent by 0.5 degrees in each direction
xlims_skeetch <- c(ext(skeetch_sf)$xmin - 0.5, ext(skeetch_sf)$xmax + 0.5)
ylims_skeetch <- c(ext(skeetch_sf)$ymin - 0.5, ext(skeetch_sf)$ymax + 0.5)
extent_skeetch <- terra::ext(xlims_skeetch, ylims_skeetch)

# crop original sf object to new extent
skeetch_vect_extended <- st_crop(skeetch_sf, extent_skeetch)
skeetch_vect_extended

# write to file for reuse
st_write(skeetch_vect_extended, dsn = "data/extents/skeetch_vect_cropped_albers.shp", append = FALSE)
