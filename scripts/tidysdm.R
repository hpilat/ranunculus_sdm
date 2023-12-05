## tidysdm package tutorial

library(tidysdm)


## Data Preparation ##


# read in a set of presences for a species of lizard that inhabits the Iberian Peninsula
data(lacerta)
lacerta

# plot the presences on a map to visualize them
# cast coordinates into an sf object and set its CRS to WGS84
library(sf)
lacerta <- st_as_sf(lacerta, coords = c("longitude", "latitude"))
st_crs(lacerta) <- 4326 # WGS84

# plot the locations directly on the raster used to extract climatic variables
# get land mask for available datasets and use that as background for locations
library(pastclim)
worldclim <- download_dataset(dataset = "WorldClim_2.1_10m", bio_variables = NULL, 
                 annual = TRUE, monthly = FALSE)
land_mask <- 
  get_land_mask(time_ce = 1985, dataset = "WorldClim_2.1_10m")

# Iberian Peninsula extension
iberia_poly <- 
  terra::vect("POLYGON((-9.8 43.3,-7.8 44.1,-2.0 43.7,3.6 42.5,3.8 41.5,1.3 40.8,0.3 39.5,
     0.9 38.6,-0.4 37.5,-1.6 36.7,-2.3 36.3,-4.1 36.4,-4.5 36.4,-5.0 36.1,
    -5.6 36.0,-6.3 36.0,-7.1 36.9,-9.5 36.6,-9.4 38.0,-10.6 38.9,-9.5 40.8,
    -9.8 43.3))"
  )

crs(iberia_poly) <- "lonlat"
# crop the extent
land_mask <- crop(land_mask, iberia_poly)
# and mask to the polygon
land_mask <- mask(land_mask, iberia_poly)

# use tidyterra for plotting so terra rasters can be plotted with ggplot
library(tidyterra)
library(ggplot2)
ggplot() +
  geom_spatraster(data = land_mask, aes(fill = land_mask_1985)) +
  geom_sf(data = lacerta)


## Thinning Occurrences ##

# thin the observations to have one per cell in the raster
  # better when using an equal area projection
set.seed(1234567)
lacerta <- thin_by_cell(lacerta, raster = land_mask)
nrow(lacerta)

# plot the thinned occurrences
ggplot() +
  geom_spatraster(data = land_mask, aes(fill = land_mask_1985)) +
  geom_sf(data = lacerta)

# thin occurrences further  to remove points closer than 20km
  # use km2m to reduce number of 0s written for 20km
set.seed(1234567)
lacerta_thin <- thin_by_dist(lacerta, dist_min = km2m(20))
nrow(lacerta_thin)

# plot the thinned occurrences again
ggplot() +
  geom_spatraster(data = land_mask, aes(fill = land_mask_1985)) +
  geom_sf(data = lacerta_thin)


##  Sample Pseudoabsences  ##

# constrain pseudoabsences to be minimum 50km away from presence points
# select 3 times as many pseudoabsences as presences
set.seed(1234567)
lacerta_thin <- sample_pseudoabs(lacerta_thin, 
                                 n = 3 * nrow(lacerta_thin), 
                                 raster = land_mask, 
                                 coords = NULL, 
## error message said coords vector (x and y column names) needs to be specified,
## internal function wants a data.frame with coordinates and columns with x and y
                                 method = c("dist_min"), km2m(50))

##  Sample Pseudoabsences  ##
# constrain pseudoabsences to be minimum 50km away from presence points
# select 3 times as many pseudoabsences as presences
set.seed(1234567)

#lacerta_thin_withcoords <- lacerta_thin %>% dplyr::bind_cols(sf::st_coordinates(lacerta_thin))

lacerta_thin_new <- sample_pseudoabs(data = lacerta_thin, 
                                     n = 3 * nrow(lacerta_thin), 
                                     raster = land_mask, 
                                     coords = c("X", "Y"), 
                                     method = c("dist_min"), km2m(50))

#                                 Error in `dplyr::bind_rows()`:
#                                   ! Can't combine `..1$class` <character> and `..2$class` <double>.
