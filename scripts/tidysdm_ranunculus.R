# tidysdm tutorial
library(tidysdm)
library(tidyterra)
library(sf)
library(pastclim)

# read in Ranunculus glaberrimus presence dataframe, 
  # dataframe with ID, latitude, and longitude columns
ran_occ

# plot the presences on a map to visualize them
# cast coordinates into an sf object and set its CRS to WGS84
ran_occ <- st_as_sf(ran_occ, coords = c("decimalLongitude", "decimalLatitude"))
# set CRS to WGS84
st_crs(ran_occ) <- 4326

# crop the multiraster with predictors to the extent of interest
predictors_multirast <- crop(predictors_multirast, bc_bound)

# mask the multiraster to the extent
predictors_mask <- mask(predictors_multirast, bc_bound)

# plot the points on the raster that contains the climatic variables
library(ggplot2)
ggplot() +
  geom_raster(data = predictors_multirast, aes(fill = pred_multirast)) +
  geom_point(data = ran_occ_vect, aes(x = longitude, y = latitude), color = "green3")
