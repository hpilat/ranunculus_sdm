# tidysdm tutorial
library(tidysdm)
library(tidyterra)

# read in Ranunculus glaberrimus presence dataframe, 
  # dataframe with ID, latitude, and longitude columns turned to SpatVector
ran_occ

# create a SpatVector object for the occurrence data
ran_occ_vect <- vect(ran_occ, geom = c("longitude", "latitude"), 
                     crs = "EPSG:4326", keepgeom = FALSE)

# crop the SpatVector to the extent of British Columbia
ran_occ_bc <- crop(ran_occ_vect, bc_extent_rast)
ran_occ_bc

# plot the points on the raster that contains the climatic variables
library(ggplot2)
ggplot() +
  geom_raster(data = pred_multirast, aes(fill = pred_multirast)) +
  geom_point(data = ran_occ_vect, aes(x = longitude, y = latitude), color = "green3")
