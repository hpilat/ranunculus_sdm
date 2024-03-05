#### Projecting to the Future ####

# This script projects our bioclim30s model to present day predictions of Ranunculus
# habitat suitability
# please first run the following scripts in the following order:
# 01_data_download.R
# 02_continental_divide.Rmd
# 03_cropped_extent.R
# 04_data_processing
# 05_ranunculus_bioclim_30s.R

library(tidysdm)

# need to bring in climate_future:
climate_future <- 




# error in code chunk below: altitude not available for future predictions,
# but included in vars_uncorr - need to remove altitude then later paste the 
# values from climate_present into climate_future
# select uncorrelated variables
climate_future_uncorr <- climate_future[[predictors_uncorr]]
climate_future_uncorr

# predict using the ensemble:
prediction_future <- predict_raster(ran_ensemble, climate_future_uncorr)

ggplot() +
  geom_spatraster(data = prediction_future, aes(fill = mean)) +
  scale_fill_terrain_c()

# write to file:
writeRaster(prediction_future, filename = "outputs/ran_predict_future_bioclim30s.tif")