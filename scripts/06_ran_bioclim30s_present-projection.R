# This script projects our bioclim30s model to present day predictions of Ranunculus
  # habitat suitability
# please first run the following scripts in the following order:
  # 01_data_download.R
  # 02_continental_divide.Rmd
  # 03_cropped_extent.R
  # 04_data_processing
  # 05_ranunculus_bioclim_30s.R

library(tidysdm)


## Projecting to the Present ##


# need to bring back in climate_present
climate_present <- 

suggested_vars <- c("bio06",
                    "bio12",
                    "bio10",
                    "bio19",
                    "bio07")

climate_present_selected <- climate_present[[suggested_vars]]

# make predictions with the ensemble
ran_ensemble <- rast("outputs/ran_ensemble_bioclim30s.tif")
# prediction_present <- predict_raster(ran_ensemble, climate_present_uncorr)
# ggplot() +
#  geom_spatraster(data = prediction_present, aes(fill = mean)) +
#  scale_fill_terrain_c() + # "c" for continuous variables
# plot the presences used in the model
#  geom_sf(data = ran_pres_abs_pred %>% filter(class == "presence"))

# subset the model to only use the best models, based on AUC
# set threshold of 0.8 for AUC
# take the median of the available model predictions (mean is the default)
prediction_present_best <- predict_raster(ran_ensemble, climate_present_selected, 
                                          metric_thresh = c("roc_auc", 0.8), 
                                          fun= "mean")

ggplot() +
  geom_spatraster(data = prediction_present_best, aes(fill = mean)) +
  scale_fill_terrain_c() # + # c = continuous
# geom_sf(data = ran_pres_abs_pred %>% filter(class == "presence"))
# if plot doesn't change much, models are consistent
# model gives us probability of occurrence

# write to file
writeRaster(prediction_present_best, filename = "outputs/ran_predict_present_bioclim30s.tif")