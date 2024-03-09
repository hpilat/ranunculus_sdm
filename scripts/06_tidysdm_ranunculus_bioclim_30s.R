# Following tidysdm tutorial, we input Ranunculus glaberrimus occurrence records 
  # and WorldClim predictors at 30 arcsec resolution into the tidysdm pipeline
# Please first run scripts in the following order: 
  # 01_data_download.R
  # 02_continental_divide.Rmd
  # 03_cropped_extent.R
  # 04_data_processing.R


library(tidysdm)
library(tidyterra)
library(sf)
library(terra)
library(ggplot2)
library(overlapping)

# North American extent (west coast to continental divide)
# new geographic extent created in 02_continental_divide.Rmd
# extent cropped to smaller extent in 03_data_prep_ranunculus.R
# read in extent objects:
# raster to use as a basemap
na_bound_rast <- rast("data/extents/na_bound_rast.tif")
# vector object to use for masking and area calculations
na_bound_vect <- vect("data/extents/na_bound_vect.shp")
# sf object masked to study extent, for area calculations
na_bound_sf <- read_sf("data/extents/na_bound_sf.shp")

# read in Ranunculus glaberrimus occurrences:
# cropped to proper study extent in 03_data_prep_ranunculus.R
ran_occ_vect <- vect("data/processed/ran_occ_masked.shp")
# mask to study area (all occurrences outside bounds set to NA)
ran_occ_vect <- mask(ran_occ_vect, na_bound_vect)
# cast to sf object
ran_occ_sf <- st_as_sf(ran_occ_vect)

# plot occurrences directly on raster with predictor variables
# read in processed WorldClim rasters
climate_present <- rast("data/processed/worldclim_present_masked.tif")
climate_future <- rast("data/processed/worldclim_future_masked.tif")

# use tidyterra package for plotting so ggplot can be used with terra rasters
# aes(fill = layer) refers to column name in na_bound_rast
ggplot()+
  geom_spatraster(data = na_bound_rast, aes(fill = layer)) +
  geom_sf(data = ran_occ_sf) # sf object with coordinates



### Thinning Occurrences ###



# thin the occurrences to have one per cell in the na_bound_rast raster

set.seed(1234567)
ran_occ_thin_cell <- thin_by_cell(ran_occ_sf, raster = na_bound_rast)
nrow(ran_occ_thin_cell) # 2462

ggplot() +
  geom_spatraster(data = na_bound_rast, aes(fill = layer)) +
  geom_sf(data = ran_occ_thin_cell) # thinned occurrences

# thin further to remove points closer than 5km
# default is metres, could input 5000 or use km2m(5)
# attempted 5km, filter_high_cor below wouldn't run, so try 10
# 10 still didn't work, try 15?
set.seed(1234567)
ran_occ_thin_dist <- thin_by_dist(ran_occ_sf, dist_min = km2m(15))
nrow(ran_occ_thin_dist) # 1400 at 5km thinning, 1046 at 10km thinning
# 1040 at 10km thinning with reduced spatial extent
  # but failed to project with high res bioclim data
# 827 at 15 km thinning
# 653 at 20km spatial thinning

ggplot() +
  geom_spatraster(data = na_bound_rast, aes(fill = layer)) +
  geom_sf(data = ran_occ_thin_dist)



### Pseudoabsences ###



# sample pseudoabsences/background points
# constrain pseudoabsences to be between 5 and 15km from any presences
# choice of 5 and 15km is arbitrary
# select 10 times as many pseudoabsences as presences 
# (recommended 10 000 pseudoabsences by lit review)

set.seed(1234567)
ran_pres_abs <- sample_pseudoabs(ran_occ_thin_dist, 
                                 n = 10 * nrow(ran_occ_thin_dist), 
                                 raster = na_bound_rast, 
                                 method = c("dist_disc", km2m(50), km2m(75))
                                 )
nrow(ran_pres_abs) 
# 11 440 with 10km thinning
# 9097 with 15km thinning
# 7183 with 20km thinning

# plot presences and absences
ggplot() +
  geom_spatraster(data = na_bound_rast, aes(fill = layer)) +
  geom_sf(data = ran_pres_abs, aes(col = class))



### Variable Selection ###

# Extract variables from predictors_multirast for all presences and pseudoabsences
summary(climate_present) # 50 000 + NAs per column
nrow(ran_pres_abs) # 11 440
nrow(climate_present) # 4042

ran_pres_abs_pred <- ran_pres_abs %>% 
  bind_cols(terra::extract(climate_present, ran_pres_abs, ID = FALSE, na.rm = TRUE))
nrow(ran_pres_abs_pred) # 11 440
# after this step, no NA values in ran_occ_thin from tutorial
# but I have NA values from predictors_multi
summary(ran_pres_abs_pred) # still some NAs, bioclim script magically has none at this point

# remove rows with NA values
ran_pres_abs_pred <- na.omit(ran_pres_abs_pred)
nrow(ran_pres_abs_pred) # 11 415, 25 rows removed
summary(ran_pres_abs_pred) # No NA values

# skipped non-overlapping distribution step in tutorial

# need a smaller sample to calculate collinearity between variables

# try sample size of 5000 cells
set.seed(1234567)
predictors_sample <- terra::spatSample(climate_present, size = 5000, 
                                       method = "random", replace = FALSE, 
                                       na.rm = FALSE, as.raster = TRUE,
                                       values = TRUE, cells = FALSE, xy = TRUE)

pairs(predictors_sample)

# subset to variables below 0.8 Pearson's correlation coefficient
# climate_present = SpatRaster with predictor data (all numeric, no NAs)

predictors_uncorr <- filter_high_cor(predictors_sample, cutoff = 0.8, 
                                     verbose = TRUE, names = TRUE, to_keep = NULL)
predictors_uncorr

# remove highly correlated predictors
# here is where the "class" column gets dropped, which messes up recipe below
# need to retain class column (not in original tutorial code)
ran_pres_abs_pred <- ran_pres_abs_pred %>% dplyr::select(dplyr::all_of(c(predictors_uncorr, "class")))
ran_pres_abs_pred

# now subset the uncorrelated predictors from climate_present
climate_present_selected <- climate_present[[predictors_uncorr]]



#### Fit the model by cross-validation ####



# use a recipe to define how to handle our dataset
# need to define the formula (class is the outcome, all other variables are predictors)
# for sf objects, geometry is auto-replaced by X and Y and assigned as coords, therefore not used as predictors
ran_recipe <- recipe(ran_pres_abs_pred, formula = class ~ .)
ran_recipe

# tidymodels assumes the level of interest for the response (presences) is the reference level
# confirm the data are correctly formatted
ran_pres_abs_pred %>% check_sdm_presence(class)

# build a workflow_set of different models, defining which hyperparameters we want to tune
# for most commonly used models, tidysdm auto chooses the most important parameters
ran_models <- 
  workflow_set(
    preproc = list(default = ran_recipe), 
    models = list(
      glm = sdm_spec_glm(), # standard GLM specs
      rf = sdm_spec_rf(), # rf specs with tuning
      gbm = sdm_spec_boost_tree(), # boosted tree specs with tuning
      maxent = sdm_spec_maxent() # maxent specs with tuning
    ), 
    # make all combos of preproc and models:
    cross = TRUE
  ) %>% 
  # tweak controls to store information needed later to create the ensemble
  option_add(control = control_ensemble_grid())

# set up spatial block cross-validation to tune and assess models:
# 80:20 split with 5 folds (v = 5) (supported by literature review)
set.seed(100)
ran_cross_val <- spatial_block_cv(ran_pres_abs_pred, v = 5)
autoplot(ran_cross_val)

# use block CV folds to tune and assess models
# tutorial uses 3 combos of hyperparameters and says this is far too few for real life
# 10 combos of hyperparameters = ~ 3 mins computation time, less crowded plots
# 20 combos of hyperparameters = ~ 3 mins computation time, crowded plots
# 10 combos = 25 minute computation time on February 14th
set.seed(1234567)
ran_models <- 
  ran_models %>% 
  workflow_map("tune_grid", 
               resamples = ran_cross_val, grid = 10, # attempting 10 combos of hyperparameters
               metrics = sdm_metric_set(), verbose = TRUE
  ) 

# want workflow_set to correctly detect no tuning parameters for GLM
# inspect performance of models:
autoplot(ran_models)
model_metrics <- collect_metrics(ran_models)




## Ensemble ##



# use AUC as metric to choose best random forest and boosted tree models
# when adding members to an ensemble, they are auto-fitted to the full
  # training dataset and therefore ready to make predictions
ran_ensemble <- simple_ensemble() %>% 
  add_member(ran_models, metric = "roc_auc")
# can also use roc_auc and tss_max as metrics
ran_ensemble
autoplot(ran_ensemble)
# need to have tidysdm version 0.9.3 or greater for this to work
ran_ensemble_metrics <- collect_metrics(ran_ensemble)



## Projecting to the Present ##



# make predictions with the ensemble
# prediction_present <- predict_raster(ran_ensemble, climate_present_uncorr)
# ggplot() +
#  geom_spatraster(data = prediction_present, aes(fill = mean)) +
#  scale_fill_terrain_c() + # "c" for continuous variables


# subset the model to only use the best models, based on AUC
# set threshold of 0.8 for AUC
# take the median of the available model predictions (mean is the default)
prediction_present_best <- predict_raster(ran_ensemble, 
                                          climate_present_selected, 
                                          metric_thresh = c("roc_auc", 0.8), 
                                          fun= "mean")

ggplot() +
  geom_spatraster(data = prediction_present_best, aes(fill = mean)) +
  scale_fill_terrain_c() +
  labs(title = "Ranunculus Present Prediction", subtitle = "Bioclim Model", xlab = "Longitude", ylab = "Latitude")
 # geom_sf(data = ran_pres_abs_pred %>% filter(class == "presence"))
# if plot doesn't change much, models are consistent
# model gives us probability of occurrence

# write to file
writeRaster(prediction_present_best, filename = "outputs/ran_bioclim30s_predict-present.tif", overwrite = TRUE)


# can convert to binary predictions (present vs absence)

ran_ensemble_binary <- calib_class_thresh(ran_ensemble, 
                                          class_thresh = "tss_max"
                                          )

prediction_present_binary <- predict_raster(ran_ensemble_binary, 
                                            climate_present_selected, 
                                            type = "class", 
                                            class_thresh = c("tss_max"))
prediction_present_binary

# plot the binary map
ggplot() +
  geom_spatraster(data = prediction_present_binary, aes(fill = binary_mean)) +
  labs(title = "Ranunculus Present Prediction", subtitle = "Bioclim Model", xlab = "Longitude", ylab = "Latitude") # +
  # geom_sf(data = ran_pres_abs_pred %>% filter(class == "presence"))

# write to file
writeRaster(prediction_present_binary, filename = "outputs/ran_bioclim30s_predict-present-binary.tif", overwrite = TRUE)



#### Projecting to the Future ####



# all 19 bioclimatic variables, no altitude (because it doesn't change over time)
# select a subset of 5 uncorrelated predictors, using suggested_vars from before:
climate_future_selected <- climate_future[[predictors_uncorr]]
climate_future_selected

# predict using the ensemble:
# prediction_future <- predict_raster(ran_ensemble, climate_future_selected)

# ggplot() +
 # geom_spatraster(data = prediction_future, aes(fill = mean)) +
 # scale_fill_terrain_c()

prediction_future_best <- predict_raster(ran_ensemble, 
                                         climate_future_selected, 
                                         metric_thresh = c("roc_auc", 0.8), 
                                         fun= "mean")

ggplot() +
  geom_spatraster(data = prediction_future_best, aes(fill = mean)) +
  scale_fill_terrain_c() +
  labs(title = "Ranunculus Future Prediction", subtitle = "Bioclim Model", xlab = "Longitude", ylab = "Latitude")

# write to file
writeRaster(prediction_future_best, filename = "outputs/ran_predict_future_bioclim30s.tif", overwrite = TRUE)



# convert predictions to binary (presence/absence)


ran_ensemble_binary <- calib_class_thresh(ran_ensemble, 
                                          class_thresh = "tss_max"
                                          )

prediction_future_binary <- predict_raster(ran_ensemble_binary, 
                                           climate_future_selected, 
                                           type = "class", 
                                           class_thresh = c("tss_max"))
prediction_future_binary

ggplot() +
  geom_spatraster(data = prediction_future_binary, aes(fill = binary_mean)) +
  labs(title = "Ranunculus Future Prediction", subtitle = "Bioclim Model", xlab = "Longitude", ylab = "Latitude")#+
 # geom_sf(data = ran_pres_abs_pred %>% filter(class == "presence"))

# write to file
writeRaster(prediction_future_binary, filename = "outputs/ran_bioclim30s_predict-future-binary.tif", overwrite = TRUE)



#### Visualizing the Contribution of Individual Variables ####



# for a written explanation of variable importance:
# using DALEX library, integrated with tidysdm
# create an explainer object
library(DALEX)
explainer_ran_ensemble <- explain_tidysdm(ran_ensemble)
vip_ensemble <- model_parts(explainer = explainer_ran_ensemble, 
                            type = "variable_importance")
vip_ensemble
plot(vip_ensemble)


# Marginal Response Curves


# marginal response curves can show the effect of a variable while keeping
  # all other variables at their mean
# use step_profile() to create a new recipe for generating a dataset to make 
  # the marginal prediction
# uncorrelated predictors:
predictors_uncorr


# investigate the contribution of bio02:
bio02_prof <- ran_recipe %>%  # recipe from above
  step_profile(-bio02, profile = vars(bio02)) %>% 
  prep(training = ran_pres_abs_pred)

bio02_data <- bake(bio02_prof, new_data = NULL)

bio02_data <- bio02_data %>% 
  mutate(
    pred = predict(ran_ensemble, bio02_data)$mean
  )

ggplot(bio02_data, aes(x = bio02, y = pred)) +
  geom_point(alpha = .5, cex = 1)


# investigate the contribution of bio07:
bio03_prof <- ran_recipe %>%  # recipe from above
  step_profile(-bio03, profile = vars(bio03)) %>% 
  prep(training = ran_pres_abs_pred)

bio03_data <- bake(bio03_prof, new_data = NULL)

bio03_data <- bio03_data %>% 
  mutate(
    pred = predict(ran_ensemble, bio03_data)$mean
  )

ggplot(bio03_data, aes(x = bio03, y = pred)) +
  geom_point(alpha = .5, cex = 1)


# investigate the contribution of bio05:
bio04_prof <- ran_recipe %>%  # recipe from above
  step_profile(-bio04, profile = vars(bio04)) %>% 
  prep(training = ran_pres_abs_pred)

bio04_data <- bake(bio04_prof, new_data = NULL)

bio04_data <- bio04_data %>% 
  mutate(
    pred = predict(ran_ensemble, bio04_data)$mean
  )

ggplot(bio04_data, aes(x = bio04, y = pred)) +
  geom_point(alpha = .5, cex = 1)


# investigate the contribution of bio08:
bio08_prof <- ran_recipe %>%  # recipe from above
  step_profile(-bio08, profile = vars(bio08)) %>% 
  prep(training = ran_pres_abs_pred)

bio08_data <- bake(bio08_prof, new_data = NULL)

bio08_data <- bio08_data %>% 
  mutate(
    pred = predict(ran_ensemble, bio08_data)$mean
  )

ggplot(bio08_data, aes(x = bio08, y = pred)) +
  geom_point(alpha = .5, cex = 1)


# investigate the contribution of bio09:
bio09_prof <- ran_recipe %>%  # recipe from above
  step_profile(-bio09, profile = vars(bio09)) %>% 
  prep(training = ran_pres_abs_pred)

bio09_data <- bake(bio09_prof, new_data = NULL)

bio09_data <- bio09_data %>% 
  mutate(
    pred = predict(ran_ensemble, bio09_data)$mean
  )

ggplot(bio09_data, aes(x = bio09, y = pred)) +
  geom_point(alpha = .5, cex = 1)


# investigate the contribution of bio14:
bio14_prof <- ran_recipe %>%  # recipe from above
  step_profile(-bio14, profile = vars(bio14)) %>% 
  prep(training = ran_pres_abs_pred)

bio14_data <- bake(bio14_prof, new_data = NULL)

bio14_data <- bio14_data %>% 
  mutate(
    pred = predict(ran_ensemble, bio14_data)$mean
  )

ggplot(bio14_data, aes(x = bio14, y = pred)) +
  geom_point(alpha = .5, cex = 1)


# investigate the contribution of bio15:
bio15_prof <- ran_recipe %>%  # recipe from above
  step_profile(-bio15, profile = vars(bio15)) %>% 
  prep(training = ran_pres_abs_pred)

bio15_data <- bake(bio15_prof, new_data = NULL)

bio15_data <- bio15_data %>% 
  mutate(
    pred = predict(ran_ensemble, bio15_data)$mean
  )

ggplot(bio15_data, aes(x = bio15, y = pred)) +
  geom_point(alpha = .5, cex = 1)


# investigate the contribution of bio18:
bio18_prof <- ran_recipe %>%  # recipe from above
  step_profile(-bio18, profile = vars(bio18)) %>% 
  prep(training = ran_pres_abs_pred)

bio18_data <- bake(bio18_prof, new_data = NULL)

bio18_data <- bio18_data %>% 
  mutate(
    pred = predict(ran_ensemble, bio18_data)$mean
  )

ggplot(bio18_data, aes(x = bio18, y = pred)) +
  geom_point(alpha = .5, cex = 1)



## Repeated Ensembles ##



# explore the effect of thinning and sampling pseudoabsences on model performance
# create a list of simple_ensembles by looping through the SDM pipeline
# create an empty object to store the simple ensembles we will create:
ensemble_list <- list()
set.seed(123) # make sure seed is set outside of the loop

for (i_repeat in 1:3) {
  # thin the data
  ran_thin_rep <- thin_by_cell(ran_pres_abs_pred, raster = climate_present)
  ran_thin_rep <- thin_by_dist(ran_thin_rep, dist_min = 5000)
  # sample pseudo-absences
  ran_thin_rep <- sample_pseudoabs(ran_thin_rep,
                                       n = 3 * nrow(ran_thin_rep),
                                       raster = climate_present,
                                       method = c("dist_min", 50000)
  )
  # get climate
  ran_thin_rep <- ran_thin_rep %>%
    bind_cols(terra::extract(climate_present, ran_thin_rep, ID = FALSE))
  # create folds
  ran_thin_rep_cv <- spatial_block_cv(ran_thin_rep, v = 5) # 5 folds
  # create a recipe
  ran_thin_rep_rec <- recipe(ran_thin_rep, formula = class ~ .)
  # create a workflow_set
  ran_thin_rep_models <-
    # create the workflow_set
    workflow_set(
      preproc = list(default = ran_thin_rep_rec),
      models = list(
        # the standard glm specs
        glm = sdm_spec_glm(),
        # maxent specs with tuning
        maxent = sdm_spec_maxent()
      ),
      # make all combinations of preproc and models,
      cross = TRUE
    ) %>%
    # tweak controls to store information needed later to create the ensemble
    option_add(control = control_ensemble_grid())
  
  # train the model
  ran_thin_rep_models <-
    ran_thin_rep_models %>%
    workflow_map("tune_grid",
                 resamples = ran_thin_rep_cv, grid = 10,
                 metrics = sdm_metric_set(), verbose = TRUE
    )
  # make an simple ensemble and add it to the list
  ensemble_list[[i_repeat]] <- simple_ensemble() %>%
    add_member(ran_thin_rep_models, metric = "roc_auc")
}

# now create repeat_ensemble from the list:
ran_thin_rep_ens <- repeat_ensemble() %>% add_repeat(ensemble_list)
ran_thin_rep_ens

# predict by taking the mean and median of all models
ran_thin_rep_ens <- predict_raster(ran_thin_rep_ens, 
                                   climate_present, 
                                   fun = c("mean", "median"))

ggplot() +
  geom_spatraster(data = ran_thin_rep_ens, aes(fill = median)) +
  scale_fill_terrain_c() +
  labs(title = "Ranunculus Prediction Sensitivity", subtitle = "Bioclim Model", xlab = "Longitude", ylab = "Latitude")
# convert to binary and calculate area?
