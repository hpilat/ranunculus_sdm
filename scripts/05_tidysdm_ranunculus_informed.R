# Following tidysdm tutorial, we input Ranunculus glaberrimus occurrence records 
  # and informed predictors  at 30 arcsec resolution into the tidysdm pipeline
# Please first run scripts in the following order: 
  # 01_data_download.R
  # 02_continental_divide.Rmd
  # 03_cropped_extent.R
  # 04_data_processing.R
      
# dir.create("outputs/")


library(tidysdm)
library(tidyterra)
library(sf)
# library(pastclim)
library(ggplot2)
library(overlapping)
library(xgboost)

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

# read in multilayer raster with predictor data, created in
# 03_data_prep_ranunculus
predictors_multi <- rast("data/processed/predictors_multi.tif")


# plot occurrences directly on raster with predictor variables

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
  # 827 at 15km thinning
  # 635 at 20km thinning

ggplot() +
  geom_spatraster(data = na_bound_rast, aes(fill = layer)) +
  geom_sf(data = ran_occ_thin_dist)



### Pseudoabsences ###



# sample pseudoabsences/background points
# constrain pseudoabsences to be between 5 and 15km from any presences
  # choice of 5 and 15km is arbitrary
# select 10 times as many pseudoabsences as presences 
  # (recommended 10 000 pseudoabsences by lit review)
# ran_occ_thin will then have presences and pseudoabsences
set.seed(1234567)
ran_pres_abs <- sample_pseudoabs(ran_occ_thin_dist, 
                               n = 10 * nrow(ran_occ_thin_dist), 
                               raster = na_bound_rast, 
                               method = c("dist_disc", km2m(50), km2m(75))
                               )
nrow(ran_pres_abs) # 11 440 

# 5-50 discs = around 0.7 AUC for most models
# 15-50 discs = around 0.75 for most models and ensemble
# 50-75 discs = around 0.85 for most models and ensemble
# 50-100 discs - high performance but R aborted at projection phase

# plot presences and absences
ggplot() +
  geom_spatraster(data = na_bound_rast, aes(fill = layer)) +
  geom_sf(data = ran_pres_abs, aes(col = class))



### Variable Selection ###

# Extract variables from predictors_multirast for all presences and pseudoabsences
summary(predictors_multi) # 50 000 + NAs per column
nrow(ran_pres_abs) # 11 440
nrow(predictors_multi) # 4042, cropped to new extent hence fewer rows

ran_pres_abs_pred <- ran_pres_abs %>% 
  bind_cols(terra::extract(predictors_multi, ran_pres_abs, ID = FALSE, na.rm = TRUE))
nrow(ran_pres_abs_pred) # 11 440

# after this step, no NA values in ran_occ_thin (predictors_multi equivalent) from tutorial
  # but I have NA values from predictors_multi
summary(ran_pres_abs_pred) # still some NAs, bioclim script magically has none at this point

# remove rows with NA values
ran_pres_abs_pred <- na.omit(ran_pres_abs_pred)
nrow(ran_pres_abs_pred) # 11 310, 130 rows removed with 10 km thinning and 5-50km buffer distance
  # 11297 with 10 km thinning and 15-50 km buffer distance
  # 11236 with 10 km thinning and 20-70 km buffer distance
  # 7046 with 20 km thinning and 20-70 km buffer distance
  # 10 976 with 10 km thinning and 50-100 km buffer distance
  # 11 004 with 10 km thinning and 50-75 km buffer distance
  # 8772 with 15 km thinning and 50-75 buffer distance

# skipped non-overlapping distribution step in tutorial

# inspect the variables for collinearity
pairs(predictors_multi)

# may need a smaller sample to calculate collinearity between variables

# try sample size of 5000 cells
set.seed(1234567)
predictors_sample <- terra::spatSample(predictors_multi, size = 5000, 
                                       method = "random", replace = FALSE, 
                                       na.rm = FALSE, as.raster = TRUE,
                                       values = TRUE, cells = FALSE, xy = TRUE)


# subset to variables below 0.8 Pearson's correlation coefficient
# predictors_multi = SpatRaster with predictor data (all numeric, no NAs)

# below code was taking forever to run, but no delays in the bioclim code
# sub predictors_multi with predictors_sample if code below won't run
predictors_uncorr <- filter_high_cor(predictors_sample, cutoff = 0.8, 
                                     verbose = TRUE, names = TRUE, to_keep = NULL)
predictors_uncorr

# remove highly correlated predictors
# here is where the "class" column gets dropped, which messes up recipe below
  # need to retain class column (not in original tutorial code)
ran_pres_abs_pred <- ran_pres_abs_pred %>% dplyr::select(dplyr::all_of(c(predictors_uncorr, "class")))
ran_pres_abs_pred

# now subset the uncorrelated predictors within the multiraster
predictors_multi_input <- predictors_multi[[predictors_uncorr]]
predictors_multi_input



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



#### Ensemble ####



# select the best set of parameters for each model
# algorithms in the ensemble are auto-fitted to the full training dataset
  # therefore ready to make predictions
ran_ensemble <- simple_ensemble() %>% 
  add_member(ran_models, metric = "roc_auc") # or boyce_cont or tss_max
ran_ensemble
autoplot(ran_ensemble)

# a tabular form of the model metrics:
ran_ensemble_metrics <-  collect_metrics(ran_ensemble) 
# need tidysdm version > 0.9.3 for this to work



#### Projecting to the Present ####



# predictions using the ensemble
# default is taking the mean of the predictions from each model
# line below uses over 10GB of RAM
# need to input uncorrelated predictor data
# prediction_present_multirast <- predict_raster(ran_ensemble, predictors_multi_input)

# ggplot() +
  # geom_spatraster(data = prediction_present_multirast, aes (fill = mean)) +
  # scale_fill_terrain_c()# + # c for continuous
  # geom_sf(data = ran_pres_abs_pred %>% filter(class == "presence"))

# subset the ensemble to only use the best models (AUC > 0.8)
  # note: had to subset to 0.7 to include any models when pseudoabsence discs 
  # are closer than 50km from presence points
  # switched to 0.8 threshold for 50-75km pseudoabsence discs
# take the mean of the available model predictions (default is the mean)
prediction_present_best <- predict_raster(ran_ensemble, predictors_multi_input, 
                                         metric_thresh = c("roc_auc", 0.8), 
                                         fun = "mean"
                                         )

ggplot() +
  geom_spatraster(data = prediction_present_best, aes(fill = mean)) +
  scale_fill_terrain_c() +
  labs(title = "Ranunculus Present Prediction", subtitle = "Informed Model", xlab = "Longitude", ylab = "Latitude")

 #  geom_sf(data = ran_pres_abs_pred %>% filter(class == "presence"))

# write to file
writeRaster(prediction_present_best, filename = "outputs/ran_informed_prediction_present.tif", overwrite = TRUE)



# Binary Predictions
# clear unused R memory before running the below code - apparently need as much RAM as possible



# desirable to have binary predictions (presence/absence) rather than probability of occurrence
  # calibrate threshold used to convert probabilities into classes
ran_ensemble <- calib_class_thresh(ran_ensemble,
                                   class_thresh = "tss_max")

prediction_present_binary <- predict_raster(ran_ensemble, 
                                            predictors_multi_input, 
                                            type = "class", 
                                            class_thresh = c("tss_max") 
                                            )

ggplot() +
  geom_spatraster(data = prediction_present_binary, aes(fill = binary_mean)) +
  # geom_sf(data= ran_pres_abs_pred %>% filter(class == "presence")) +
  labs(title = "Ranunculus Present Prediction", subtitle = "Informed Model", xlab = "Longitude", ylab = "Latitude")

# write to file
writeRaster(prediction_present_binary, filename = "outputs/ran_informed_prediction_present_binary.tif")



#### Visualizing the Contribution of Individual Variables ####



# for a written explanation of variable importance:
# using DALEX library, integrated with tidysdm
# create an explainer object
library(DALEX)
explainer_ran_ensemble <- explain_tidysdm(ran_ensemble)
vip_ensemble <- model_parts(explainer = explainer_ran_ensemble, 
                            type = "variable_importance")
vip_ensemble


# marginal response curves can show the effect of a variable while keeping
# all other variables at their mean
# use step_profile() to create a new recipe for generating a dataset to make 
# the marginal prediction
# uncorrelated predictors:
predictors_uncorr

# investigate the contribution of anth_biome:
anth_biome_prof <- ran_recipe %>%  # recipe from above
  step_profile(-anth_biome, profile = vars(anth_biome)) %>% 
  prep(training = ran_pres_abs_pred)

anth_biome_data <- bake(anth_biome_prof, new_data = NULL)

anth_biome_data <- anth_biome_data %>% 
  mutate(
    pred = predict(ran_ensemble, anth_biome_data)$mean
  )

ggplot(anth_biome_data, aes(x = anth_biome, y = pred)) +
  geom_point(alpha = .5, cex = 1)


# investigate the contribution of Climate (climate_zones):
climate_zones_prof <- ran_recipe %>%  # recipe from above
  step_profile(-climate_zones, profile = vars(climate_zones)) %>% 
  prep(training = ran_pres_abs_pred)

climate_zones_data <- bake(climate_zones_prof, new_data = NULL)

climate_zones_data <- climate_zones_data %>% 
  mutate(
    pred = predict(ran_ensemble, climate_zones_data)$mean
  )

ggplot(climate_zones_data, aes(x = climate_zones, y = pred)) +
  geom_point(alpha = .5, cex = 1)


# investigate the contribution of ecoregions:
ecoregions_prof <- ran_recipe %>%  # recipe from above
  step_profile(-ecoregions, profile = vars(ecoregions)) %>% 
  prep(training = ran_pres_abs_pred)

ecoregions_data <- bake(ecoregions_prof, new_data = NULL)

ecoregions_data <- ecoregions_data %>% 
  mutate(
    pred = predict(ran_ensemble, ecoregions_data)$mean
  )

ggplot(ecoregions_data, aes(x = ecoregions, y = pred)) +
  geom_point(alpha = .5, cex = 1)


# investigate the contribution of elevation:
elevation_prof <- ran_recipe %>%  # recipe from above
  step_profile(-elevation, profile = vars(elevation)) %>% 
  prep(training = ran_pres_abs_pred)

elevation_data <- bake(elevation_prof, new_data = NULL)

elevation_data <- elevation_data %>% 
  mutate(
    pred = predict(ran_ensemble, elevation_data)$mean
  )

ggplot(elevation_data, aes(x = elevation, y = pred)) +
  geom_point(alpha = .5, cex = 1)


# investigate the contribution of lndcvr_na:
landcover_prof <- ran_recipe %>%  # recipe from above
  step_profile(-landcover, profile = vars(landcover)) %>% 
  prep(training = ran_pres_abs_pred)

landcover_data <- bake(landcover_prof, new_data = NULL)

landcover_data <- landcover_data %>% 
  mutate(
    pred = predict(ran_ensemble, landcover_data)$mean
  )

ggplot(landcover_data, aes(x = landcover, y = pred)) +
  geom_point(alpha = .5, cex = 1)


# investigate the contribution of soil_temp_5_15:
soil_temp_5_15_prof <- ran_recipe %>%  # recipe from above
  step_profile(-soil_temp_5_15, profile = vars(soil_temp_5_15)) %>% 
  prep(training = ran_pres_abs_pred)

soil_temp_5_15_data <- bake(soil_temp_5_15_prof, new_data = NULL)

soil_temp_5_15_data <- soil_temp_5_15_data %>% 
  mutate(
    pred = predict(ran_ensemble, soil_temp_5_15_data)$mean
  )

ggplot(soil_temp_5_15_data, aes(x = soil_temp_5_15, y = pred)) +
  geom_point(alpha = .5, cex = 1)


# investigate the contribution of watersheds:
watersheds_prof <- ran_recipe %>%  # recipe from above
  step_profile(-watersheds, profile = vars(watersheds)) %>% 
  prep(training = ran_pres_abs_pred)

watersheds_data <- bake(watersheds_prof, new_data = NULL)

watersheds_data <- watersheds_data %>% 
  mutate(
    pred = predict(ran_ensemble, watersheds_data)$mean
  )

ggplot(watersheds_data, aes(x = watersheds, y = pred)) +
  geom_point(alpha = .5, cex = 1)



## Repeated Ensembles ##



# explore the effect of thinning and sampling pseudoabsences on model performance
# create a list of simple_ensembles by looping through the SDM pipeline
# create an empty object to store the simple ensembles we will create:
ensemble_list <- list()
set.seed(123) # make sure seed is set outside of the loop

for (i_repeat in 1:3) {
  # thin the data
  ran_thin_rep <- thin_by_cell(ran_occ_sf, raster = predictors_multi)
  ran_thin_rep <- thin_by_dist(ran_thin_rep, dist_min = 15)
  # sample pseudo-absences
  ran_thin_rep <- sample_pseudoabs(ran_thin_rep,
                                   n = 3 * nrow(ran_thin_rep),
                                   raster = predictors_multi,
                                   method = c("dist_disc", 50, 90)
  )
  # get climate
  ran_thin_rep <- ran_thin_rep %>%
    bind_cols(terra::extract(predictors_multi, ran_thin_rep, ID = FALSE))
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
                                   predictors_multi_input, 
                                   fun = c("mean", "median")
                                   )

ggplot() +
  geom_spatraster(data = ran_thin_rep_ens, aes(fill = mean)) +
  scale_fill_terrain_c() +
  labs(title = "Ranunculus Present Sensitivity", subtitle = "Bioclim Model", xlab = "Longitude", ylab = "Latitude")
  

ran_thin_rep_ens
# convert to binary and calculate area?
