# tidysdm tutorial

# dir.create("outputs/")

# attempting multiraster with categorical rasters converted to numeric (line 104)

library(tidysdm)
library(tidyterra)
library(sf)
library(pastclim)
library(ggplot2)
library(overlapping)

# North American extent (west coast to continental divide)
# new geographic extent created in continental_divide.Rmd
# read in na_bound_rast
na_bound_rast <- rast("data/processed/na_bound_rast.tif")

# read in multilayer raster with predictor data, created in
  # 03_data_prep_ranunculus
predictors_multi <- rast("data/processed/predictors_multi.tif")

# read in Ranunculus glaberrimus presence dataframe, 
# dataframe with ID, latitude, and longitude columns
ran_occ_download # tibble/dataframe

# plot the presences on a map to visualize them
# cast coordinates into an sf object and set its CRS to WGS84
ran_occ_sf <- st_as_sf(ran_occ_download, coords = c("decimalLongitude", "decimalLatitude"))
# set CRS to WGS84
st_crs(ran_occ_sf) <- 4326

# lines 26-30 already done in data prep script
# read in multiraster with predictors cropped and masked to the extent of interest
# predictors_multirast <- crop(predictors_multirast, na_bound)

# mask the multiraster to the extent (all values outside na_bound set to NA)
# predictors_multirast <- mask(predictors_multirast, na_bound)

# plot occurrences directly on raster with predictor variables

# use tidyterra package for plotting so ggplot can be used with terra rasters
# aes(fill = layer) refers to column name in na_bound_rast
ggplot()+
  geom_spatraster(data = na_bound_rast, aes(fill = layer)) +
  geom_sf(data = ran_occ_sf) # sf object with coordinates



### Thinning Occurrences ###



# thin the occurrences to have one per cell in the na_bound_rast raster
set.seed(1234567)
ran_occ <- thin_by_cell(ran_occ_sf, raster = na_bound_rast)
nrow(ran_occ)

ggplot() +
  geom_spatraster(data = na_bound_rast, aes(fill = layer)) +
  geom_sf(data = ran_occ) # thinned occurrences

# thin further to remove points closer than 5km
# default is metres, could input 5000 or use km2m(5)
set.seed(1234567)
ran_occ_th <- thin_by_dist(ran_occ, dist_min = km2m(5))
nrow(ran_occ_th)

# test out how many occurrences when thinning to 20km
# set.seed(1234567)
# ran_occ_thin_20 <- thin_by_dist(ran_occ, dist_min = km2m(20))
# nrow(ran_occ_thin_20) # only 658 occurrences left

ggplot() +
  geom_spatraster(data = na_bound_rast, aes(fill = layer)) +
  geom_sf(data = ran_occ_th)



### Pseudoabsences ###



# sample pseudoabsences/background points
# constrain pseudoabsences to be at least 50km away from any presences
# select 10 times as many pseudoabsences as presences 
  # (recommended 10 000 pseudoabsences by lit review)
# ran_occ_thin will then have presences and pseudoabsences
set.seed(1234567)
ran_occ_th <- sample_pseudoabs(ran_occ_th, 
                               n = 10 * nrow(ran_occ_th), 
                               raster = na_bound_rast, 
                               method = c("dist_min", km2m(50))
                               )

# plot presences and absences
ggplot() +
  geom_spatraster(data = na_bound_rast, aes(fill = layer)) +
  geom_sf(data = ran_occ_th, aes(col = class))



### Variable Selection ###



# select variables for which presences are markedly different from the 
  # underlying background
# First, extract variables from predictors_multirast 
  # for all presences and pseudoabsences
ran_occ_th <- ran_occ_th %>% 
  bind_cols(terra::extract(predictors_multi, ran_occ_th, ID = FALSE))
# above code assigns new names for some reason and appears to have many more layers in some runs
names(ran_occ_th)
summary(ran_occ_th)

# ran_occ_th contains NA values (counterpart in tutorial does not)
  # attempt to remove rows with NA values:
ran_occ_th <- na.omit(ran_occ_th)
summary(ran_occ_th)

# use violin plots to compare the distribution of the variables for presences
  # and pseudoabsences
ran_occ_th %>% plot_pres_vs_bg(class)


# want to select variables for which presences use values different from 
  # the background/pseudoabsences
# rank the variables based on the overlap of the respective density plots
ran_occ_th %>% dist_pres_vs_bg(class)

# focus on variables with at least 30% of non-overlapping distribution between 
  # presences and pseudoabsences
  # below code messes with correlation filtering step
# vars_to_keep <- ran_occ_th %>% dist_pres_vs_bg(class)
# vars_to_keep <- names(vars_to_keep[vars_to_keep > 0.3])
# ran_occ_th <- ran_occ_th %>% select(all_of(c(vars_to_keep, "class")))
# vars_to_keep

# tutorial has a list of variables suggested by the literature to be important
  # in determining the distribution of the species
# below code isn't working in the commands following
# suggested_vars <- c("anth_biome", "Climate", "elevation_na", "lndcvr_na", 
                   #  "soil_phh2o_0_5", "soilphh2o_5_15", 
                   #  "soil_temp_0_5", "soil_temp_5_15")

suggested_vars <- names(predictors_multi) # this actually worked with pairs command

# inspect the variables for collinearity
pairs(predictors_multi[[suggested_vars]])

# need a smaller sample to calculate collinearity between variables
nrow(predictors_multi) # 8024 rows 
# try sample size of 5000 cells
predictors_sample <- terra::spatSample(predictors_multi, size = 5000, 
                                       method = "random", replace = FALSE, 
                                       na.rm = FALSE, as.raster = TRUE,
                                       values = TRUE, cells = FALSE, xy = TRUE)


# subset to variables below 0.8 Pearson's correlation coefficient
# start with 0.7 first (as done in tutorial)
# predictors_multi = SpatRaster with predictor data (all numeric, no NAs)
predictors_sample <- predictors_sample[[suggested_vars]]
# below code was taking forever to run, but no delays in the bioclim code
predictors_uncorr <- filter_high_cor(predictors_sample, cutoff = 0.7, 
                                     verbose = FALSE, names = TRUE, to_keep = NULL)
predictors_uncorr

# remove highly correlated predictors
# here is where the "class" column gets dropped, which messes up recipe below
  # need to retain class column (not in original tutorial code)
ran_occ_th <- ran_occ_th %>% select(all_of(c(predictors_uncorr, "class")))
predictors_multi <- predictors_multi[[predictors_uncorr]]
predictors_multi


#### Fit the model by cross-validation ####



# use a recipe to define how to handle our dataset
# need to define the formula (class is the outcome, all other variables are predictors)
# for sf objects, geometry is auto-replaced by X and Y and assigned as coords, therefore not used as predictors
ran_occ_recipe <- recipe(ran_occ_th, formula = class ~ .)
ran_occ_recipe

# tidymodels assumes the level of interest for the response (presences) is the reference level
# confirm the data are correctly formatted
ran_occ_th %>% check_sdm_presence(class)

# build a workflow_set of different models, defining which hyperparameters we want to tune
# for most commonly used models, tidysdm auto chooses the most important parameters
ran_occ_models <- 
  workflow_set(
    preproc = list(default = ran_occ_recipe), 
    models = list(
      glm = sdm_spec_glm(), # standard GLM specs
      rf = sdm_spec_rf(), # rf specs with tuning
      gbm = sdm_spec_boost_tree(), # boosted tree specs with tuning
      maxent = sdm_spec_maxent() # maxent specs with tuning
    ), 
    # make all combos of proporc and models:
    cross = TRUE
    ) %>% 
  # tweak controls to store information needed later to create the ensemble
  option_add(control = control_ensemble_grid())

# set up spatial block cross-validation to tune and assess models:
# 80:20 split with 5 folds (v = 5) (supported by literature review)
set.seed(100)
ran_occ_cv <- spatial_block_cv(ran_occ_th, v = 5)
autoplot(ran_occ_cv)

# use block CV folds to tune and assess models
  # tutorial uses 3 combos of hyperparameters and says this is far too few for real life
  # 10 combos of hyperparameters = ~ 3 mins computation time, less crowded plots
  # 20 combos of hyperparameters = ~ 3 mins computation time, crowded plots
# 10 combos = 25 minute computation time on February 14th
set.seed(1234567)
ran_occ_models <- 
  ran_occ_models %>% 
  workflow_map("tune_grid", 
               resamples = ran_occ_cv, grid = 10, # attempting 10 combos of hyperparameters
               metrics = sdm_metric_set(), verbose = TRUE
               ) 

# want workflow_set to correctly detect no tuning parameters for GLM
# inspect performance of models:
autoplot(ran_occ_models)



#### Ensemble ####



# select the best set of parameters for each model
# algorithms in the ensemble are auto-fitted to the full training dataset
  # therefore ready to make predictions
ran_ensemble <- simple_ensemble() %>% 
  add_member(ran_occ_models, metric = "roc_auc") # or boyce_cont or tss_max
ran_ensemble
autoplot(ran_ensemble)

# a tabular form of the model metrics:
ran_ensemble %>% collect_metrics() # error said no collect_metric() exists for that type of object



#### Projecting to the Present ####



# predictions using the ensemble
# default is taking the mean of the predictions from each model
# line below uses over 10GB of RAM
prediction_present <- predict_raster(ran_ensemble, predictors_multi)

ggplot() +
  geom_spatraster(data = prediction_present, aes (fill = mean)) +
  scale_fill_terrain_c() + # c for continuous
  geom_sf(data = ran_occ_th %>% filter(class == "presence"))

# subset the ensemble to only use the best models (AUC > 0.8)
# take the median of the available model predictions (default is the mean)
prediction_present_AUC <- predict_raster(ran_ensemble, ran_occ_th, 
                                         metric_thresh = c("roc_auc", 0.8), 
                                         fun = "median"
                                         )

ggplot() +
  geom_spatraster(data = prediction_present_AUC, aes(fill = median)) +
  scale_fill_terrain_c() +
  geom_sf(data = ran_occ_th %>% filter(class == "presence"))

# desirable to have binary predictions (presence/absence) rather than probability of occurrence
  # calibrate threshold used to convert probabilities into classes
ran_ensemble <- calib_class_threshold(ran_ensemble, 
                                      class_thresh = "roc_auc"
                                      )

prediction_present_binary <- predict_raster(ran_ensemble, 
                                            predictors_multi, 
                                            type = "class", 
                                            class_thresh = c("roc_auc")
                                            )

ggplot() +
  geom_spatraster(data = prediction_present_binary, aes(fill = binary_mean)) +
  geom_sf(data= ran_occ_th %>% filter(class == "presence"))



#### Projecting to the Future ####



download_dataset("WorldClim_2.1_HadGem3-GC31-LL_ssp245_10m")