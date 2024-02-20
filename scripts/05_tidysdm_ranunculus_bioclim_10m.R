# Following tidysdm tutorial, we input Ranunculus glaberrimus occurrence records 
  # and WorldClim predictors into the tidysdm pipeline
# Please first run scripts in the following order: 
  # 01_data_download_ranunculus.R
  # 02_continental_divide.Rmd
  # 03_data_prep_ranunculus.R

# dir.create("outputs/")


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
na_bound_sf <- read_sf("data/raw/continental_divide_buffer_boundary.shp")
na_bound_vect <- vect(na_bound_sf)

# read in Ranunculus glaberrimus occurrences:
ran_occ_sf <- st_read(dsn = "data/processed/ran_occ_sf.shp")

# check which datasets are available through pastclim:
pastclim::get_available_datasets()
# can use "WorldClim_2.1_10m" or "WorldClim_2.1_5m"

# plot species occurrences directly on the raster used to extract climatic variables
# get land mask for available datasets, use that as background for occurrences
# download date: February 18th, 2024
worldclim <- pastclim::download_dataset(dataset = "WorldClim_2.1_10m", 
                                        bio_variables = NULL, 
                                        annual = FALSE, monthly = TRUE)

download_dataset("WorldClim_2.1_10m")
# TRUE under values in environment = successful download

# create land mask from pastclim
# timeframe is 1970 to 2000, listed as 1985, which is the midpoint
land_mask <- 
  pastclim::get_land_mask(time_ce = 1985, dataset = "WorldClim_2.1_10m")

# crop the extent of the land mask to match our study's extent
land_mask <- crop(land_mask, na_bound_vect)
# mask to the polygon
land_mask <- mask(land_mask, na_bound_vect)

# plot occurrences directly on raster with predictor variables

# use tidyterra package for plotting so ggplot can be used with terra rasters
# aes(fill = layer) refers to column name in na_bound_rast
ggplot()+
  geom_spatraster(data = land_mask, aes(fill = land_mask_1985)) +
  geom_sf(data = ran_occ_sf) # sf object with coordinates



### Thinning Occurrences ###



# thin the occurrences to have one per cell in the na_bound_rast raster
set.seed(1234567)
ran_occ_thin_cell <- thin_by_cell(ran_occ_sf, raster = na_bound_rast)
nrow(ran_occ_thin_cell) # 2462

ggplot() +
  geom_spatraster(data = land_mask, aes(fill = land_mask_1985)) +
  geom_sf(data = ran_occ_thin_cell) # thinned occurrences

# thin further to remove points closer than 5km
# default is metres, could input 5000 or use km2m(5)
# attempted 5km, filter_high_cor below wouldn't run, so try 10
# 10 still didn't work, try 15?
set.seed(1234567)
ran_occ_thin_dist <- thin_by_dist(ran_occ_sf, dist_min = km2m(10))
nrow(ran_occ_thin_dist) # 1400 at 5km thinning, 1046 at 10km thinning
# 1040 at 10km thinning with reduced spatial extent

ggplot() +
  geom_spatraster(data = land_mask, aes(fill = land_mask_1985)) +
  geom_sf(data = ran_occ_thin_dist) +



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
                                 raster = land_mask, 
                                 method = c("dist_disc", km2m(5), km2m(50))
)
nrow(ran_pres_abs) # 11 440

# plot presences and absences
ggplot() +
  geom_spatraster(data = land_mask, aes(fill = land_mask_1985)) +
  geom_sf(data = ran_pres_abs, aes(col = class))



### Variable Selection ###

# see which variables are available from WorldClim dataset
# 10 and 5m resolution available through pastclim
climate_vars <- pastclim::get_vars_for_dataset("WorldClim_2.1_10m")
climate_vars

# download dataset at the correct resolution (10 arcmin here, can do 5)
download_dataset("WorldClim_2.1_10m")
# dataset covers years 1970-2000, pastclim dates it at midpoint of 1985
# TRUE in console = successful download

# create a Spatraster for the WorldClim dataset
climate_present <- pastclim::region_slice(
  time_ce = 1985, 
  bio_variables = climate_vars, 
  data = "WorldClim_2.1_10m", 
  crop = na_bound # SpatVector with area boundary
)

# Extract variables from predictors_multirast for all presences and pseudoabsences
summary(climate_present) # 90 000 + NAs per column
nrow(ran_pres_abs) # 11 440
nrow(predictors_multi) # 4109
ran_pres_abs_pred <- ran_pres_abs %>% 
  bind_cols(terra::extract(climate_present, ran_pres_abs, ID = FALSE, na.rm = TRUE))
nrow(ran_pres_abs_pred) # 11 440
# after this step, no NA values in ran_occ_thin from tutorial
# but I have NA values from predictors_multi
summary(ran_pres_abs_pred) # still some NAs, bioclim script magically has none at this point

# remove rows with NA values
ran_pres_abs_pred <- na.omit(ran_pres_abs_pred)
nrow(ran_pres_abs_pred) # 11 307, 133 rows removed

# skipped non-overlapping distribution step in tutorial

# inspect the variables for collinearity
pairs(climate_present)

# may need a smaller sample to calculate collinearity between variables

# try sample size of 5000 cells
# predictors_sample <- terra::spatSample(climate_present, size = 5000, 
                                       #method = "random", replace = FALSE, 
                                       # na.rm = FALSE, as.raster = TRUE,
                                       # values = TRUE, cells = FALSE, xy = TRUE)


# subset to variables below 0.8 Pearson's correlation coefficient
# climate_present = SpatRaster with predictor data (all numeric, no NAs)

predictors_uncorr <- filter_high_cor(climate_present, cutoff = 0.8, 
                                     verbose = TRUE, names = TRUE, to_keep = NULL)
predictors_uncorr

# remove highly correlated predictors
# here is where the "class" column gets dropped, which messes up recipe below
# need to retain class column (not in original tutorial code)
ran_pres_abs_pred <- ran_pres_abs_pred %>% select(all_of(c(predictors_uncorr, "class")))

# now subset the uncorrelated predictors within climate_present
climate_present_uncorr <- climate_present[[predictors_uncorr]]



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



# use AUC as metric to choose best
  # random forest and boosted tree models
# when adding members to an ensemble, they are auto-fitted to the full
  # training dataset and therefore ready to make predictions
ran_ensemble <- simple_ensemble() %>% 
  add_member(ran_models, metric = "roc_auc")
# can also use roc_auc and tss_max as metrics
ran_ensemble
autoplot(ran_ensemble)
collect_metrics(ran_ensemble) # need to update tidysdm version for this to work
ran_ensemble_metrics <- collect_metrics(ran_ensemble)

## Projecting to the Present ##

# make predictions with the ensemble
prediction_present <- predict_raster(ran_ensemble, climate_present_uncorr)
ggplot() +
  geom_spatraster(data = prediction_present, aes(fill = mean)) +
  scale_fill_terrain_c() + # "c" for continuous variables
  # plot the presences used in the model
  geom_sf(data = ran_pres_abs_pred %>% filter(class == "presence"))

# subset the model to only use the best models, based on AUC
# set threshold of 0.8 for AUC
# take the median of the available model predictions (mean is the default)
prediction_present_best <- predict_raster(ran_ensemble, climate_present_uncorr, 
                                           metric_thresh = c("roc_auc", 0.8), 
                                           fun= "median")

ggplot() +
  geom_spatraster(data = prediction_present_best, aes(fill = median)) +
  scale_fill_terrain_c() + # c = continuous
  geom_sf(data = ran_pres_abs_pred %>% filter(class == "presence"))
# if plot doesn't change much, models are consistent
# model gives us probability of occurrence
# can convert to binary predictions (present vs absence)

ran_ensemble_binary <- calib_class_thresh(ran_ensemble, 
                                          class_thresh = "tss_max"
                                          )

prediction_present_binary <- predict_raster(ran_ensemble_binary, 
                                            climate_present_uncorr, 
                                            type = "class", 
                                            class_thresh = c("tss_max"))
prediction_present_binary

ggplot() +
  geom_spatraster(data = prediction_present_binary, aes(fill = binary_mean)) +
  geom_sf(data = ran_pres_abs_pred %>% filter(class == "presence"))

# turn presence into polygon so we can calculate suitable area
# first need to filter out presence cells from raster
prediction_present_presence <- prediction_present_binary %>% 
  filter(binary_mean == "presence")

# repeat for absences? May not be entirely useful information

# vectorize raster to get a polygon around presences
# need to turn raster into data.frame first
prediction_present_presence <- as.polygons(prediction_present_presence)

# now turn prediction_present_pres polygons into sf object
prediction_present_sf <- st_as_sf(prediction_present_presence)

crs(prediction_present_sf) # WGS84

# reproject CRS to BC Albers (equal area projection, EPSG:3005) for calculating area
prediction_present_area <- st_transform(prediction_present_sf, "EPSG:3005")
prediction_present_area <- st_set_crs(prediction_present_sf, "EPSG:3005")
prediction_present_area <- st_area(prediction_present_sf) # 1.48e+12 m^2
# convert from m^2 to km^2
prediction_present_area <- st_area(prediction_present_sf)/1000000
prediction_present_area <- units::set_units(st_area(prediction_present_sf), km^2) 
  # 1 478 904 km^2 of suitable habitat

# divide predicted present area by total study area to get proportion
proportion_suitable_present <- prediction_present_area/na_bound_area



#### Projecting to the Future ####



# full list of future projections from WorldClim:
help("WorldClim_2.1")
# ssp = Shared Socioeconomic Pathways, 126, 245, 370, 585 available

# SSP 245, 2081-2100
download_dataset("WorldClim_2.1_HadGEM3-GC31-LL_ssp245_10m")

# see which times are available:
get_time_ce_steps("WorldClim_2.1_HadGEM3-GC31-LL_ssp245_10m")
# predict using 2090 (midpoint between 2081 and 2100)

# check the available variables:
get_vars_for_dataset("WorldClim_2.1_HadGEM3-GC31-LL_ssp245_10m")
# all 19 bioclimatic variables, no altitude (because it doesn't change over time)
  # to use altitude, would have to copy it over from the present
  # but altitude not included in set of uncorrelated variables from earlier, 
    # so don't include here


# error in code chunk below: altitude not available for future predictions,
  # but included in vars_uncorr - need to remove altitude then later paste the 
  # values from climate_present into climate_future
# select uncorrelated variables
vars_uncorr_fut <- vars_uncorr[ !vars_uncorr == 'altitude']
vars_uncorr_fut

climate_future <- pastclim::region_slice(
  time_ce = 2090, 
  bio_variables = vars_uncorr_fut, # uncorrelated variables created previously
  # need to find out how to deal with
  data = "WorldClim_2.1_HadGEM3-GC31-LL_ssp245_10m", 
  crop = na_bound #boundary polygon for study area
)

# need to add altitude layer from climate_present
climate_future <- c(climate_future, climate_present$altitude)
# note: terra has an add function, but wasn't working for me

# predict using the ensemble:
prediction_future <- predict_raster(ran_ensemble, climate_future)

ggplot() +
  geom_spatraster(data = prediction_future, aes(fill = mean)) +
  scale_fill_terrain_c()


# convert predictions to binary (presence/absence)
# if plot doesn't change much, models are consistent
# model gives us probability of occurrence
# can convert to binary predictions (present vs absence)

ran_ensemble_binary <- calib_class_thresh(ran_ensemble, 
                                          class_thresh = "tss_max"
                                          )

prediction_future_binary <- predict_raster(ran_ensemble_binary, 
                                            climate_future, 
                                            type = "class", 
                                            class_thresh = c("tss_max"))
prediction_future_binary

ggplot() +
  geom_spatraster(data = prediction_future_binary, aes(fill = binary_mean)) +
  geom_sf(data = ran_pres_abs_pred %>% filter(class == "presence"))

# turn presence into polygon so we can calculate suitable area
# first need to filter out presence cells from raster
prediction_future_pres <- prediction_future_binary %>% 
  filter(binary_mean == "presence")

# repeat for absences? May not be entirely useful information

# vectorize raster to get a polygon around presences
# need to turn raster into data.frame first
prediction_future_pres <- as.polygons(prediction_future_pres)

# now turn prediction_present_pres polygons into sf object
prediction_future_sf <- st_as_sf(prediction_future_pres)

crs(prediction_future_sf) # WGS84

# reproject CRS to BC Albers (equal area projection, EPSG:3005) for calculating area
prediction_future_area <- st_transform(prediction_future_sf, "EPSG:3005")
prediction_future_area <- st_set_crs(prediction_future_sf, "EPSG:3005")
prediction_future_area <- st_area(prediction_future_sf) # 1.15e+12 m^2
# convert from m^2 to km^2
prediction_future_area <- st_area(prediction_future_sf)/1000000
prediction_future_area <- units::set_units(st_area(prediction_future_sf), km^2) 
# 1 150 023 km^2 of suitable habitat

# divide predicted present area by total study area to get proportion
proportion_suitable_future <- prediction_future_area/na_bound_area

# now calculate difference between suitable habitat in the present and 2081-2100
# first need to convert area from class "units" to numeric
prediction_present_area_num <- as.numeric(prediction_present_area)
prediction_future_area_num <- as.numeric(prediction_future_area)
change_area_present_to_2100 <- prediction_future_area_num - prediction_present_area_num
  # -328 881.059 km^2 change in suitable habitat

# proportion changed:
proportion_change <- proportion_suitable_future/proportion_suitable_present



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
# bio 07, 09, 05, 19, 02, 18, 14, 08, 15, altitude
# there must be a way to loop through this?

# investigate the contribution of bio07:
bio07_prof <- ran_recipe %>%  # recipe from above
  step_profile(-bio07, profile = vars(bio07)) %>% 
  prep(training = ran_occ_thin)

bio07_data <- bake(bio07_prof, new_data = NULL)

bio07_data <- bio07_data %>% 
  mutate(
    pred = predict(ran_ensemble, bio07_data)$mean
  )

ggplot(bio07_data, aes(x = bio07, y = pred)) +
  geom_point(alpha = .5, cex = 1)

# investigate the contribution of bio09:
bio09_prof <- ran_recipe %>%  # recipe from above
  step_profile(-bio09, profile = vars(bio09)) %>% 
  prep(training = ran_occ_thin)

bio09_data <- bake(bio09_prof, new_data = NULL)

bio09_data <- bio09_data %>% 
  mutate(
    pred = predict(ran_ensemble, bio09_data)$mean
  )

ggplot(bio09_data, aes(x = bio09, y = pred)) +
  geom_point(alpha = .5, cex = 1)

# investigate the contribution of bio05:
bio05_prof <- ran_recipe %>%  # recipe from above
  step_profile(-bio05, profile = vars(bio05)) %>% 
  prep(training = ran_occ_thin)

bio05_data <- bake(bio05_prof, new_data = NULL)

bio05_data <- bio05_data %>% 
  mutate(
    pred = predict(ran_ensemble, bio05_data)$mean
  )

ggplot(bio05_data, aes(x = bio05, y = pred)) +
  geom_point(alpha = .5, cex = 1)

# investigate the contribution of bio19:
bio19_prof <- ran_recipe %>%  # recipe from above
  step_profile(-bio19, profile = vars(bio19)) %>% 
  prep(training = ran_occ_thin)

bio19_data <- bake(bio19_prof, new_data = NULL)

bio19_data <- bio19_data %>% 
  mutate(
    pred = predict(ran_ensemble, bio19_data)$mean
  )

ggplot(bio19_data, aes(x = bio19, y = pred)) +
  geom_point(alpha = .5, cex = 1)

# investigate the contribution of bio02:
bio02_prof <- ran_recipe %>%  # recipe from above
  step_profile(-bio02, profile = vars(bio02)) %>% 
  prep(training = ran_occ_thin)

bio02_data <- bake(bio02_prof, new_data = NULL)

bio02_data <- bio02_data %>% 
  mutate(
    pred = predict(ran_ensemble, bio02_data)$mean
  )

ggplot(bio02_data, aes(x = bio02, y = pred)) +
  geom_point(alpha = .5, cex = 1)

# investigate the contribution of bio18:
bio18_prof <- ran_recipe %>%  # recipe from above
  step_profile(-bio05, profile = vars(bio18)) %>% 
  prep(training = ran_occ_thin)

bio18_data <- bake(bio18_prof, new_data = NULL)

bio18_data <- bio18_data %>% 
  mutate(
    pred = predict(ran_ensemble, bio18_data)$mean
  )

ggplot(bio18_data, aes(x = bio18, y = pred)) +
  geom_point(alpha = .5, cex = 1)

# investigate the contribution of bio14:
bio14_prof <- ran_recipe %>%  # recipe from above
  step_profile(-bio14, profile = vars(bio14)) %>% 
  prep(training = ran_occ_thin)

bio14_data <- bake(bio14_prof, new_data = NULL)

bio14_data <- bio14_data %>% 
  mutate(
    pred = predict(ran_ensemble, bio14_data)$mean
  )

ggplot(bio14_data, aes(x = bio14, y = pred)) +
  geom_point(alpha = .5, cex = 1)

# investigate the contribution of bio08:
bio08_prof <- ran_recipe %>%  # recipe from above
  step_profile(-bio08, profile = vars(bio08)) %>% 
  prep(training = ran_occ_thin)

bio08_data <- bake(bio08_prof, new_data = NULL)

bio08_data <- bio08_data %>% 
  mutate(
    pred = predict(ran_ensemble, bio08_data)$mean
  )

ggplot(bio08_data, aes(x = bio08, y = pred)) +
  geom_point(alpha = .5, cex = 1)

# investigate the contribution of bio15:
bio15_prof <- ran_recipe %>%  # recipe from above
  step_profile(-bio15, profile = vars(bio15)) %>% 
  prep(training = ran_occ_thin)

bio15_data <- bake(bio15_prof, new_data = NULL)

bio15_data <- bio15_data %>% 
  mutate(
    pred = predict(ran_ensemble, bio15_data)$mean
  )

ggplot(bio15_data, aes(x = bio15, y = pred)) +
  geom_point(alpha = .5, cex = 1)

# investigate the contribution of altitude:
altitude_prof <- ran_recipe %>%  # recipe from above
  step_profile(-altitude, profile = vars(altitude)) %>% 
  prep(training = ran_occ_thin)

altitude_data <- bake(altitude_prof, new_data = NULL)

altitude_data <- altitude_data %>% 
  mutate(
    pred = predict(ran_ensemble, altitude_data)$mean
  )

ggplot(altitude_data, aes(x = altitude, y = pred)) +
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
  scale_fill_terrain_c()
# convert to binary and calculate area?
