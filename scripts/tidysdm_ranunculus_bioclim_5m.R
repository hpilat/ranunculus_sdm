# Following tidysdm tutorial, we input Ranunculus glaberrimus occurrence records 
# and WorldClim predictors into the tidysdm pipeline
# Please first run scripts in the following order: 
# 01_data_download_ranunculus.R
# 02_continental_divide.Rmd
# 03_data_prep_ranunculus.R

# this particular script is run with 5m WorldClim data instead of 10m

# dir.create("outputs/")

library(tidysdm)
library(tidyterra)
library(sf)
library(pastclim)
library(ggplot2)
library(overlapping)
library(ranger)
library(xgboost)

# North American extent (west coast to continental divide)
# new geographic extent created in continental_divide.Rmd
# read in na_bound_rast
na_bound_rast <- rast("data/processed/na_bound_rast.tif")

na_bound_sf <- read_sf("data/raw/continental_divide_buffer_boundary.shp")
na_bound <- vect(na_bound_sf)

# read in Ranunculus glaberrimus presence dataframe, 
# dataframe with ID, latitude, and longitude columns
ran_occ_download # tibble/dataframe

# plot the presences on a map to visualize them
# cast coordinates into an sf object and set its CRS to WGS84
ran_occ_sf <- st_as_sf(ran_occ_download, coords = c("decimalLongitude", "decimalLatitude"))
# set CRS to WGS84
st_crs(ran_occ_sf) <- 4326

# check which datasets are available through pastclim:
pastclim::get_available_datasets()
# can use "WorldClim_2.1_10m" or "WorldClim_2.1_5m"

# plot species occurrences directly on the raster used to extract climatic variables
# get land mask for available datasets, use that as background for occurrences
# download date: February 18th, 2024
worldclim <- pastclim::download_dataset(dataset = "WorldClim_2.1_5m", 
                                        bio_variables = NULL, 
                                        annual = FALSE, monthly = TRUE)

download_dataset("WorldClim_2.1_5m")
# TRUE under values in environment = successful download

# create land mask from pastclim
# timeframe is 1970 to 2000, listed as 1985, which is the midpoint
land_mask <- 
  pastclim::get_land_mask(time_ce = 1985, dataset = "WorldClim_2.1_5m")

# crop the extent of the land mask to match our study's extent
land_mask <- crop(land_mask, na_bound)
# mask to the polygon
land_mask <- mask(land_mask, na_bound)

# use tidyterra package for plotting so ggplot can be used with terra rasters
ggplot()+
  geom_spatraster(data = land_mask, aes(fill = land_mask_1985)) +
  geom_sf(data = ran_occ_sf) # sf object with coordinates


## Thinning Occurrences ##

# thin the observations to have one per cell in the raster
# works better when using an equal area projection
set.seed(1234567)
ran_occ_sf <- thin_by_cell(ran_occ_sf, raster = land_mask)
nrow(ran_occ_sf)
# thinned from 3768 observations to 1079

# plot the thinned occurrences
ggplot() +
  geom_spatraster(data = land_mask, aes(fill = land_mask_1985)) +
  geom_sf(data = ran_occ_sf)

# thin occurrences further to remove points closer than 5km
# distance given in metres, use km2m to reduce number of 0s written for 20km
set.seed(1234567)
ran_occ_thin <- thin_by_dist(ran_occ_sf, dist_min = km2m(10))
nrow(ran_occ_thin)

# plot the thinned occurrences again
ggplot() +
  geom_spatraster(data = land_mask, aes(fill = land_mask_1985)) +
  geom_sf(data = ran_occ_thin)


## Sample Pseudoabsences ##

# constrain pseudoabsences to be minimum 50km away from presence points
# add pseudoabsences to object with thinned occurrences
# select 3 times as many pseudoabsences as presences 
# Barbet-Massin (2012) recommend a large number of pseudoabsences, 
# like 10 000, to adequately represent the different environmental conditions

set.seed(1234567)
ran_occ_thin <- sample_pseudoabs(ran_occ_thin, 
                                 n = 10 * nrow(ran_occ_thin), 
                                 raster = land_mask, 
                                 coords = NULL, 
                                 method = c("dist_disc", km2m(5), km2m(15))
                                 )
# 'dist_disc': pseudo-absences/background randomly sampled from the unioned 
  # discs around presences with the two values of 'dist_disc' defining the 
  # minimum and maximum distance from presences.
  # 5 and 15km disc around presences was arbitrarily chosen

# now plot the presences and absences
ggplot() +
  geom_spatraster(data = land_mask, aes(fill = land_mask_1985)) +
  geom_sf(data = ran_occ_thin, aes(col = class))


### Variable Selection ###


# see which variables are available from WorldClim dataset
# 10 and 5m resolution available through pastclim
climate_vars <- pastclim::get_vars_for_dataset("WorldClim_2.1_5m")
climate_vars

# download dataset at the correct resolution (5 arcmin here, can do 10)
download_dataset("WorldClim_2.1_5m")
# dataset covers years 1970-2000, pastclim dates it at midpoint of 1985
# TRUE in console = successful download

# create a Spatraster for the WorldClim dataset
climate_present <- pastclim::region_slice(
  time_ce = 1985, 
  bio_variables = climate_vars, 
  data = "WorldClim_2.1_5m", 
  crop = na_bound # SpatVector with area boundary
)

nrow(ran_occ_thin) # 14201
summary(climate_present) # lots of NA values
nrow(climate_present) # 802, matches land_mask

# select variables noticeably different from the underlying background
# first extract climate for all presences and pseudoabsences
ran_occ_thin <- ran_occ_thin %>% 
  bind_cols(terra::extract(climate_present, ran_occ_thin, ID = FALSE))
summary(ran_occ_thin) # no NA values
nrow(ran_occ_thin) # 11330, no reduction in # of rows

# use violin plots to compare the distribution of climate variables for
# presences and pseudoabsences
ran_occ_thin %>% plot_pres_vs_bg(class)

# want to select variables where presences have different values from background
# rank variables based on the overlap of the respective density plots
library(overlapping)
ran_occ_thin %>% dist_pres_vs_bg(class)

# select variables with at least 30% of non-overlapping distribution between
# presences and pseudoabsences
# vars_to_keep <- ran_occ_thin %>% dist_pres_vs_bg(class)
# vars_to_keep <- names(vars_to_keep[vars_to_keep > 0.30])
# ran_occ_thin <- ran_occ_thin %>% select(all_of(c(vars_to_keep, "class")))
# vars_to_keep

# selected certain variables based on literature:
# suggested_vars <- c("bio01", "bio05", "bio06", "bio07", "bio13", "bio14")

# inspect the correlation among variables:
# pairs(climate_present[[suggested_vars]])

# subset to variables below a correlation threshold of 0.7
# climate_present <- climate_present[[suggested_vars]]
# vars_uncor <- filter_high_cor(climate_present, cutoff = 0.7)
# vars_uncor

# only left with 3 variables here, attempt to threshold all variables

suggested_vars <- names(climate_present) # this actually worked with pairs command

# inspect the variables for collinearity
# pairs(climate_present[[suggested_vars]])

# subset to variables below 0.8 Pearson's correlation coefficient
# start with 0.7 first (as done in tutorial)
# predictors_multi = SpatRaster with predictor data (all numeric, no NAs)
climate_present <- climate_present[[suggested_vars]]

# below code was taking forever to run, but no delays in the bioclim code
vars_uncorr <- filter_high_cor(climate_present, cutoff = 0.8, 
                               verbose = FALSE, names = TRUE, to_keep = NULL)
vars_uncorr
# left with 10 predictors for 0.8 threshold (Feb. 18th run)

# select uncorrelated variables
ran_occ_thin <- ran_occ_thin %>% select(all_of(c(vars_uncorr, "class")))
climate_present <- climate_present[[vars_uncorr]]


## Model Fitting ##

# fit the model by cross-validation
# need to set up a recipe to define how to handle our dataset
ran_recipe <- recipe(ran_occ_thin, formula = class ~ .)
ran_recipe
# class = outcome, predictors = 3 chosen, coords= x and y
# assumption is that the level of interest for the response (presences) is
# the reference level
# confirm the data are correctly formatted:
ran_occ_thin %>% check_sdm_presence(class)
# output says TRUE

# now build a workflow_set of different models
# used glm, random forest, boosted trees, and maxent here
# tidysdm automatically chooses the most important model parameters
# model parameters come from the tidymodels parsnip package
# can add GAM (gen_additive_mod) but have to select parameters 
  # (otherwise models fail lines 187-198)
# also try adding boost_tree, mars
ran_models <- 
  workflow_set(
    preproc = list(default = ran_recipe), 
    models = list(
      glm = sdm_spec_glm(), # standard glm specs
      rf = sdm_spec_rf(), # rf specs with tuning
      gbm = sdm_spec_boost_tree(), # boosted tree model specs with tuning
      maxent = sdm_spec_maxent() # maxent specs with tuning
    ), 
    cross= TRUE # make all combinations of preproc and models
  ) %>% 
  option_add(control = control_ensemble_grid()) 
# ^ tweak controls to store info needed later to create the ensemble

# set up a spatial block cross-validation scheme to tune and assess models
# 80:20 split
# insert watersheds layer here when conducting sensitivity analysis?
set.seed(100)
ran_cross_val <- spatialsample::spatial_block_cv(ran_occ_thin, v = 5)
autoplot(ran_cross_val)

# use block cross validation folds to tune and assess the models
# used 3 combinations of hyperparameters per model (far too few for reality)
# need to change grid = 3 to a higher number
library(ranger)
library(xgboost)
set.seed(1234567)
ran_models <- 
  ran_models %>% 
  workflow_map("tune_grid", 
               resamples = ran_cross_val, grid = 10, # grid needs to increase
               metrics = sdm_metric_set(), verbose = TRUE)
# warning messages: no event observations were detected in 'truth with 
# event level 'presence'
# workflow_set should correctly detect no tuning parameters for GLM

# look at the performance of the models:
autoplot(ran_models)
model_metrics <- collect_metrics(ran_models)
# metrics are Boyce continuous index, TSS max, and ROC AUC


## Ensemble ##

# use AUC as metric to choose best
# random forest and boosted tree models
# when adding members to an ensemble, they are auto-fitted to the full
# training dataset and therefore ready to make predictions
ran_ensemble <- simple_ensemble() %>% 
  add_member(ran_models, metric = "roc_auc")
# can also use boyce_cont and tss_max as metrics
ran_ensemble
autoplot(ran_ensemble)
ran_ensemble_metrics <- collect_metrics(ran_ensemble)

## Projecting to the Present ##

# make predictions with the ensemble
prediction_present <- predict_raster(ran_ensemble, climate_present)
ggplot() +
  geom_spatraster(data = prediction_present, aes(fill = mean)) +
  scale_fill_terrain_c() + # "c" for continuous variables
  # plot the presences used in the model
  geom_sf(data = ran_occ_thin %>% filter(class == "presence"))

# subset the model to only use the best models, based on AUC
# set threshold of 0.8 for AUC
# take the median of the available model predictions (mean is the default)
prediction_present_best_bioclim_5 <- predict_raster(ran_ensemble, climate_present, 
                                                  metric_thresh = c("roc_auc", 0.8), 
                                                  fun= "median")

ggplot() +
  geom_spatraster(data = prediction_present_best_bioclim_5, aes(fill = median)) +
  scale_fill_terrain_c() + # c = continuous
  geom_sf(data = ran_occ_thin %>% filter(class == "presence"))
# if plot doesn't change much, models are consistent
# model gives us probability of occurrence
# can convert to binary predictions (present vs absence)

ran_ensemble_binary <- calib_class_thresh(ran_ensemble, 
                                          class_thresh = "tss_max"
)

prediction_present_binary <- predict_raster(ran_ensemble_binary, 
                                            climate_present, 
                                            type = "class", 
                                            class_thresh = c("tss_max"))
prediction_present_binary

ggplot() +
  geom_spatraster(data = prediction_present_binary, aes(fill = binary_mean)) +
  geom_sf(data = ran_occ_thin %>% filter(class == "presence"))

# turn presence into polygon so we can calculate suitable area
# first need to filter out presence cells from raster
prediction_present_pres <- prediction_present_binary %>% 
  filter(binary_mean == "presence")

# repeat for absences? May not be entirely useful information

# vectorize raster to get a polygon around presences
# need to turn raster into data.frame first
prediction_present_pres <- as.polygons(prediction_present_pres)

# now turn prediction_present_pres polygons into sf object
prediction_present_sf <- st_as_sf(prediction_present_pres)

crs(prediction_present_sf) # WGS84

# reproject CRS to BC Albers (equal area projection, EPSG:3005) for calculating area
prediction_present_area <- st_transform(prediction_present_sf, "EPSG:3005")
prediction_present_area <- st_set_crs(prediction_present_sf, "EPSG:3005")
prediction_present_area <- st_area(prediction_present_sf) # 1.24e+12 m^2
# convert from m^2 to km^2
prediction_present_area <- st_area(prediction_present_sf)/1000000
prediction_present_area <- units::set_units(st_area(prediction_present_sf), km^2) 
# 1 241 631 km^2 of suitable habitat

# divide predicted present area by total study area to get proportion
proportion_suitable_present <- prediction_present_area/na_bound_area
# 



#### Projecting to the Future ####



# full list of future projections from WorldClim:
help("WorldClim_2.1")
# ssp = Shared Socioeconomic Pathways, 126, 245, 370, 585 available

# SSP 245, 2081-2100
download_dataset("WorldClim_2.1_HadGEM3-GC31-LL_ssp245_5m")

# see which times are available:
get_time_ce_steps("WorldClim_2.1_HadGEM3-GC31-LL_ssp245_5m")
# predict using 2090 (midpoint between 2081 and 2100)

# check the available variables:
get_vars_for_dataset("WorldClim_2.1_HadGEM3-GC31-LL_ssp245_5m")
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
  data = "WorldClim_2.1_HadGEM3-GC31-LL_ssp245_5m", 
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
# change threshold to an number? (0.5?)
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
  geom_sf(data = ran_occ_thin %>% filter(class == "presence"))

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
prediction_future_area <- st_area(prediction_future_sf) # 6.3e+11 m^2
# convert from m^2 to km^2
prediction_future_area <- st_area(prediction_future_sf)/1000000
prediction_future_area <- units::set_units(st_area(prediction_future_sf), km^2) 
# 629 690 km^2 of suitable habitat

# divide predicted present area by total study area to get proportion
proportion_suitable_future <- prediction_future_area/na_bound_area
# 6.85% of study area

# now calculate difference between suitable habitat in the present and 2081-2100
# first need to convert area from class "units" to numeric
prediction_present_area_num <- as.numeric(prediction_present_area)
prediction_future_area_num <- as.numeric(prediction_future_area)
change_area_present_to_2100 <- prediction_future_area_num - prediction_present_area_num
# -611 940 km^2 change in suitable habitat

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
  ran_thin_rep <- thin_by_cell(ran_occ_sf, raster = climate_present)
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
