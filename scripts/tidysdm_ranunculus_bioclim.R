# tidysdm tutorial

# dir.create("outputs/")

library(tidysdm)
library(tidyterra)
library(sf)
library(pastclim)
library(ggplot2)
library(overlapping)

# read in Ranunculus glaberrimus presence dataframe, 
  # dataframe with ID, latitude, and longitude columns
ran_occ_download # tibble/dataframe
ran_occ # spatvector

# plot the presences on a map to visualize them
# cast coordinates into an sf object and set its CRS to WGS84
ran_occ_sf <- st_as_sf(ran_occ_download, coords = c("decimalLongitude", "decimalLatitude"))
# set CRS to WGS84
st_crs(ran_occ_sf) <- 4326

# plot species occurrences directly on the raster used to extract climatic variables
# get land mask for available datasets, use that as background for occurrences
worldclim <- pastclim::download_dataset(dataset = "WorldClim_2.1_10m", 
                                        bio_variables = NULL, 
                                        annual = TRUE, monthly = FALSE) 
                                        # ^ alter this for monthly variables
# TRUE under values in environment = successful download

# create land mask from pastclim
# timeframe is 1970 to 2000, listed as 1985, which is the midpoint
land_mask <- 
  pastclim::get_land_mask(time_ce = 1985, dataset = "WorldClim_2.1_10m")

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
ran_occ_thin <- thin_by_dist(ran_occ_sf, dist_min = km2m(5))
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
                                 n = 5 * nrow(ran_occ_thin), 
                                 raster = land_mask, 
                                 coords = NULL, 
                                 method = c("dist_min", km2m(50))
                                 )

# now plot the presences and absences
ggplot() +
  geom_spatraster(data = land_mask, aes(fill = land_mask_1985)) +
  geom_sf(data = ran_occ_thin, aes(col = class))

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

# select variables noticeably different from the underlying background
# first extract climate for all presences and pseudoabsences
ran_occ_thin <- ran_occ_thin %>% 
  bind_cols(terra::extract(climate_present, ran_occ_thin, ID = FALSE))

# use violin plots to compare the distribution of climate variables for
  # presences and pseudoabsences
ran_occ_thin %>% plot_pres_vs_bg(class)

# want to select variables where presences have different values from background
  # rank variables based on the overlap of the respective density plots
library(overlapping)
ran_occ_thin %>% dist_pres_vs_bg(class)

# select variables with at least 30% of non-overlapping distribution between
  # presences and pseudoabsences
vars_to_keep <- ran_occ_thin %>% dist_pres_vs_bg(class)
vars_to_keep <- names(vars_to_keep[vars_to_keep > 0.30])
ran_occ_thin <- ran_occ_thin %>% select(all_of(c(vars_to_keep, "class")))
vars_to_keep

# selected certain variables based on literature:
suggested_vars <- c("bio01", "bio05", "bio06", "bio07", "bio13", "bio14")

# inspect the correlation among variables:
pairs(climate_present[[suggested_vars]])

# subset to variables below a correlation threshold of 0.7
climate_present <- climate_present[[suggested_vars]]
vars_uncor <- filter_high_cor(climate_present, cutoff = 0.7)
vars_uncor
# only left with 3 variables here

# try thresholding all 19 variables
# vars_to_keep <- c("bio01", "bio02", "bio03", "bio04", "bio05", "bio06", "bio07", 
                  "bio08", "bio09", "bio10", "bio11", "bio12", "bio13", "bio14", 
                  "bio15", "bio16", "bio17", "bio18", "bio19", "altitude")

# climate_present <- climate_present[[vars_to_keep]]
# vars_uncor_all <- filter_high_cor(climate_present, cutoff = 0.7)
# vars_uncor_all

# select uncorrelated variables
ran_occ_thin <- ran_occ_thin %>% select(all_of(c(vars_uncor, "class")))
climate_present <- climate_present[[vars_uncor]]


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
# can add GAM but have to select parameters (otherwise models fail lines 187-198)
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
# metrics are Boyce continuous index, TSS max, and ROC AUC


## Ensemble ##

# use Boyce-continuous index as metric to choose best
  # random forest and boosted tree models
# when adding members to an ensemble, they are auto-fitted to the full
  # training dataset and therefore ready to make predictions
ran_ensemble <- simple_ensemble() %>% 
  add_member(ran_models, metric = "roc_auc")
# can also use roc_auc and tss_max as metrics
ran_ensemble
autoplot(ran_ensemble)


## Projecting to the Present ##

# make predictions with the ensemble
prediction_present <- predict_raster(ran_ensemble, climate_present)
ggplot() +
  geom_spatraster(data = prediction_present, aes(fill = mean)) +
  scale_fill_terrain_c() + # "c" for continuous variables
  # plot the presences used in the model
  geom_sf(data = ran_occ_thin %>% filter(class == "presence"))

# subset the model to only use the best models, based on Boyce continuous index
# set threshold of 0.8 for Boyce continuous index
  # 0.8 threshold excluded all models, used 0.7 instead
# take the median of the available model predictions (mean is the default)
prediction_present_boyce <- predict_raster(ran_ensemble, climate_present, 
                                           metric_thresh = c("roc_auc", 0.8), 
                                           fun= "median")

ggplot() +
  geom_spatraster(data = prediction_present_boyce, aes(fill = median)) +
  scale_fill_terrain_c() + # c = continuous
  geom_sf(data = ran_occ_thin %>% filter(class == "presence"))
# if plot doesn't change much, models are consistent
# model gives us probability of occurrence
# can convert to binary predictions (present vs absence)
ran_ensemble_binary <- calib_class_thresh(ran_ensemble, 
                                          class_thresh = "tss_max")

# now predict for the whole continent?
prediction_present_binary <- predict_raster(ran_ensemble_binary, 
                                            climate_present, 
                                            type = "class", 
                                            class_thresh = c("tss_max"))

ggplot() +
  geom_spatraster(data = prediction_present_binary, aes(fill = binary_mean)) +
  geom_sf(data = ran_occ_thin %>% filter(class == "presence"))


## Projecting to the Future ##

# full list of future projections from WorldClim:
help("WorldClim_2.1")
# ssp = Shared Socioeconomic Pathways, 126, 245, 370, 585 available
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

climate_future <- pastclim::region_slice(
  time_ce = 2090, 
  bio_variables = vars_uncor, # uncorrelated variables created previously
  data = "WorldClim_2.1_HadGEM3-GC31-LL_ssp245_10m", 
  crop = na_bound #boundary polygon for study area
)

# predict using the ensemble:
prediction_future <- predict_raster(ran_ensemble, climate_future)
ggplot() +
  geom_spatraster(data = prediction_future, aes(fill = mean)) +
  scale_fill_terrain_c()


## Visualizing the Contribution of Individual Variables ##

# marginal response curves can show the effect of a variable while keeping
  # all other variables at their mean
# use step_profile() to create a new recipe for generating a dataset to make 
  # the marginal prediction
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
    add_member(ran_thin_rep_models, metric = "boyce_cont")
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
