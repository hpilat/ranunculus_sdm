## tidysdm package tutorial

library(tidysdm)


## Data Preparation ##


# read in a set of presences for a species of lizard that inhabits the Iberian Peninsula
data(lacerta)
lacerta # tibble / dataframe

# plot the presences on a map to visualize them
# cast coordinates into an sf object and set its CRS to WGS84
library(sf)
# convert tibble/dataframe to sf object
lacerta <- st_as_sf(lacerta, coords = c("longitude", "latitude"))
# set CRS to WGS84
st_crs(lacerta) <- 4326

# plot the locations directly on the raster used to extract climatic variables
# get land mask for available datasets and use that as background for locations
library(pastclim)
worldclim <- download_dataset(dataset = "WorldClim_2.1_10m", bio_variables = NULL, 
                 annual = TRUE, monthly = FALSE)

# create land mask from pastclim
land_mask <- 
  pastclim::get_land_mask(time_ce = 1985, dataset = "WorldClim_2.1_10m")

# Iberian Peninsula boundary polygon
iberia_poly <- 
  terra::vect("POLYGON((-9.8 43.3,-7.8 44.1,-2.0 43.7,3.6 42.5,3.8 41.5,1.3 40.8,0.3 39.5,
     0.9 38.6,-0.4 37.5,-1.6 36.7,-2.3 36.3,-4.1 36.4,-4.5 36.4,-5.0 36.1,
    -5.6 36.0,-6.3 36.0,-7.1 36.9,-9.5 36.6,-9.4 38.0,-10.6 38.9,-9.5 40.8,
    -9.8 43.3))"
  )

crs(iberia_poly) <- "lonlat"
# crop the extent of the land mask to match the Iberian Peninsula
land_mask <- crop(land_mask, iberia_poly)
# and mask to the polygon
land_mask <- mask(land_mask, iberia_poly)

# use tidyterra for plotting so terra rasters can be plotted with ggplot
library(tidyterra)
library(ggplot2)
ggplot() +
  geom_spatraster(data = land_mask, aes(fill = land_mask_1985)) +
  geom_sf(data = lacerta) # sf object with coordinates


## Thinning Occurrences ##

# thin the observations to have one per cell in the raster
  # better when using an equal area projection
set.seed(1234567)
lacerta <- thin_by_cell(lacerta, raster = land_mask)
nrow(lacerta)

# plot the thinned occurrences
ggplot() +
  geom_spatraster(data = land_mask, aes(fill = land_mask_1985)) +
  geom_sf(data = lacerta)

# thin occurrences further to remove points closer than 20km
  # use km2m to reduce number of 0s written for 20km
set.seed(1234567)
lacerta_thin <- thin_by_dist(lacerta, dist_min = km2m(20))
nrow(lacerta_thin)

# plot the thinned occurrences again
ggplot() +
  geom_spatraster(data = land_mask, aes(fill = land_mask_1985)) +
  geom_sf(data = lacerta_thin)


##  Sample Pseudoabsences  ##

# constrain pseudoabsences to be minimum 50km away from presence points
# select 3 times as many pseudoabsences as presences
set.seed(1234567)
lacerta_thin <- sample_pseudoabs(lacerta_thin, 
                                 n = 3 * nrow(lacerta_thin), 
                                 raster = land_mask, # base raster
                                 coords = NULL, 
                                 method = c("dist_min", km2m(50))
)
# lacerta_thin now contains presences and pseudoabsences

# plot presences and absences
ggplot() +
  geom_spatraster(data = land_mask, aes(fill = land_mask_1985)) + 
  geom_sf(data = lacerta_thin, aes(col = class))


# see which variables are available from WorldClim dataset
climate_vars <- get_vars_for_dataset("WorldClim_2.1_10m")
climate_vars

# download the dataset at the correct resolution (10 arcmin here)
download_dataset("WorldClim_2.1_10m")
# dataset covers years 1970-2000, pastclim dates it at 1985 (midpoint)
# TRUE = download successful

# create a SpatRaster for the Worldclim dataset
climate_present <- pastclim::region_slice(
  time_ce = 1985, 
  bio_variables = climate_vars, 
  data = "WorldClim_2.1_10m", 
  crop = iberia_poly # SpatVector of Iberian Peninsula boundary
)

# select variables noticeably different from the underlying background
# first extract climate for all presences and pseudo-absences
lacerta_thin <- lacerta_thin %>% 
  bind_cols(terra::extract(climate_present, lacerta_thin, ID = FALSE))

# use violin plots to compare the distribution of climatic variables for
  # presences and pseudoabsences
lacerta_thin %>% plot_pres_vs_bg(class)

# want to select variables where presences have different values from background
# rank variables based on the overlap of the respective density plots
library(overlapping)
lacerta_thin %>% dist_pres_vs_bg(class)

# focus on variables with at least 30% of non-overlapping distribution between 
  # presences and pseudoabsences
vars_to_keep <- lacerta_thin %>% dist_pres_vs_bg(class)
vars_to_keep <- names(vars_to_keep[vars_to_keep > 0.30])
lacerta_thin <- lacerta_thin %>% select(all_of(c(vars_to_keep, "class")))
vars_to_keep

# selected certain variables based on literature:
suggested_vars <- c("bio05", "bio06", "bio13", "bio14", "bio15")

# inspect the correlation among variables:
pairs(climate_present[[suggested_vars]])

# subset to variables below a correlation threshold of 0.7
climate_present <- climate_present[[suggested_vars]]
vars_uncor <- filter_high_cor(climate_present, cutoff = 0.7)
vars_uncor # removes bio14, which was correlated with bio05

# select uncorrelated variables
lacerta_thin <- lacerta_thin %>% select(all_of(c(vars_uncor, "class")))
climate_present <- climate_present[[vars_uncor]]

## Model fitting ##

# fit the model by cross-validation
# need to set up a recipe to define how to handle our dataset
lacerta_rec <- recipe(lacerta_thin, formula = class ~ .)
lacerta_rec 
# class = outcome, other variables are predictors, coords is X and Y columns
# assumption is that the level of interest for the response (presences) is the reference level
# confirm the data are correctly formatted:
lacerta_thin %>% check_sdm_presence(class)

# now build a workflow_set of different models
# used glm, random forest, boosted_trees, and maxent here
# tidysdm automatically chooses the most important parameters
lacerta_models <- 
  workflow_set(          # create workflow_set
    preproc = list(default = lacerta_rec), # lacerta recipe from above
    models = list(
      glm = sdm_spec_glm(),        # standard glm specs
      rf = sdm_spec_rf(),          # rf specs with tuning
      gbm = sdm_spec_boost_tree(),       # boosted tree model specs with tuning
      maxent = sdm_spec_maxent()   # maxent specs with tuning
      ), 
      cross = TRUE       # make all combinations of preproc and models
  ) %>% 
  option_add(control = control_ensemble_grid()) # tweak controls to store info 
                     # needed later to create the ensemble

# set up a spatial block cross-validation scheme to tune and assess models
# 80:20 split, 5 folds
set.seed(100)
lacerta_cv <- spatialsample::spatial_block_cv(lacerta_thin, v = 5)
autoplot(lacerta_cv)

# use block CV folds to tune and assess the models
# used 3 combinations of hyperparameters per model, far too few for reality
# find out how many combinations to use
# ran code below and error said ranger and xgboost packages required
library(ranger)
library(xgboost)
set.seed(1234567)
lacerta_models <- 
  lacerta_models %>% 
  workflow_map("tune_grid", 
               resamples = lacerta_cv, grid = 3, # grid needs to be higher number
               metrics = sdm_metric_set(), verbose = TRUE
  )
# workflow_set correctly detects no tuning parameters for glm

# look at the performance of the models:
autoplot(lacerta_models)

## Ensemble ##
# use Boyce-continuous index as metric to choose best 
    # random forest and boosted tree models
# when adding members to an ensemble, they are auto-fitted to the full 
  # training dataset and therefore ready to make predictions
lacerta_ensemble <- simple_ensemble() %>% 
  add_member(lacerta_models, metric = "boyce_cont")
# can also use roc_auc and tss_max
lacerta_ensemble
autoplot(lacerta_ensemble)

## Projecting to the Present ## 

# make predictions with the ensemble
prediction_present <- predict_raster(lacerta_ensemble, climate_present)
ggplot() +
  geom_spatraster(data = prediction_present, aes(fill = mean)) +
  scale_fill_terrain_c() + # "c" for continuous values 
  # plot the presences used in the model
  geom_sf(data = lacerta_thin %>% filter(class == "presence"))

# subset model to use only the best models, based on Boyce continuous index
# set threshold of 0.8 for Boyce continuous index
# take the median of the available model predictions (mean is default)
prediction_present_boyce <- predict_raster(lacerta_ensemble, climate_present, 
                                           metric_thresh = c("boyce_cont", 0.8), 
                                           fun = "median")
ggplot() + 
  geom_spatraster(data = prediction_present_boyce, aes(fill = median)) +
  scale_fill_terrain_c() + 
  geom_sf(data = lacerta_thin %>% filter(class =="presence"))
# plot didn't change much, therefore the models are consistent
# gives us probability of occurrence
# can convert to binary predictions (presence vs absence)
lacerta_ensemble <- calib_class_thresh(lacerta_ensemble, 
                                       class_thresh = "tss_max"
                                       )

# now predict for the whole continent:
prediction_present_binary <- predict_raster(lacerta_ensemble, 
                                            climate_present, 
                                            type = "class", 
                                            class_thresh = c("tss_max")
                                            )
ggplot() +
  geom_spatraster(data = prediction_present_binary, aes(fill = binary_mean)) +
  geom_sf(data = lacerta_thin %>% filter(class == "presence"))


## Projecting to the Future ##


# full list of future projections from WorldClim:
help("WorldClim_2.1")
download_dataset("WorldClim_2.1_HadGEM3-GC31-LL_ssp245_10m")

# see which times are available:
get_time_ce_steps("WorldClim_2.1_HadGEM3-GC31-LL_ssp245_10m")
# predict for 2090 (which is 2081-2100)

# check the available variables:
get_vars_for_dataset("WorldClim_2.1_HadGEM3-GC31-LL_ssp245_10m")
# future predictions don't include altitude (doesn't change over time)
  # to use altitude, would have to copy it over from the present
  # but not in set of uncorrelated variables from earlier so don't include in this case
climate_future <- pastclim::region_slice(
  time_ce = 2090,
  bio_variables = vars_uncor, 
  data = "WorldClim_2.1_HadGEM3-GC31-LL_ssp245_10m", 
  crop = iberia_poly # boundary polygon for area of study
)

# predict using the ensemble:
prediction_future <- predict_raster(lacerta_ensemble, climate_future)
ggplot() +
  geom_spatraster(data = prediction_future, aes(fill = mean)) +
  scale_fill_terrain_c()


## Visualizing the Contribution of Individual Variables ##

# marginal response curves can show the effect of a variable while keeping 
  # all other variables at their mean
# use step_profile() to create a new recipe for generating a dataset to make 
  # the marginal prediction
# investigate the contribution of bio05:
bio05_prof <- lacerta_rec %>%  # recipe from before
  step_profile(-bio05, profile = vars(bio05)) %>% 
  prep(training = lacerta_thin)

bio05_data <- bake(bio05_prof, new_data = NULL)

bio05_data <- bio05_data %>% 
  mutate(
    pred = predict(lacerta_ensemble, bio05_data)$mean
  )

ggplot(bio05_data, aes(x = bio05, y = pred)) +
  geom_point(alpha = .5, cex = 1)


## Repeated Ensembles ##

# explore the effect of thinning and sampling pseudoabsences on model performance
# create a list of simple_ensembles by looping through the SDM pipeline
# empty object to store the simple ensembles we will create:
ensemble_list <- list()
set.seed(123) # make sure you set the seed OUTSIDE the loop

for (i_repeat in 1:3) {
  # thin the data
  lacerta_thin_rep <- thin_by_cell(lacerta, raster = climate_present)
  lacerta_thin_rep <- thin_by_dist(lacerta_thin_rep, dist_min = 20000)
  # sample pseudo-absences
  lacerta_thin_rep <- sample_pseudoabs(lacerta_thin_rep,
                                       n = 3 * nrow(lacerta_thin_rep),
                                       raster = climate_present,
                                       method = c("dist_min", 50000)
  )
  # get climate
  lacerta_thin_rep <- lacerta_thin_rep %>%
    bind_cols(terra::extract(climate_present, lacerta_thin_rep, ID = FALSE))
  # create folds
  lacerta_thin_rep_cv <- spatial_block_cv(lacerta_thin_rep, v = 5)
  # create a recipe
  lacerta_thin_rep_rec <- recipe(lacerta_thin_rep, formula = class ~ .)
  # create a workflow_set
  lacerta_thin_rep_models <-
    # create the workflow_set
    workflow_set(
      preproc = list(default = lacerta_thin_rep_rec),
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
  lacerta_thin_rep_models <-
    lacerta_thin_rep_models %>%
    workflow_map("tune_grid",
                 resamples = lacerta_thin_rep_cv, grid = 10,
                 metrics = sdm_metric_set(), verbose = TRUE
    )
  # make an simple ensemble and add it to the list
  ensemble_list[[i_repeat]] <- simple_ensemble() %>%
    add_member(lacerta_thin_rep_models, metric = "boyce_cont")
}

# now create repeat_ensemble from the list:
lacerta_thin_rep_ens <- repeat_ensemble() %>% add_repeat(ensemble_list)
lacerta_thin_rep_ens

# predict by taking the mean and median of all models
lacerta_thin_rep_ens <- predict_raster(lacerta_thin_rep_ens, climate_present, 
                                       fun = c("mean", "median")
                                       )
ggplot() + 
  geom_spatraster(data = lacerta_thin_rep_ens, aes(fill = median)) +
  scale_fill_terrain_c()