## tidysdm package tutorial

library(tidysdm)


## Data Preparation ##


# read in a set of presences for a species of lizard that inhabits the Iberian Peninsula
data(lacerta)
lacerta

# plot the presences on a map to visualize them
# cast coordinates into an sf object and set its CRS to WGS84
library(sf)
lacerta <- st_as_sf(lacerta, coords = c("longitude", "latitude"))
st_crs(lacerta) <- 4326 # WGS84

# plot the locations directly on the raster used to extract climatic variables
# get land mask for available datasets and use that as background for locations
library(pastclim)
worldclim <- download_dataset(dataset = "WorldClim_2.1_10m", bio_variables = NULL, 
                 annual = TRUE, monthly = FALSE)
land_mask <- 
  get_land_mask(time_ce = 1985, dataset = "WorldClim_2.1_10m")

# Iberian Peninsula extension
iberia_poly <- 
  terra::vect("POLYGON((-9.8 43.3,-7.8 44.1,-2.0 43.7,3.6 42.5,3.8 41.5,1.3 40.8,0.3 39.5,
     0.9 38.6,-0.4 37.5,-1.6 36.7,-2.3 36.3,-4.1 36.4,-4.5 36.4,-5.0 36.1,
    -5.6 36.0,-6.3 36.0,-7.1 36.9,-9.5 36.6,-9.4 38.0,-10.6 38.9,-9.5 40.8,
    -9.8 43.3))"
  )

crs(iberia_poly) <- "lonlat"
# crop the extent
land_mask <- crop(land_mask, iberia_poly)
# and mask to the polygon
land_mask <- mask(land_mask, iberia_poly)

# use tidyterra for plotting so terra rasters can be plotted with ggplot
library(tidyterra)
library(ggplot2)
ggplot() +
  geom_spatraster(data = land_mask, aes(fill = land_mask_1985)) +
  geom_sf(data = lacerta)


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

# thin occurrences further  to remove points closer than 20km
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
                                 raster = land_mask, 
                                 coords = NULL, 
                                 method = c("dist_min", km2m(50))
)

# plot presences and absences
ggplot() +
  geom_spatraster(data = land_mask, aes(fill = land_mask_1985)) + 
  geom_sf(data = lacerta_thin, aes(col = class))

# see which variables are available from WorldClim dataset
climate_vars <- get_vars_for_dataset("WorldClim_2.1_10m")

# download the dataset at the correct resolution (10 arcmin here)
download_dataset("WorldClim_2.1_10m")
# dataset covers years 1970-2000, pastclim dates it at 1985 (midpoint)

# create a SpatRaster for the worlclim dataset
climate_present <- pastclim::region_slice(
  time_ce = 1985, 
  bio_variables = climate_vars, 
  data = "WorldClim_2.1_10m", 
  crop = iberia_poly
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
    preproc = list(default = lacerta_rec), 
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
               resamples = lacerta_cv, grid = 3, 
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