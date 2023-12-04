# Flexsdm package
# devtools::install_github('sjevelazco/flexsdm')
library(flexsdm)
library(terra)
library(dplyr)

# Delimit of a calibration area

names(pred_rast) <- c("CAN_elv_msk", 
                      "SBIO4_Temperature_Seasonality_0_5cm",
                      "SBIO4_Temperature_Seasonality_5_15cm")

names(ran_occ) <- c("gbifID", "occurrenceStatus", "longitude", "latitude")

# data(abies)

# change occurrence status to 1 for presence/detection
# would need to also change absent to 0 in a presence/absence dataset
ran_occ$occurrenceStatus <- 1
# create an object for the presences?
presences <- ran_occ$occurrenceStatus

# select columns with longitude, latitude, and occurrences
ran_occ_pres <- ran_occ %>%
   dplyr::select("longitude", "latitude", "occurrenceStatus") # %>%
   # dplyr::filter("occurrenceStatus" == 1) # filter only for presence locations
   # filter for 1 in a dataset of presences (1) and absences (0)

names(ran_occ_pres) <- c("longitude", "latitude", "occurrenceStatus")

# create a calibration area with 100 km buffer around presence points
cal_area <-
  calib_area(
    data = ran_occ,
    x = 'longitude', # longitude
    y = 'latitude', # latitude
    method =  c('buffer', width = 100000),
    crs = crs(pred_rast) # somevar = enviro data multilayer raster
  )

# visualize the species occurrences
layer1 <- pred_rast[[1]]
layer1[!is.na(layer1)] <- 1

# layer2 <- pred_multirast[[2]]
# layer2[!is.na(layer2)] <- 1

plot(layer1, col="gray80", legend=FALSE, axes=FALSE)
plot(crop(cal_area, layer1), add=TRUE)
points(ran_occ_pres[,c("longitude", "latitude")], col = "#00000480")

# Occurrence Filtering
# thin the occurrences based on environmental space, using 8 bins
# add unique ID to each row:
ran_occ$gbifID <- 1:nrow(ran_occ) 
ran_occ_presfilt <- ran_occ %>% 
  occfilt_env(
    data = .,
    x = "longitude", 
    y = "latitude", 
    id = "gbifID", # column with unique IDs
    nbins = 8, 
    env_layer = pred_rast
  ) %>% 
  left_join(ran_occ, by = c("gbifID", "longitude", "latitude")) # relevant columns
plot(layer1, col = "gray80", legend = FALSE, axes = FALSE)
plot(crop(cal_area, layer1), add = TRUE)
points(ran_occ[, c("longitude", "latitude")], col = "#00000480") # plot coordinates
points(ran_occ[, c("longitude", "latitude")], col = "#5DC86180") # plot filtered coordinates

# Block Partition with 4 Folds
# geographically-structured partitioning good for transferring models
# divide the data into 4 different partitions using the spatial block method
set.seed(10)
occ_part <- ran_occ_presfilt %>% 
  part_sblock(
    data = ., # dataframe or tibble with occurrence records
    env_layer = pred_rast, 
    pr_ab = "occurrenceStatus", # occurrence column
    x = "longitude", # longitude
    y = "latitude", # latitude
    n_part = 4, # number of partitions, default is 2
    min_res_mult = 3, # min value used for multiplying raster resolution and 
    # the finest resolution to be tested. Default is 3
    max_res_mult = 200, # max value used for multiplying raster resolution and
    # the coarsest resolution to be tested, default 200
    num_grids = 30, # number of grid to be tested between min and max raster
    # resolution, default is 30
    prop = 1 # proportion of point used for testing autocorrelation between groups
    # values between 0 and 1
  )
occ_part # resulting SpatRaster after block partitioning
ran_occ_presfilt <- occ_part$part

# transform best block partition to a raster layer with same resolution and 
# extent as predictor variables. Input partitioned raster from above
block_layer <- get_block(env_layer = pred_rast, best_grid = occ_part$grid)
cl <- c("#64146D", "#9E2962", "#F47C15", "#FCFFA4")
plot(block_layer, col = cl, legend = FALSE, axes = FALSE)
points(ran_occ_presfilt[, c("x", "y")])
# number of presences per block:
ran_occ_presfilt %>% 
  dplyr::group_by(.part) %>% 
  dplyr::count()
# additional information of the best block:
occ_part$best_part_info

# Pseudoabsence/Background Points
# using partition (created previously) as a mask
# spatial blocks where species occurs
# sample background points throughout the study area with random method
# allocate 10 times the number of presences as background
set.seed(10)
background <- lapply(1:4, function(x) {
  sample_background(
    data = ran_occ_presfilt, 
    x = "x", 
    y = "y", 
    n = sum(ran_occ_presfilt$.part == x) * 10, 
    method = "random", 
    rlayer = block_layer, 
    maskval = x, 
    calibarea = cal_area
  )
}) %>% 
  bind_rows()
background <- sdm_extract(data = background, x = "x", y = "y", env_layer = block_layer)

# sample a number of pseudoabsences equal to the presence in each partition
set.seed(10)
pseudoabsences <- lapply(1:4, function(x) {
  sample_pseudoabs(
    data = ran_occ_presfilt, 
    x = "x", 
    y = "y", 
    n = sum(ran_occ_presfilt$.part == x), 
    method = "random", 
    rlayer = block_layer, 
    maskval = x, 
    calibarea = cal_area
  )
}) %>% 
  bind_rows()
pseudoabsences <- sdm_extract(data = pseudoabsences, x = "x", y = "y", env_layer = block_layer)

cl <- c("#64146D", "#9E2962", "#F47C15", "#FCFFA4")
plot(block_layer, col = "gray80", legend = FALSE, axes = FALSE)
# background points:
points(background[, c("x", "y")], col = cl[background$.part], cex = 0.8)
# pseudoabsences
points(pseudoabsences[, c("x", "y")], bg = cl[pseudoabsences$.part], cex = 0.8, pch = 21) 

# bind presences and pseudoabsences
ran_pr_abs <- bind_rows(ran_occ_presfilt, pseudoabsences)
# presence-pseudoabsence database:
ran_pr_abs
# background points:
background

# extract environmental data for presence-absence and background data
ran_pr_abs <- ran_pr_abs %>% 
  sdm_extract(
    data = ., 
    x = "x", 
    y = "y", 
    env_layer = pred_rast, 
    filter_na = TRUE
  )

# Model Fitting:

# Tuned MaxEnt model:
## error in running this model: says columns don't exist (in pred_rast?)
t_max <- tune_max(
  data = ran_pr_abs,
  response = "pr_ab",
  predictors = names(pred_rast),
  background = background,
  partition = ".part",
  grid = expand.grid(
    regmult = seq(0.1, 3, 0.5),
    classes = c("l", "lq", "lqhpt")
  ),
  thr = c("max_sens_spec", "equal_sens_spec", "max_sorensen"),
  metric = "TSS",
  clamp = TRUE,
  pred_type = "cloglog"
)

# Tuned Gaussian Process Model:
f_gau <- fit_gau(
  data = ran_pr_abs, 
  response = "pr_ab", 
  predictors = names(pred_rast), 
  partition = ".part", 
  thr = c("max_sens_spec", "equal_sens_spec", "max_sorensen")
)

# Tuned GLM:
f_glm <- fit_glm(
  data = ran_pr_abs, 
  response = "pr_ab", 
  predictors = names(pred_rast), 
  partition = ".part", 
  thr = c("max_sens_spec", "equal_sens_spec", "max_sorensen"), 
  poly = 2
)

# Fit an Ensemble Model:
# fit an ensemble model for red fir based on the weighted average of the 
# 3 individual models
ensemble <- fit_ensemble(
  models = list(f_gau, f_glm), # should have t_max but model wouldn't run
  ens_method = "meanw", 
  thr = c("max_sens_spec", "equal_sens_spec", "max_sorensen"), 
  thr_model = "max_sens_spec", 
  metric = "TSS"
)
ensemble$performance

# use sdm_summarize() to merge model performance tables to compare metrics across models
model_perf <- sdm_summarize(list(f_gau, f_glm, ensemble)) # should include t_max
model_perf

# Project the Ensemble Model:
# project the ensemble model across the California Floristic Province
# can project with multiple threshold values
# end result is a SpatRaster with continuous suitability values above the threshold
projection1 <- sdm_predict(
  models = ensemble, 
  pred = pred_rast, 
  thr = "max_sens_spec", 
  con_thr = TRUE, # continuous suitability raster above threshold values
  predict_area = NULL
)

unconstrained <- projection1$meanw[[1]]
names(unconstrained) <- "unconstrained"

cl <- c("#FDE725", "#B3DC2B", "#6DCC57", "#36B677", "#1F9D87", "#25818E", 
                 "#30678D", "#3D4988", "#462777", "#440154")
                 plot(unconstrained, col = cl, legend = FALSE, axes = FALSE)
                 
# Constrain the model with msdm_posterior
# occurrence-based restriction assumes suitable patches that intercept species 
# occurrences are more likely part of a species' distribution 
# must use the original database (presences without filtering)
thr_val <- ensemble$performance %>% 
  dplyr::filter(threshold == "max_sens_spec") %>% 
  pull(thr_value)
m_pres <- msdm_posteriori(
  records = ran_occ_pres,
  x = "longitude", 
  y = "latitude", 
  pr_ab = "occurrenceStatus", # error says this column does not exist
  cont_suit = projection1$meanw[[1]], 
  method = c("obr"), 
  thr = c("sensitivity", sens = thr_val), 
  buffer = NULL
  )
                 
constrained <- m_pres$meanw[[1]]
names(constrained) <- "constrained"
cl <- c("#FDE725", "#B3DC2B", "#6DCC57", "#36B677", "#1F9D87", "#25818E", 
        "#30678D", "#3D4988", "#462777", "#440154")
plot(constrained, col = cl, legend = FALSE, axes = FALSE)
                                  