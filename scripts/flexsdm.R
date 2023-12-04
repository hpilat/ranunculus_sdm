# Flexsdm package
# devtools::install_github('sjevelazco/flexsdm')
library(flexsdm)
library(terra)
library(dplyr)

# Delimit of a calibration area
somevar <- system.file("external/somevar.tif", package = "flexsdm")
somevar <- terra::rast(somevar) # environmental data
names(somevar) <- c("aet", "cwd", "tmx", "tmn")
data(abies)
abies

# select columns with longitude, latitude, and occurrences
abies_presence <- abies %>%
  dplyr::select(x, y, pr_ab) %>%
  filter(pr_ab == 1) # filter only for presence locations, 0 = absence

# create a calibration area with 100 km buffer around presence points
cal_area <-
  calib_area(
    data = abies_presence,
    x = 'x', # longitude
    y = 'y', # latitude
    method =  c('buffer', width = 100000),
    crs = crs(somevar) # somevar = enviro data multilayer raster
  )

# visualize the species occurrences
# create layer1 object for 1st layer of environmental data
layer1 <- somevar[[1]]
layer1[!is.na(layer1)] <- 1 # block NA values in environmental data

# plot occurrences as points
plot(layer1, col="gray80", legend=FALSE, axes=FALSE)
plot(crop(cal_area, layer1), add=TRUE) # crop layer1 to fit calibrated area
points(abies_presence[,c("x", "y")], col = "#00000480") # x and y = lon and lat

## Occurrence Filtering

# thin the Red fir occurrences based on environmental space, using 8 bins
# add unique ID to each row:
abies_presence$id <- 1:nrow(abies_presence) # all rows have unique ID
abies_presencefilt <- abies_presence %>% 
  occfilt_env(
    data = .,
    x = "x", # longitude
    y = "y", # latitude
    id = "id", # column with unique IDs
    nbins = 8, 
    env_layer = somevar # environmental variables
  ) %>% 
  left_join(abies_presence, by = c("id", "x", "y")) # relevant columns
plot(layer1, col = "gray80", legend = FALSE, axes = FALSE)
plot(crop(cal_area, layer1), add = TRUE)
points(abies_presence[, c("x", "y")], col = "#00000480") # plot coordinates
points(abies_presencefilt[, c("x", "y")], col = "#5DC86180") # plot filtered coordinates

## Block Partition with 4 Folds

# geographically-structured partitioning good for transferring models
# divide the data into 4 different partitions using the spatial block method
set.seed(10)
occ_part <- abies_presencefilt %>% 
  part_sblock(
    data = ., # dataframe or tibble with occurrence records and coordinates
    env_layer = somevar, # must be continuous environmental variables
    pr_ab = "pr_ab", # occurrence column
    x = "x", # longitude
    y = "y", # latitude
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
occ_part # resulting spatraster after block partitioning
abies_presencefilt <- occ_part$part

# transform best block partition to a raster layer with same resolution and 
  # extent as predictor variables. Input block partitioned raster from above
block_layer <- get_block(env_layer = somevar, best_grid = occ_part$grid)
cl <- c("#64146D", "#9E2962", "#F47C15", "#FCFFA4")
plot(block_layer, col = cl, legend = FALSE, axes = FALSE)
points(abies_presencefilt[, c("x", "y")])
# number of presences per block:
abies_presencefilt %>% 
  dplyr::group_by(.part) %>% 
  dplyr::count()
# additional information of the best block:
occ_part$best_part_info

## Pseudoabsence/Background Points

# using partition (created previously) as a mask
# spatial blocks where species occurs
# sample background points throughout the study area with random method
  # allocate 10 times the number of presences as background
set.seed(10)
background <- lapply(1:4, function(x) {
  sample_background(
    data = abies_presencefilt, 
    x = "x", 
    y = "y", 
    n = sum(abies_presencefilt$.part == x) * 10, 
    method = "random", 
    rlayer = block_layer, 
    maskval = x, # original code had X here
    calibarea = cal_area
  )
}) %>% 
  bind_rows()
background <- sdm_extract(data = background, x = "x", y = "y", env_layer = block_layer)

# sample a number of pseudoabsences equal to the presence in each partition
set.seed(10)
pseudoabsences <- lapply(1:4, function(x) {
  sample_pseudoabs(
    data = abies_presencefilt, 
    x = "x", 
    y = "y", 
    n = sum(abies_presencefilt$.part == x), 
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
abies_pres_abs <- bind_rows(abies_presencefilt, pseudoabsences)
# presence-pseudoabsence database:
abies_pres_abs
# background points:
background

# extract environmental data for presence-absence and background data
abies_pres_abs <- abies_pres_abs %>% 
  sdm_extract(
    data = ., 
    x = "x", 
    y = "y", 
    env_layer = somevar, 
    filter_na = TRUE
  )

# view the distributions of presence points, pseudoabsence points, and background 
  # points using the blocks as a reference map
background <- background %>%
  sdm_extract(
    data = ., 
    x = "x", 
    y = "y",
    env_layer = somevar, 
    filter_na = TRUE
  )




## LEFT OFF HERE ##




# Model Fitting: 

# Tuned MaxEnt model:
t_max <- tune_max(
  data = abies_pres_abs,
  response = "pr_ab",
  predictors = names(somevar),
  background = background,
  partition = ".part",
  grid = expand.grid(
    regmult = seq(0.1, 3, 0.5),
    classes = c("l", "lq", "lqhpt")
  ),
  thr = c("max_sens_spec", "equal_sens_spec", "max_sorensen"),
  metric = "TSS", # sub with AUC?
  clamp = TRUE,
  pred_type = "cloglog"
)

# Tuned Gaussian Process Model:
f_gau <- fit_gau(
  data = abies_pres_abs, 
  response = "pr_ab", 
  predictors = names(somevar), 
  partition = ".part", 
  thr = c("max_sens_spec", "equal_sens_spec", "max_sorensen")
)

# Tuned GLM:
f_glm <- fit_glm(
  data = abies_pres_abs, 
  response = "pr_ab", 
  predictors = names(somevar), 
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


### left off here ###

# Project the Ensemble Model:
# project the ensemble model across the California Floristic Province
  # can project with multiple threshold values
# end result is a SpatRaster with continuous suitability values above the threshold
projection1 <- sdm_predict(
  models = ensemble, 
  pred = somevar, 
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
thr_val <- ens_m$performance %>% 
  dplyr::filter(threshold == "max_sens_spec") %>% 
  pull(thr_value)
m_pres <- msdm_posteriori(
  records = abies_p, 
  x = "x", 
  y = "y", 
  pr_ab = "pr_ab", 
  cont_suit = pr_1$meanw[[1]], 
  method = c("obr"), 
  thr = c("sensitivity", sens = thr_val), 
  buffer = NULL
)

constrained <- m_pres$meanw[[1]]
names(constrained) <- "constrained"
cl <- c("#FDE725", "#B3DC2B", "#6DCC57", "#36B677", "#1F9D87", "#25818E", 
                 "#30678D", "#3D4988", "#462777", "#440154")
plot(constrained, col = cl, legend = FALSE, axes = FALSE)
