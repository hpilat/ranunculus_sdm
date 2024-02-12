# tidysdm tutorial

# dir.create("outputs/")

# attempting multiraster with categorical rasters converted to numeric (line 104)

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
                               n = 10 * nrow(ran_occ_thin), 
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
ran_occ_th_no_NA <- na.omit(ran_occ_th)
summary(ran_occ_th_no_NA)

# use violin plots to compare the distribution of the variables for presences
  # and pseudoabsences
# below code returned 49 warnings prior to changing layer names
  # unable to see plots
# after changing layer names, able to see plots. 11 warnings
ran_occ_th_no_NA %>% plot_pres_vs_bg(class)
# try removing NA values?
  # update: assigning -9999 to NA values skews these plots really badly (as expected)
# ran_occ_thin_NA <- ran_occ_thin[!is.na(ran_occ_thin$), ]

# want to select variables for which presences use values different from 
  # the background/pseudoabsences
# rank the variables based on the overlap of the respective density plots
ran_occ_th_no_NA %>% dist_pres_vs_bg(class)
# above code wouldn't run, "Error in density.default(X[[i]], ...) : non-finite 'from'"
  # probably having NA values is the issue here
  # set NA values in multiraster to -9999 and the code ran

# focus on variables with at least 30% of non-overlapping distribution between presences and pseudoabsences
  # this will remove a lot, maybe try 25% later?
vars_to_keep <- ran_occ_th_no_NA %>% dist_pres_vs_bg(class)
vars_to_keep <- names(vars_to_keep[vars_to_keep > 0.3])
ran_occ_th <- ran_occ_th_no_NA %>% select(all_of(c(vars_to_keep, "class")))
vars_to_keep

# tutorial has a list of variables suggested by the literature to be important
  # in determining the distribution of the species
names(predictors_multi)
# below code isn't working in lines 148 and 151 
suggested_vars <- c("anth_biome", "Climate", "elevation_na", "lndcvr_na", 
                    "TYPE_PA", "TYPE_PA", "soil_phh2o_0_5", "soilphh2o_5_15", 
                    "soil_temp_0_5", "soil(soil_temp_5_15")

# inspect the variables for collinearity
pairs(predictors_multi[[suggested_vars]])
# subset to variables below 0.8 Pearson's correlation coefficient
# start with 0.7 first <-  was the same list as 8, so going with 8
ran_occ_th <- ran_occ_th[[suggested_vars]] # suggested_vars part causes an error
predictors_uncorr <- filter_high_cor(predictors_multi, cutoff = 0.8, names = TRUE)
predictors_uncorr

# remove highly correlated predictors
ran_occ_th <- ran_occ_th %>% select(all_of(c(predictors_uncorr, "class")))
predictors_multi_num <- predictors_multi_num[[predictors_uncorr]]