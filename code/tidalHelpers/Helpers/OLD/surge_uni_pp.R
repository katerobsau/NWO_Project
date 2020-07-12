surge_dir = "/Users/katesaunders/Documents/No_Back_Up_Data/SURGE/"
num_members = 50
na_value = 99.99
location_ref = "LAUW"
element_ref = "SURG"
print_my_warnings = FALSE
overwrite_saved = FALSE
selected_season = "WINTER"

ensemble_path <- paste(surge_dir, "reduced_ENS_",location_ref, ".rds", sep = "")

# -----------------------------------------------------------------------------

# PACKAGES AND FUNCTIONS

library(tidyverse)
library(gamlss)
library(Rcpp)
sourceCpp("code/tidalHelpers/Helpers/crps_ensemble.cpp")

# -----------------------------------------------------------------------------

# READ IN THE ENSEMBLE DATA
ensemble_data <- readRDS(ensemble_path)

# -----------------------------------------------------------------------------

# ADD NECESSARY COLUMNS FOR PREPROCESSING
summer_months = 3:8
ensemble_data <- ensemble_data %>%
  # CHECK NA HANDLING (should already be fine)
  dplyr::mutate(OBS = if_else(OBS == na_value, NA_real_, OBS)) %>%
  # ADD SEASON COL
  dplyr::mutate(MONTH = lubridate::month(FORECAST_DATE)) %>%
  dplyr::mutate(SEASON = MONTH %in% summer_months) %>%
  dplyr::mutate(SEASON = dplyr::if_else(SEASON, "SUMMER", "WINTER")) %>%
  # ADD A GROUP_REF FOR L.O.O.CV (season != year)
  dplyr::mutate(YEAR = lubridate::year(FORECAST_DATE)) %>%
  dplyr::mutate(GROUP_REF = dplyr::if_else(MONTH < min(summer_months),
                                      YEAR - 1, YEAR)) %>%
  dplyr::mutate(GROUP_REF =
                  dplyr::if_else(YEAR %in% c(2015,2017) & SEASON == "WINTER",
                2015, YEAR)) %>%
  dplyr::select(-MONTH, -YEAR) %>%
  # REDUCE THE NUMBER OF LEAD TIMES
  dplyr::mutate(NUM_LT = as.numeric(LEAD_TIME/60/60)) %>%
  dplyr::filter(NUM_LT == trunc(NUM_LT)) %>%
  dplyr::select(-NUM_LT) %>%
  # ADD A FILTER_REF FOR ITERATING OVER
  tidyr::unite("FILTER_REF", c("INIT_TIME", "LEAD_TIME", "SEASON"))

if(print_my_warnings){
  warning("Hard coded a filter on lead times for hourly only")
  warning("Hard coded and combined 2015 and 2017 winters in the group ref")
}

# Comment:
# should be able to do this SEASON with a case_when() but
# lubridate /dplyr interactions seemed odd
# also tried a function (could try again)

# -----------------------------------------------------------------------------
#
# # NA OBS
# # manual check of missing observations
# na_obs <- ensemble_data %>%
#   dplyr::select(SEASON, INIT_TIME, LEAD_TIME, OBS) %>%
#   dplyr::mutate(NA_OBS = is.na(OBS)) %>%
#   dplyr::group_by(SEASON, INIT_TIME, LEAD_TIME) %>%
#   dplyr::count(NA_OBS) %>%
#   dplyr::filter(NA_OBS == TRUE)
# print(paste("Max missing per lead time, init time and season group is",
#             max(na_obs$n)))
# print("NEED TO FIX THIS TO HANDLE NAs AUTOMATICALLY")

# -----------------------------------------------------------------------------
#
# # GET LEAVE ONE OUT GROUPS (SEPARATE GROUPS BY SEASON)
# # Not really counting by lead_time ...
#
# count_data_by_groups = ensemble_data %>%
#   dplyr::filter(!is.na(OBS)) %>%
#   dplyr::select(SEASON, GROUP_REF, FORECAST_DATE) %>%
#   dplyr::distinct() %>%
#   dplyr::group_by(SEASON) %>%
#   dplyr::count(GROUP_REF) %>%
#   dplyr::mutate(NOT_SUITED = n < 365/2*0.9)
# count_data_by_groups
#
# # cv_groups = count_data_by_groups %>%
# #   dplyr::filter(NOT_SUITED == FALSE) %>%
# #   dplyr::pull(GROUP_REF) %>%
# #   unique()
#
# summer_groups = count_data_by_groups %>%
#   dplyr::filter(SEASON == "SUMMER", NOT_SUITED == FALSE) %>%
#   dplyr::pull(GROUP_REF) %>%
#   unique()
#
# winter_groups = count_data_by_groups %>%
#   dplyr::filter(SEASON == "WINTER", NOT_SUITED == FALSE) %>%
#   dplyr::pull(GROUP_REF) %>%
#   unique()

# -----------------------------------------------------------------------------
#
# # DATA WARNING FOR CV GROUPS
#
# if(print_my_warnings){
#
#   if(any(count_data_by_groups$NOT_SUITED == TRUE))
#     warning("Not all years suited to be left out of the train data")
#
#   check_groups_by_season = count_data_by_groups %>%
#     dplyr::filter(NOT_SUITED == TRUE) %>%
#     dplyr::select(SEASON) %>%
#     dplyr::count(SEASON)
#
#   count_by_season = check_groups_by_season %>%
#     dplyr::pull(n)
#
#   if(length(count_by_season) == 1)
#     warning("Years left out of the train data differ by season")
#
#   if(count_by_season[1] != count_by_season[2])
#     warning("Years left out of the train data differ by season")
#
# }

# -----------------------------------------------------------------------------
#
# # SPLIT THE ENSEMBLE INTO FORECAST GROUPS
# # These groups are init, lead and season
# # For a given date, we assume we have all lead time and init data
# # However, the number of dates per season per year may differ
# # For this reason the LOO.CV groups are different
# # So we must apply the LOO.CV for seasonal group separately
# # So the ensemble must be filtered by season, then split by init and lead time
#
# summer_split <- ensemble_data %>%
#   dplyr::filter(SEASON == "SUMMER") %>%
#   group_by(INIT_TIME, LEAD_TIME) %>%
#   group_split()
#
# winter_split <- ensemble_data %>%
#   dplyr::filter(SEASON == "WINTER") %>%
#   group_by(INIT_TIME, LEAD_TIME) %>%
#   group_split()
#
# # ensemble_split <- ensemble_data %>%
# #   group_by(INIT_TIME, LEAD_TIME, SEASON) %>%
# #   group_split()
#
# print("NOTE: Would be nice to generalise the split using a fixed var arguement")

# -----------------------------------------------------------------------------

seperate_test_and_train_data <- function(df, g){

  y_train = df %>%
    dplyr::filter(GROUP_REF != g) %>%
    dplyr::select(OBS) %>%
    dplyr::filter(!is.na(OBS)) %>%
    as.matrix()

  X_train = df %>%
    dplyr::filter(GROUP_REF != g) %>%
    dplyr::filter(!is.na(OBS)) %>%
    dplyr::select(starts_with("PAR"))

  group_meta = df %>%
    dplyr::filter(GROUP_REF == g)

  # raw_ens = df %>%
  #   dplyr::filter(GROUP_REF == g) %>%
  #   dplyr::select(starts_with("ENS"))

  X_test = df %>%
    dplyr::filter(GROUP_REF == g) %>%
    dplyr::select(PAR_MEAN, PAR_SIGMA)

  y_test = df %>%
    dplyr::filter(GROUP_REF == g) %>%
    dplyr::select(OBS) %>%
    # dplyr::filter(!is.na(OBS)) %>%
    as.matrix()

  data_list = list(y_train = y_train, y_test = y_test,
                   X_train = X_train, X_test = X_test,
                   group_meta = group_meta)

  return(data_list)

}

simulate_rnorm_ensemble <- function(pred, num_members = 50){

  sim_ensemble <- mapply(rnorm,
                         mean = pred$mu,
                         sd = pred$sigma,
                         USE.NAMES = TRUE,
                         MoreArgs = list(n = num_members)) %>%
    t() %>%
    as.data.frame() %>%
    set_names(paste("SIM", 1:num_members, sep = ""))

  return(sim_ensemble)

}

sample_clim_ensemble <- function(num_reps, df, num_members = 50){
  clim_ensemble <- replicate(num_reps,
            sample(x = df$OBS[!is.na(df$OBS)], size = num_members)
  ) %>%
  t() %>%
  as.data.frame() %>%
  set_names(paste("CLIM", 1:num_members, sep = ""))
  return(clim_ensemble)
}

# -----------------------------------------------------------------------------

# count_by_groups <- ensemble_data %>%
#   tidyr::unite("FILTER_REF", c("INIT_TIME", "LEAD_TIME", "SEASON")) %>%
#   dplyr::select(FILTER_REF, GROUP_REF) %>%
#   dplyr::group_by(FILTER_REF) %>%
#   dplyr::count(GROUP_REF) %>%
#   dplyr::filter(n < 365/2*0.9 | n > 184*1.1)

# -----------------------------------------------------------------------------

# Don't need to do this (already added the groups)
# iterations <- expand.grid(INIT_TIME = unique(ensemble_data$INIT_TIME),
#                           LEAD_TIME = unique(ensemble_data$LEAD_TIME),
#                           SEASON = unique(ensemble_data$SEASON)) %>%
#   dplyr::mutate(INIT_TIME = lubridate::as.duration(INIT_TIME),
#                 LEAD_TIME = lubridate::as.duration(LEAD_TIME))
#
# row_i = iterations[i, ]
#
# df <- ensemble_data %>%
#   dplyr::filter(INIT_TIME == row_i$INIT_TIME,
#                 LEAD_TIME == row_i$LEAD_TIME,
#                 SEASON == row_i$SEASON)

# -----------------------------------------------------------------------------

all_filter_refs = unique(ensemble_data$FILTER_REF)
new_ensembles = NULL

for(f in all_filter_refs){
  df <- ensemble_data %>%
    dplyr::filter(FILTER_REF == f)
  loo_data <- seperate_test_and_train_data(df, g)
  model_fit <- fit_NGR(loo_data$y_train, loo_data$X_train)
  pred <- predict_NGR(model_fit, loo_data$X_test)
  sim_ensemble <- simulate_rnorm_ensemble(pred)
  clim_ensemble <- sample_clim_ensemble(num_reps = nrow(sim_ensemble), df = df)
  filter_ref_ensembles <- data.frame(loo_data$group_meta,
                                 sim_ensemble, clim_ensemble)
  new_ensembles <- rbind(new_ensembles, filter_ref_ensembles)

}
# crps_sim = crps_ensemble(as.matrix(sim_ensemble), as.matrix(loo_data$y_test))
# crps_raw = crps_ensemble(as.matrix(loo_data$raw_ens), as.matrix(loo_data$y_test))
# crps_clim = crps_ensemble(clim_ensemble, as.matrix(loo_data$y_test))




# crpss_sim = 1 - crps_sim/crps_clim
# crpss_raw = 1 - crps_raw/crps_clim

score_data <- data.frame(crps_raw, crps_sim, crps_clim) %>%
  bind_cols(df %>% dplyr::filter(GROUP_REF == g))

if(print_my_warnings){
  warning("Need to think more deeply about how to sample the climate")
  warning("Hard coded an NA filter on the train data")
}

# -----------------------------------------------------------------------------

# FIT THE GAMLSS MODEL
# so output list -> length(ensemble_split) x # of groups)
if(selected_season == "WINTER"){
  ensemble_split = winter_split
  cv_groups = winter_groups
}else if(selected_season == "SUMMER"){
  ensemble_split = summer_split
  cv_groups = summer_groups
}else{
  stop("Mispecified your season")
}
# if I do the summer/winter season separately there is no need for NA handling

# -----------------------------------------------------------------------------

crps_scores_loo <- lapply(ensemble_split, function(df){

  # Fit the NGR (leaving out one group each time)
  NGR_fits_loo <- lapply(cv_groups, function(g){

    y_train = df %>%
      dplyr::filter(GROUP_REF != g) %>%
      dplyr::select(OBS) %>%
      dplyr::filter(!is.na(OBS)) %>%
      as.matrix()

    X_train = df %>%
      dplyr::filter(GROUP_REF != g) %>%
      dplyr::filter(!is.na(OBS)) %>%
      dplyr::select(PAR_MEAN, PAR_SIGMA)

    if(print_my_warnings)
      warning("PARAMETER HARD CODE")

    fit <- fit_NGR(y_train, X_train)

    return(fit)

  })
  if(print_my_warnings)
    warning("Filtered out NA obs manually")

  # Predict from the fitted NGR model (using the group left out)
  predict_from_loo <- lapply(cv_groups, function(g){

    i = which(g == cv_groups)
    model_fit = NGR_fits_loo[[i]]

    X_test = df %>%
      dplyr::filter(GROUP_REF == g) %>%
      dplyr::select(PAR_MEAN, PAR_SIGMA)
    if(nrow(X_test)== 0)
      stop(print("Problem with test data for group", g))

    if(print_my_warnings)
      warning("PARMAETER HARDCODE")

    pred <- predict_NGR(model_fit, X_test)

    return(pred)

  })

  # Simulate an ensemble from the predicted parameters
  simulated_ensemble <- lapply(predict_from_loo, function(pred, num_members){

    sim_ensemble <- mapply(rnorm,
                           mean = pred$mu,
                           sd = pred$sigma,
                           USE.NAMES = TRUE,
                           MoreArgs = list(n = num_members)) %>%
      t() #%>%
      #as.data.frame() %>%
      #set_names(paste("SIM", 1:num_members, sep = "")) %>%
      #bind_cols(df %>% dplyr::select(-starts_with("ENS")))

  }, num_members)

  # Score the ensemble prediction using the CRPS
  if(print_my_warnings)
    warning("globally specified, cv_groups, simulated_ensemble, df")
  # (internal specification in the lapply failed ? don't know why)

  crps_results <- lapply(cv_groups, function(g){

                      i = which(g == cv_groups)
                      ens = simulated_ensemble[[i]]

                      Y_test = df %>%
                        dplyr::filter(GROUP_REF == g) %>%
                        dplyr::select(OBS) %>%
                        as.matrix()

                      crps_simulated = crps_ensemble(ens = ens, obs = Y_test)

                      ens_raw = df %>%
                        dplyr::filter(GROUP_REF == g) %>%
                        dplyr::select(starts_with("ENS")) %>%
                        as.matrix()

                      crps_raw = crps_ensemble(ens = ens_raw, obs = Y_test)

                      crps_scores = data.frame(Predicted = crps_simulated,
                                               Raw = crps_raw)
                      return(crps_scores)
  })

  result_list <- list(fits = NGR_fits_loo,
                      ens = simulated_ensemble,
                      crps = crps_results)

  return(result_list)

})

print("Should specify the functions separately")

# ---------------------------------------------------------------------------------

climate_data <- ensemble_data %>%
  dplyr::filter(SEASON == selected_season) %>%
  dplyr::select(FORECAST_DATE, INIT_TIME, LEAD_TIME, OBS) %>%
  dplyr::mutate(LEAD_TIME = as.numeric(LEAD_TIME/60/60)) %>%
  tidyr::pivot_wider(names_from = LEAD_TIME, values_from = OBS)

# # CRPSS SCORES
#
# crpss <- function(crps_pred, crps_ref){
#   1 - crps_pred/crps_ref
# }

# ---------------------------------------------------------------------------------

# ESTIMATE THE CLIMATE DISTRIBUTION
climate_distribution = ensemble_data %>%
  dplyr::filter(!is.na(OBS)) %>%
  dplyr::filter(SEASON == selected_season) %>%
  dplyr::mutate(OBS_DATE = FORECAST_DATE + INIT_TIME + LEAD_TIME) %>%
  dplyr::select(OBS_DATE, OBS) %>%
  dplyr::distinct() %>%
  dplyr::pull(OBS)

auto_corr = which(acf(climate_distribution)$acf < 0.1)[1]
# i = seq(1, length(climate_distribution), by = auto_corr)
# sampled_climate = climate_distribution[i]
#
# # FEW THINGS TO NOTE:
# library(moments)
# clim_mean = mean(sampled_climate, na.rm = TRUE)
# clim_sd = sd(sampled_climate, na.rm = TRUE)
# clim_mean; clim_sd;
# skewness(sampled_climate, na.rm = TRUE)
# kurtosis(sampled_climate, na.rm = TRUE)
#
# scale = sqrt(3*var(sampled_climate)/(pi^2))
#
# ggplot() +
#   geom_density(aes(climate_distribution)) +
#   geom_density(aes(sampled_climate), linetype = "dotted") +
#   geom_density(aes(rnorm(10000, clim_mean, clim_sd)), col = "blue") +
#   geom_density(aes(rnorm(10000, clim_mean, clim_sd*0.55)),
#                col = "blue", linetype = "dotted") +
#   geom_density(aes(rlogis(10000, location = clim_mean, scale = scale)), col = "red") +
#   geom_density(aes(rlogis(10000, location = clim_mean, scale = scale*0.625)),
#                col = "red", linetype = "dotted")

raw_ens <- ensemble_split %>% do.call("rbind", .)
clim_ens <- replicate(nrow(raw_ens),
                      sample(climate_distribution, num_members))
y_obs <- raw_ens %>% dplyr::pull(OBS)
raw_ens <- raw_ens %>% dplyr::select(starts_with("ENS"))

crps_raw <- crps_ensemble(clim_ens, y_obs)

# ---------------------------------------------------------------------------------

# Visualise the CRPS scores
crps_results <- lapply(1:length(ensemble_split),
                          function(i){
                            df = ensemble_split[[i]] %>%
                              dplyr::select(INIT_TIME, LEAD_TIME, SEASON) %>%
                              dplyr::distinct()
                            l = crps_scores_loo[[i]]
                            crps_info = do.call("rbind", l$crps) %>%
                              bind_cols(df)
                            return(crps_info)
                            }
                       ) %>%
                       do.call("rbind", .) %>%
  tidyr::pivot_longer(cols = c("Predicted", "Raw"),
               names_to = "Method", values_to = "CRPS") %>%
  dplyr::mutate(NUM_LT = as.numeric(LEAD_TIME/60/60/24)) %>%
  dplyr::filter(NUM_LT == trunc(NUM_LT))

ggplot(data = crps_results) +
  geom_boxplot(aes(x = NUM_LT, y = CRPS,
                   group = interaction(NUM_LT, Method),
                   col = Method)) +
  xlab("Lead time in days") +
  scale_x_continuous(breaks = seq(0:10)) +
  ggtitle(paste(selected_season, location_ref)) +
  theme_bw()

# Do I want to include outliers?
# Change the legend labels ? shift legend position

# ---------------------------------------------------------------------------------
# * iterate over element in the list (init, lead time, season)
# * find the group that minimised the mean, median
# * if the mean is very different examine the model fit for the large scores
# * select the best group
# * Choose the model that scores the lowest on the independent data set
# * for the best prediction model, show the rank histograms
# * I'm clearly confused here as to how to select the best model
# * Final step is to write up the CRPS score and the rank histogram result and what leave one out is
# * Potentially also try a student-t (instead of a normal just to see ?)



