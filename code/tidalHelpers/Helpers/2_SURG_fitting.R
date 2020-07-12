# -----------------------------------------------------------------------------

# READ IN THE ENSEMBLE DATA
if(!file.exists(ensemble_path))
  stop("Can't find the ensemble file")
ensemble_data <- readRDS(ensemble_path)

# -----------------------------------------------------------------------------

# ADD NECESSARY COLUMNS FOR PREPROCESSING / GAMLSS FITTING
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
                  dplyr::if_else(YEAR %in% combine_winter & SEASON == "WINTER",
                                 combine_winter[1], YEAR)) %>%
  dplyr::select(-MONTH, -YEAR) %>%
  # REDUCE THE NUMBER OF LEAD TIMES (HOURLY ONLY)
  dplyr::mutate(NUM_LT = as.numeric(LEAD_TIME/60/60)) %>%
  dplyr::filter(NUM_LT == trunc(NUM_LT)) %>%
  dplyr::select(-NUM_LT) %>%
  # ADD A FILTER_REF FOR ITERATING OVER
  tidyr::unite("FILTER_REF", c("INIT_TIME", "LEAD_TIME", "SEASON"))

warning("Hard coded 2015 and 2017 winters as a single 2015 group ref")
if(print_my_warnings){
  warning("Hard coded a filter for hourly lead times")
}

# -----------------------------------------------------------------------------

# FOR EACH OF THE FITTING GROUPS DO L.O.O.C.V

# Check if this output is already saved
if(file.exists(pred_file_path) & overwrite_saved == FALSE){
  ensemble_pred = readRDS(pred_file_path)

  # Otherwise fit the models and save output
}else{

  # Group and split by data for fitting
  ensemble_split = ensemble_data %>%
    group_by(FILTER_REF) %>%
    group_split()

  # Fit and predict using loo cv
  ensemble_pred = vector("list", length(ensemble_split))
  for(l in 1:length(ensemble_split)){
    print(l)
    ensemble_pred[[l]] <- loo_cv(ensemble_split[[l]], omit_groups = omit_years)
  }
  # Did this in a list incase something crashes for an easy restart

  # save the output
  saveRDS(ensemble_pred, pred_file_path)

  # remove from memory
  rm(ensemble_split)

  if(print_my_warnings){
    warning("Need to think more deeply about how to sample the climate")
    warning("Hard coded an NA filter on the train data")
  }

}

# -----------------------------------------------------------------------------

# SIMULATE ENSEMBLE FROM PREDICTIONS
simulated_ensemble <- lapply(ensemble_pred, simulate_rnorm_ensemble)

# -----------------------------------------------------------------------------

# SIMULATE CLIMATE ENSEMBLE
climate_ensemble <- lapply(ensemble_pred, sample_climate_ensemble)
# cheated with the sampling and am only using the observations at a given lead time
# as an estimate of the climate distribution (rough, but probably sufficient)

# -----------------------------------------------------------------------------

# COMBINE IT ALL TOGETHER
sim_ens <- do.call("rbind", simulated_ensemble)
clim_ens <- do.call("rbind", climate_ensemble)
pred_df <- do.call("rbind", ensemble_pred)

combined_data <- bind_cols(pred_df, sim_ens, clim_ens)
rm(sim_ens, clim_ens, pred_df)

# SEPARATE THE REFERENCE COLS: INIT, LEAD, SEASON
ensemble_data_extended <- combined_data %>%
  tidyr::separate("FILTER_REF",
                  c("INIT_TIME", "LEAD_TIME", "SEASON"), sep = "_") %>%
  dplyr::mutate(INIT_TIME = lubridate::as.duration(INIT_TIME),
                LEAD_TIME = parse_number(LEAD_TIME)) %>%
  dplyr::mutate(LEAD_TIME = lubridate::as.duration(LEAD_TIME))

# -----------------------------------------------------------------------------

## ADD A FINAL SAVE STEP HERE
saveRDS(ensemble_data_extended, ens_pp_path)

# -----------------------------------------------------------------------------

# # OLD INITIALISATION
# surge_dir = "/Users/katesaunders/Documents/No_Back_Up_Data/SURGE/"
# num_members = 50
# na_value = 99.99
# location_ref = "HARL" #"LAUW"
# element_ref = "SURG"
# print_my_warnings = FALSE
# overwrite_saved = FALSE
# selected_season = "WINTER"
# combine_winter = c(2015,2017)
# omit_years = c(2010,2019)
#
# ensemble_path <- paste(surge_dir, "reduced_ENS_",location_ref, ".rds", sep = "")
# pred_file_path = paste(getwd(), "/data/", location_ref, "_pred.rds", sep = "")
# ens_pp_path = paste(getwd(), "/data/", location_ref, "_pp_ens.rds", sep = "")
#
# # -----------------------------------------------------------------------------
#
# # PACKAGES AND FUNCTIONS
# library(tidyverse)
# library(gamlss)
# library(Rcpp)
# sourceCpp("code/tidalHelpers/Helpers/crps_ensemble.cpp")
# source("code/tidalHelpers/Helpers/LAUW_fitting_utils.R")
# # gotta source this! NGR_functions.R
