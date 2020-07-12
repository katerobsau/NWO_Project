surge_dir = "/Users/katesaunders/Documents/No_Back_Up_Data/SURGE/"
num_members = 50
na_value = 99.99
location_ref = "LAUW"
element_ref = "SURG"
print_my_warnings = FALSE
overwrite_saved = FALSE
selected_season = "WINTER"
combine_winter = c(2015,2017)
omit_years = c(2010,2019)

ensemble_path <- paste(surge_dir, "reduced_ENS_",location_ref, ".rds", sep = "")
pred_file_path = paste(getwd(), "/data/", location_ref, "_pred.rds", sep = "")

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

seperate_test_and_train_data <- function(df, g){

  y_train = df %>%
    dplyr::filter(GROUP_REF != g) %>%
    dplyr::filter(!is.na(OBS)) %>%
    dplyr::select(OBS) %>%
    as.matrix()

  X_train = df %>%
    dplyr::filter(GROUP_REF != g) %>%
    dplyr::filter(!is.na(OBS)) %>%
    dplyr::select(starts_with("PAR"))

  group_data = df %>%
    dplyr::filter(GROUP_REF == g)

  # raw_ens = df %>%
  #   dplyr::filter(GROUP_REF == g) %>%
  #   dplyr::select(starts_with("ENS"))

  X_test = df %>%
    dplyr::filter(GROUP_REF == g) %>%
    dplyr::select(starts_with("PAR"))

  y_test = df %>%
    dplyr::filter(GROUP_REF == g) %>%
    dplyr::select(OBS) %>%
    # dplyr::filter(!is.na(OBS)) %>%
    as.matrix()

  data_list = list(y_train = y_train, y_test = y_test,
                   X_train = X_train, X_test = X_test,
                   group_data = group_data)

  return(data_list)

}

loo_cv <- function(df, omit_groups = NULL){

  df_pred = NULL
  group_refs = unique(df$GROUP_REF) %>% setdiff(omit_groups)
  for(g in group_refs){

    loo_data <- seperate_test_and_train_data(df, g)
    model_fit <- fit_NGR(loo_data$y_train, loo_data$X_train)
    pred_from_fit <- predict_NGR(model_fit, loo_data$X_test)
    group_pred <- bind_cols(loo_data$group_data, pred_from_fit)
    df_pred <- bind_rows(df_pred, group_pred)

  }

  return(df_pred)

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

sample_climate_ensemble <- function(df, num_members = 50){

  # FIX ME!!! With equi-space quantiles
  clim_ensemble <- replicate(nrow(df),
            sample(x = df$OBS[!is.na(df$OBS)], size = num_members)
  ) %>%
  t() %>%
  as.data.frame() %>%
  set_names(paste("CLIM", 1:num_members, sep = ""))
  return(clim_ensemble)
}

# -----------------------------------------------------------------------------

# FOR EACH OF THE FITTING GROUPS DO L.O.O.C.V

# Check if this output is already saved
if(file.exists(pred_file_path) & overwrite_saved = FALSE){
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

# Add CRPS Scores
crps_sim = crps_ensemble(ensemble_data_extended  %>% dplyr::select(starts_with("SIM")) %>% as.matrix(),
                         ensemble_data_extended  %>% dplyr::select(OBS) %>% as.matrix())
crps_raw = crps_ensemble(ensemble_data_extended  %>% dplyr::select(starts_with("ENS")) %>% as.matrix(),
                         ensemble_data_extended %>% dplyr::select(OBS) %>% as.matrix())
crps_ref = crps_ensemble(ensemble_data_extended  %>% dplyr::select(starts_with("CLIM")) %>% as.matrix(),
                         ensemble_data_extended  %>% dplyr::select(OBS) %>% as.matrix())

ensemble_data_extended <- ensemble_data_extended %>%
  dplyr::mutate(CRPS_SIM = crps_sim,
                CRPS_RAW = crps_raw,
                CRPS_REF = crps_ref) %>%
  dplyr::mutate(CRPSS_SIM = 1 - CRPS_SIM/CRPS_REF,
                CRPSS_RAW = 1 - CRPS_RAW/CRPS_REF)

rm(crps_sim, crps_raw, crps_ref)

# ---------------------------------------------------------------------------------

# ADD THE RANK SCORES
sim_rank <- apply(ensemble_data_extended %>%
                    dplyr::select("OBS", starts_with("SIM")), 1,
                  rank, ties.method = "random")[1,]

raw_rank <- apply(ensemble_data_extended %>%
                    dplyr::select("OBS", starts_with("ENS")), 1,
                  rank, ties.method = "random")[1,]

clim_rank <- apply(ensemble_data_extended %>%
                     dplyr::select("OBS", starts_with("CLIM")), 1,
                  rank, ties.method = "random")[1,]

ensemble_data_extended <- ensemble_data_extended %>%
  dplyr::mutate(RANK_CLIM = clim_rank,
                RANK_SIM = sim_rank,
                RANK_RAW = raw_rank)

rm(clim_rank, sim_rank, raw_rank)

# ---------------------------------------------------------------------------------

# CLIMATE DISTRIBUTION
clim_samples <- ensemble_data_extended %>%
  dplyr::filter(LEAD_TIME == 0, INIT_TIME == 0) %>%
  dplyr::select(FORECAST_DATE, SEASON, OBS)

clim_plot <- ggplot(data = clim_samples) +
  geom_density(aes(OBS, group = SEASON, col = SEASON)) +
  theme_bw()
clim_plot

season_cols <- clim_samples %>%
  tidyr::pivot_wider(names_from = SEASON, values_from = OBS) %>%
  dplyr::select(-FORECAST_DATE)

library(moments)
clim_mean = colMeans(season_cols, na.rm = TRUE)
clim_median = apply(season_cols,2, median, na.rm = TRUE)
clim_sd = apply(season_cols,2, sd, na.rm = TRUE)
clim_skew = apply(season_cols,2, skewness, na.rm = TRUE)
clim_kurt = apply(season_cols,2, kurtosis, na.rm = TRUE)
clim_mean; clim_median; clim_sd; clim_skew; clim_kurt

# ---------------------------------------------------------------------------------

# ADD THE BSS

probs = seq(0.05, 0.95, 0.05)
seasonal_quantiles <- apply(season_cols, 2, quantile, probs = probs, na.rm = TRUE)
quantiles <- seasonal_quantiles %>% as.vector() %>% unique() %>% sort()

get_brier_score = function(row, t, num_members = 50){
  bs = (sum(row[-1] > t)/num_members - (row[1] > t))^2
  return(bs)
}

obs_and_sim_ens <- ensemble_data_extended %>%
  dplyr::select("OBS", starts_with("SIM"))

bs_sim = NULL
for(q in quantiles){
  print(q)
  bs_score = apply(obs_and_sim_ens, 1, get_brier_score, q)
  bs_sim = rbind(bs_sim, bs_score)
}

bs_sim = t(bs_sim) %>%
  as.data.frame(bs_sim) %>%
  setNames(paste("BS_SIM", quantiles, sep = "_"))

ensemble_data_extended <- ensemble_data_extended %>%
  dplyr::select(-starts_with("BS_SIM")) %>%
  bind_cols(bs_sim)

# ---------------------------------------------------------------------------------

# Visualise the CRPS scores
crps_results <- ensemble_data_extended %>%
  dplyr::select(INIT_TIME, LEAD_TIME, SEASON, CRPSS_SIM, CRPSS_RAW) %>%
  tidyr::pivot_longer(cols = c("CRPSS_SIM", "CRPSS_RAW"),
                      names_to = "Method", values_to = "CRPS") %>%
  dplyr::mutate(NUM_LT = as.numeric(LEAD_TIME/60/60/24)) %>%
  dplyr::filter(NUM_LT == trunc(NUM_LT))

ggplot(data = crps_results) +
  geom_boxplot(aes(x = NUM_LT, y = CRPS,
                   group = interaction(NUM_LT, Method),
                   col = Method), outlier.alpha = 0) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_wrap(~SEASON) +
  xlab("Lead time in days") +
  ylab("CRPSS") +
  scale_x_continuous(breaks = seq(0:10)) +
  ggtitle(location_ref) +
  theme_bw() +
  ylim(c(-1, 1))

# Do I want to include outliers?
# Change the legend labels ? shift legend position
warning("Shorted the y axis")

# -----------------------------------------------------------------------------

# Visualise the rank histograms
rank_results <- ensemble_data_extended %>%
  dplyr::select(INIT_TIME, LEAD_TIME, SEASON,
                RANK_RAW, RANK_CLIM, RANK_SIM) %>%
  tidyr::pivot_longer(cols = c("RANK_RAW", "RANK_CLIM", "RANK_SIM"),
                      names_to = "Method", values_to = "RANK") %>%
  dplyr::mutate(NUM_LT = as.numeric(LEAD_TIME/60/60/24)) %>%
  dplyr::filter(NUM_LT == trunc(NUM_LT))

plot_list = vector("list", 9)
for(lead_days in 1:9){
plot_list[[lead_days]] <- ggplot(data = rank_results %>%
         dplyr::filter(NUM_LT == lead_days)) +
  geom_histogram(aes(RANK), binwidth = 1) +
  # geom_hline(yintercept = 1/51, linetype = "dotted") +
  facet_grid(SEASON~Method) +
  xlab("Rank") +
  ggtitle(paste(location_ref, lead_days)) +
  theme_bw()
}

plot_list[[3]]
plot_list[[9]]

# Plot rank by lead times for Sim
rank_hist <- ggplot(data = rank_results %>%
                      dplyr::filter(SEASON == "WINTER") %>%
                      dplyr::filter(Method == "RANK_SIM")) +
  geom_histogram(aes(RANK), binwidth = 1) +
  facet_wrap(~ NUM_LT, ncol = 3) +
  ggtitle(paste("Rank Histogram:", "Winter at", location_ref)) +
  theme_bw()

rank_hist

# -----------------------------------------------------------------------------

# Visualise the brier scores
bs_preprocess <- ensemble_data_extended %>%
  dplyr::select(INIT_TIME, LEAD_TIME, SEASON,
                starts_with("BS_SIM")) %>%
  tidyr::pivot_longer(cols = starts_with("BS"),
                      names_to = "THRESHOLD", values_to = "BS_SIM") %>%
  dplyr::mutate(THRESHOLD = readr::parse_number(THRESHOLD)) %>%
  dplyr::mutate(NUM_LT = as.numeric(LEAD_TIME/60/60/24)) %>%
  dplyr::filter(NUM_LT == trunc(NUM_LT))

bs_results <- bs_preprocess %>%
  dplyr::group_by(INIT_TIME, LEAD_TIME, NUM_LT, SEASON, THRESHOLD) %>%
  dplyr::summarise(MEAN_BS_SIM = mean(BS_SIM, na.rm = TRUE))

# Plot rank by lead times for Sim
season_ref = "SUMMER"
bs_plot <- ggplot(data = bs_results %>%
                      dplyr::filter(SEASON == season_ref)) +
  geom_point(aes(x = THRESHOLD, y = MEAN_BS_SIM)) +
  facet_wrap(~ NUM_LT, ncol = 3) +
  ggtitle(paste("Brier Score:", season_ref, "at", location_ref)) +
  theme_bw()

bs_plot

# -----------------------------------------------------------------------------


# # FIT THE GAMLSS MODEL
# # so output list -> length(ensemble_split) x # of groups)
# if(selected_season == "WINTER"){
#   ensemble_split = winter_split
#   cv_groups = winter_groups
# }else if(selected_season == "SUMMER"){
#   ensemble_split = summer_split
#   cv_groups = summer_groups
# }else{
#   stop("Mispecified your season")
# }
# # if I do the summer/winter season separately there is no need for NA handling
#
# # -----------------------------------------------------------------------------
#
# crps_scores_loo <- lapply(ensemble_split, function(df){
#
#   # Fit the NGR (leaving out one group each time)
#   NGR_fits_loo <- lapply(cv_groups, function(g){
#
#     y_train = df %>%
#       dplyr::filter(GROUP_REF != g) %>%
#       dplyr::select(OBS) %>%
#       dplyr::filter(!is.na(OBS)) %>%
#       as.matrix()
#
#     X_train = df %>%
#       dplyr::filter(GROUP_REF != g) %>%
#       dplyr::filter(!is.na(OBS)) %>%
#       dplyr::select(PAR_MEAN, PAR_SIGMA)
#
#     if(print_my_warnings)
#       warning("PARAMETER HARD CODE")
#
#     fit <- fit_NGR(y_train, X_train)
#
#     return(fit)
#
#   })
#   if(print_my_warnings)
#     warning("Filtered out NA obs manually")
#
#   # Predict from the fitted NGR model (using the group left out)
#   predict_from_loo <- lapply(cv_groups, function(g){
#
#     i = which(g == cv_groups)
#     model_fit = NGR_fits_loo[[i]]
#
#     X_test = df %>%
#       dplyr::filter(GROUP_REF == g) %>%
#       dplyr::select(PAR_MEAN, PAR_SIGMA)
#     if(nrow(X_test)== 0)
#       stop(print("Problem with test data for group", g))
#
#     if(print_my_warnings)
#       warning("PARMAETER HARDCODE")
#
#     pred <- predict_NGR(model_fit, X_test)
#
#     return(pred)
#
#   })
#
#   # Simulate an ensemble from the predicted parameters
#   simulated_ensemble <- lapply(predict_from_loo, function(pred, num_members){
#
#     sim_ensemble <- mapply(rnorm,
#                            mean = pred$mu,
#                            sd = pred$sigma,
#                            USE.NAMES = TRUE,
#                            MoreArgs = list(n = num_members)) %>%
#       t() #%>%
#       #as.data.frame() %>%
#       #set_names(paste("SIM", 1:num_members, sep = "")) %>%
#       #bind_cols(df %>% dplyr::select(-starts_with("ENS")))
#
#   }, num_members)
#
#   # Score the ensemble prediction using the CRPS
#   if(print_my_warnings)
#     warning("globally specified, cv_groups, simulated_ensemble, df")
#   # (internal specification in the lapply failed ? don't know why)
#
#   crps_results <- lapply(cv_groups, function(g){
#
#                       i = which(g == cv_groups)
#                       ens = simulated_ensemble[[i]]
#
#                       Y_test = df %>%
#                         dplyr::filter(GROUP_REF == g) %>%
#                         dplyr::select(OBS) %>%
#                         as.matrix()
#
#                       crps_simulated = crps_ensemble(ens = ens, obs = Y_test)
#
#                       ens_raw = df %>%
#                         dplyr::filter(GROUP_REF == g) %>%
#                         dplyr::select(starts_with("ENS")) %>%
#                         as.matrix()
#
#                       crps_raw = crps_ensemble(ens = ens_raw, obs = Y_test)
#
#                       crps_scores = data.frame(Predicted = crps_simulated,
#                                                Raw = crps_raw)
#                       return(crps_scores)
#   })
#
#   result_list <- list(fits = NGR_fits_loo,
#                       ens = simulated_ensemble,
#                       crps = crps_results)
#
#   return(result_list)
#
# })
#
# print("Should specify the functions separately")
#
# # ---------------------------------------------------------------------------------
#
# climate_data <- ensemble_data %>%
#   dplyr::filter(SEASON == selected_season) %>%
#   dplyr::select(FORECAST_DATE, INIT_TIME, LEAD_TIME, OBS) %>%
#   dplyr::mutate(LEAD_TIME = as.numeric(LEAD_TIME/60/60)) %>%
#   tidyr::pivot_wider(names_from = LEAD_TIME, values_from = OBS)
#
# # # CRPSS SCORES
# #
# # crpss <- function(crps_pred, crps_ref){
# #   1 - crps_pred/crps_ref
# # }
#
# # ---------------------------------------------------------------------------------
#
# # ESTIMATE THE CLIMATE DISTRIBUTION
# climate_distribution = ensemble_data %>%
#   dplyr::filter(!is.na(OBS)) %>%
#   dplyr::filter(SEASON == selected_season) %>%
#   dplyr::mutate(OBS_DATE = FORECAST_DATE + INIT_TIME + LEAD_TIME) %>%
#   dplyr::select(OBS_DATE, OBS) %>%
#   dplyr::distinct() %>%
#   dplyr::pull(OBS)
#
# auto_corr = which(acf(climate_distribution)$acf < 0.1)[1]
# # i = seq(1, length(climate_distribution), by = auto_corr)
# # sampled_climate = climate_distribution[i]
# #
# # # FEW THINGS TO NOTE:
# # library(moments)
# # clim_mean = mean(sampled_climate, na.rm = TRUE)
# # clim_sd = sd(sampled_climate, na.rm = TRUE)
# # clim_mean; clim_sd;
# # skewness(sampled_climate, na.rm = TRUE)
# # kurtosis(sampled_climate, na.rm = TRUE)
# #
# # scale = sqrt(3*var(sampled_climate)/(pi^2))
# #
# # ggplot() +
# #   geom_density(aes(climate_distribution)) +
# #   geom_density(aes(sampled_climate), linetype = "dotted") +
# #   geom_density(aes(rnorm(10000, clim_mean, clim_sd)), col = "blue") +
# #   geom_density(aes(rnorm(10000, clim_mean, clim_sd*0.55)),
# #                col = "blue", linetype = "dotted") +
# #   geom_density(aes(rlogis(10000, location = clim_mean, scale = scale)), col = "red") +
# #   geom_density(aes(rlogis(10000, location = clim_mean, scale = scale*0.625)),
# #                col = "red", linetype = "dotted")
#
# raw_ens <- ensemble_split %>% do.call("rbind", .)
# clim_ens <- replicate(nrow(raw_ens),
#                       sample(climate_distribution, num_members))
# y_obs <- raw_ens %>% dplyr::pull(OBS)
# raw_ens <- raw_ens %>% dplyr::select(starts_with("ENS"))
#
# crps_raw <- crps_ensemble(clim_ens, y_obs)

# -----------------------------------------------------------------------------

# Comment:
# should be able to do this SEASON with a case_when() but
# lubridate /dplyr interactions seemed odd
# also tried a function (could try again)


