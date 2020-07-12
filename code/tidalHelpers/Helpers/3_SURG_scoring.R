warning("You are naughty ungroup your shit")

# READ IN THE EXTENDED ENSEMBLE DATA

ensemble_data_extended <- readRDS(ens_pp_path)

# ---------------------------------------------------------------------------------

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

# ADD THE BSS

# First get the climate distribution
clim_samples <- ensemble_data_extended %>%
  dplyr::mutate(OBS_DATE = FORECAST_DATE + INIT_TIME + LEAD_TIME) %>%
  dplyr::select(OBS_DATE, SEASON, OBS) %>%
  dplyr::distinct()

# Get the seasonal quantiles
probs = seq(0.05, 0.95, 0.05)
seasonal_quantiles <- apply(season_cols, 2, quantile,
                            probs = probs, na.rm = TRUE) %>%
  as.data.frame() %>%
  setNames(names(season_cols)) %>%
  dplyr::mutate(PROBS = probs) %>%
  tidyr::pivot_longer(cols = c('SUMMER', 'WINTER'),
                      names_to = "SEASON", values_to = "THRESHOLD")

quantiles <- seasonal_quantiles %>%
  dplyr::select(THRESHOLD) %>%
  unlist() %>% unique() %>% sort()

get_brier_score = function(row, t, num_members = 50){
  # first entry of row must be obs
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

# library(verficiation); ?brier

# ---------------------------------------------------------------------------------

# SAVE OUT THE SCORES
saveRDS(ensemble_data_extended, ens_score_path)

# ---------------------------------------------------------------------------------

# BOOTSTRAP THE SCORES
ensemble_data_extended <- readRDS(ens_score_path)

days = seq(0,10)
daily_lead_times = days*24 %>%
  paste(., ":0:0", sep = "") %>%
  lubridate::hms() %>%
  lubridate::as.duration()

scores_filtered <- ensemble_data_extended %>%
  dplyr::filter(LEAD_TIME %in% daily_lead_times) %>%
  dplyr::select(tidyselect::all_of(fixed_vars),
                tidyselect::starts_with("CRPS"),
                tidyselect::starts_with("RANK"),
                tidyselect::starts_with("BS")) %>%
  tidyr::unite("FILTER_REF", fixed_vars) %>%
  dplyr::group_by(FILTER_REF)

# warning("Assume all lead times, init times and season have similar amounts of data")
# num_bootstrap_samps = scores_filtered %>%
#   pull(FILTER_REF) %>%
#   table() %>%
#   min()

mean_scores_bootstrapped <- lapply(1:num_samples, function(i){
  print(i); set.seed(i)
  mean_scores <- scores_filtered %>%
    dplyr::slice_sample(prop = 1, replace = TRUE) %>%
    dplyr::summarise_if(is.numeric, mean, na.rm = TRUE) %>%
    dplyr::ungroup()
  return(mean_scores)
}) %>%
  data.table::rbindlist() %>%
  dplyr::mutate(SUMMARY = "MEAN")

median_scores_bootstrapped <- lapply(1:num_samples, function(i){
  print(i); set.seed(i)
  median_scores <- scores_filtered %>%
    dplyr::slice_sample(prop = 1, replace = TRUE) %>%
    summarise_if(is.numeric, median, na.rm = TRUE) %>%
    ungroup()
  return(median_scores)
}) %>%
  data.table::rbindlist() %>%
  dplyr::mutate(SUMMARY = "MEDIAN")

# lower_ci_scores_bootstrapped <- lapply(1:num_samples, function(i){
#   print(i); set.seed(i)
#   lower_ci_scores <- scores_filtered %>%
#     dplyr::slice_sample(prop = 1, replace = TRUE) %>%
#     summarise_if(is.numeric, quantile, probs = 0.05, na.rm = TRUE) %>%
#     ungroup()
#   return(lower_ci_scores)
# }) %>%
#   data.table::rbindlist() %>%
#   dplyr::mutate(SUMMARY = "LOWER_CI")
#
# upper_ci_scores_bootstrapped <- lapply(1:num_samples, function(i){
#   print(i); set.seed(i)
#   upper_ci_scores <- scores_filtered %>%
#     dplyr::slice_sample(prop = 1, replace = TRUE) %>%
#     summarise_if(is.numeric, quantile, probs = 0.95, na.rm = TRUE) %>%
#     ungroup()
#   return(upper_ci_scores)
# }) %>%
#   data.table::rbindlist() %>%
#   dplyr::mutate(SUMMARY = "UPPER_CI")

scores_bootstrapped <- bind_rows(mean_scores_bootstrapped,
                                 median_scores_bootstrapped#,
                                 # lower_ci_scores_bootstrapped,
                                 # upper_ci_scores_bootstrapped
                                 ) %>%
  tidyr::separate("FILTER_REF", fixed_vars, sep = "_") %>%
  dplyr::mutate(INIT_TIME = lubridate::as.duration(INIT_TIME),
                LEAD_TIME = parse_number(LEAD_TIME)) %>%
  dplyr::mutate(LEAD_TIME = lubridate::as.duration(LEAD_TIME))

saveRDS(scores_bootstrapped, ens_score_bootsrap_path)

# ---------------------------------------------------------------------------------
#
# surge_dir = "/Users/katesaunders/Documents/No_Back_Up_Data/SURGE/"
# num_members = 50
# na_value = 99.99
# location_ref = "LAUW"
# element_ref = "SURG"
# print_my_warnings = FALSE
# overwrite_saved = FALSE
# # selected_season = "WINTER"
# # combine_winter = c(2015,2017)
# # omit_years = c(2010,2019)
# num_samples = 500
# fixed_vars = c("INIT_TIME", "LEAD_TIME", "SEASON")
#
# ens_pp_path = paste(getwd(), "/data/", location_ref, "_pp_ens.rds", sep = "")
# ens_score_path = paste(getwd(), "/data/", location_ref, "_score_ens.rds", sep = "")
# ens_score_bootsrap_path = paste(getwd(), "/data/", location_ref, "_score_bootstrap.rds", sep = "")
#
# # -----------------------------------------------------------------------------
#
# # PACKAGES AND FUNCTIONS
# library(tidyverse)
# library(moments)
#
# # -----------------------------------------------------------------------------
