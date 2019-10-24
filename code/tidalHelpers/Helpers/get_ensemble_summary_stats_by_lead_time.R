# cluster code
# print("Build this into the package!!!")
# print("Only want the functions in here")
# source("../source/surge_exploratory_analyis.R")
na_value = 99
init_var = utils_init()
main_data_dir = "/Users/katesaunders/Documents/No_Back_Up_Data/ENS/"
dates_vec = list.files(main_data_dir)
member_ref = get_ensemble_ref(init_var$num_members)
risk_level = -0.42

# -----------------------------------------------------------------------------
ensemble_summary = NULL
time1 = Sys.time()
print("Note to self, didn't account for NAs when I did my std dev.")
for(date_val in dates_vec){

  print(date_val)
  # date_val = dates_vec[i]  #"2011112900" #"2012010200" #
  data_dir = paste(main_data_dir, date_val, "/", sep = "")

  # Get Ensemble data
  ensemble_data = combine_ensemble_data(date_val, lead_time = "00000",
                                        member_ref = member_ref,
                                        data_dir = data_dir)

  if(is.null(ensemble_data)) next

  ensemble_data = ensemble_data %>%
    dplyr::select(t, harm, sur, obs, htid, wtid, wsur, wtot, wtoc, wsur, member) %>%
    dplyr::mutate(harm = ifelse(harm >= na_value, NA_real_, harm)) %>%
    dplyr::mutate(sur = ifelse(sur >= na_value, NA_real_, sur)) %>%
    dplyr::mutate(obs = ifelse(obs >= na_value, NA_real_, obs)) %>%
    dplyr::mutate(wtot = ifelse(wtot >= na_value, NA_real_, wtot)) %>%
    dplyr::mutate(wtoc = ifelse(wtoc >= na_value, NA_real_, wtoc)) %>%
    dplyr::mutate(wsur = ifelse(wsur >= na_value, NA_real_, wsur))

  dim(ensemble_data)

  mean_wsur <- ensemble_data %>%
    dplyr::group_by(t) %>%
    dplyr::summarise(mean_wsur = mean(wsur)) %>%
    dplyr::ungroup()

  sd_wsur <- ensemble_data %>%
    dplyr::left_join(mean_wsur) %>%
    dplyr::mutate(diff = (wsur - mean_wsur)^2) %>%
    dplyr::group_by(t) %>%
    dplyr::summarise(sd_wsur = 1/init_var$num_members * sum(diff)) %>%
    dplyr::ungroup()

  forecast_summary <- dplyr::left_join(mean_wsur, sd_wsur, by = "t") %>%
    dplyr::mutate(date = date_val) %>%
    dplyr::left_join(ensemble_data %>%
                       dplyr::select(t, sur) %>%
                       dplyr::distinct())

  ensemble_summary <- rbind(ensemble_summary, forecast_summary)

}
time2 = Sys.time()
saveRDS(ensemble_summary, file = "../../data/ensemble_summary_stats.rds")

# -----------------------------------------------------------------------------

# Deal with seasonality
season_data <- ensemble_summary %>%
  mutate(date = str_sub(date, 1, 8) %>% lubridate::as_date()) %>%
  filter(lubridate::month(date) > 10 | lubridate::month(date) < 4)
print("should be mid to mid but whatever")
print("fix year omitted (ie should be by season not year")

# -----------------------------------------------------------------------------

years = c(2011:2015, 2018)
print("Automate me")
lead_times = seq(1, 10, length.out = 19)*24
cv_scenarios <- expand.grid(year = years, lead_time = lead_times) %>% as.data.frame()

wrapper_ngr_fits <- function(i, season_data, scenarios){

  year = scenarios$year[i]
  lead_time = scenarios$lead_time[i]

  train_data <- season_data %>%
    dplyr::filter(lubridate::year(date) != year) %>%
    dplyr::filter(!is.na(mean_wsur) & !is.na(sur) & !is.na(sd_wsur)) %>%
    dplyr::filter(t == lead_time)

  y = train_data$sur
  cov_matrix = train_data %>%
    dplyr::select(mean_wsur, sd_wsur)

  NGR_fit <- fit_NGR(y = y, X = cov_matrix)

  return(NGR_fit)
}

ngr_fits <- lapply(1:nrow(cv_scenarios),
                   wrapper_ngr_fits,
                   season_data = season_data,
                   scenarios = cv_scenarios)

## ----------------------------------------------------------

wrapper_ngr_predict <- function(i, season_data, scenarios,
                                ngr_fits){

  year = scenarios$year[i]
  lead_time = scenarios$lead_time[i]
  m_fit = ngr_fits[[i]]

  test_data <- season_data %>%
    dplyr::filter(lubridate::year(date) == year) %>%
    dplyr::filter(!is.na(mean_wsur) & !is.na(sur) & !is.na(sd_wsur)) %>%
    dplyr::filter(t == lead_time)

  y_predict = test_data$sur
  cov_matrix_predict = test_data %>%
    dplyr::select(mean_wsur, sd_wsur)

  NGR_predict <- predict_NGR(fit = m_fit,
                           X_test = cov_matrix_predict)

}

ngr_predictions <- lapply(1:nrow(cv_scenarios),
                   wrapper_ngr_predict,
                   season_data = season_data,
                   scenarios = cv_scenarios,
                   ngr_fits = ngr_fits)

## ----------------------------------------------------------

wrapper_predictive_distribution <- function(i, scenarios,
                           season_data, ngr_predictions){

  year = scenarios$year[i]
  lead_time = scenarios$lead_time[i]
  par = ngr_predictions[i]

  y_obs <- season_data %>%
    dplyr::filter(lubridate::year(date) == year) %>%
    dplyr::filter(!is.na(mean_wsur) & !is.na(sur) & !is.na(sd_wsur)) %>%
    dplyr::filter(t == lead_time) %>%
    pull(sur)

  F_y_obs = mapply(pnorm, y_obs, par[[1]]$mu, par[[1]]$sigma)

  return(F_y_obs)

}

predictive_ecdf_obs <- lapply(1:nrow(cv_scenarios),
                          wrapper_predictive_distribution,
                          scenarios = cv_scenarios,
                          season_data = season_data,
                          ngr_predictions = ngr_predictions)


df = NULL
for(lead_time in lead_times){
  i = which(scenarios$lead_time == lead_time)
  combined_years =predictive_ecdf_obs[i] %>% unlist()
  df_lead_time = data.frame(
    lead_time = rep(lead_time, length(combined_years)),
    obs = combined_years)
  df = rbind(df, df_lead_time)
}

# bins = 20
# hgt = 1/bins * length(combined_years)
ggplot(data = df) +
  geom_histogram(aes(obs), binwidth = 1/bins,
                 fill = "gray", alpha = 0.75) +
  # geom_hline(yintercept = hgt) +
  facet_wrap(~lead_time) +
  ggtitle("Rank Histograms") +
  theme_bw()


## ----------------------------------------------------------

library(Rcpp)
sourceCpp("Helpers/crps_ensemble.cpp")

wrapper_ensemble_members <- function(i, scenarios,
                                     ngr_predictions,
                                     num_members = init_var$num_members){

  year = scenarios$year[i]
  lead_time = scenarios$lead_time[i]
  par = ngr_predictions[i]

  y_obs <- season_data %>%
    dplyr::filter(lubridate::year(date) == year) %>%
    dplyr::filter(!is.na(mean_wsur) & !is.na(sur) & !is.na(sd_wsur)) %>%
    dplyr::filter(t == lead_time) %>%
    pull(sur)

  sampled_members = mapply(rnorm,
                           mean = par[[1]]$mu,
                           sd = par[[1]]$sigma,
                           MoreArgs = list(n = num_members))

  return(sampled_members)

}

ensemble_members <- lapply(1:nrow(cv_scenarios),
                              wrapper_ensemble_members,
                              scenarios = cv_scenarios,
                              ngr_predictions = ngr_predictions) %>%
                      as.data.frame()

wrapper_observations <- function(i, scenarios, season_data){

  year = scenarios$year[i]
  lead_time = scenarios$lead_time[i]

  y_obs <- season_data %>%
    dplyr::filter(lubridate::year(date) == year) %>%
    dplyr::filter(!is.na(mean_wsur) & !is.na(sur) & !is.na(sd_wsur)) %>%
    dplyr::filter(t == lead_time) %>%
    pull(sur)

  return(y_obs)

}

observations <- lapply(1:nrow(cv_scenarios),
                           wrapper_observations,
                           scenarios = cv_scenarios,
                           season_data) %>%
  as.data.frame()

lead_time = 24*6
i = which(scenarios$lead_time == lead_time)
ens_members = ensemble_members[,i]
observation = observations[i]

crps <- crps_ensemble(as.matrix(ens_members), as.numeric(observation))
