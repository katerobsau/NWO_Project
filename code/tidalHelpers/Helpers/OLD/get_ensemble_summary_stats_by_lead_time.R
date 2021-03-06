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

# Save out the mean and sd of the ensemble for ngr fitting
# saveRDS(ensemble_summary, file = "../../data/ensemble_summary_stats.rds")
ensemble_summary <- readRDS(file = "../../data/ensemble_summary_stats.rds")
ensemble_summary <- readRDS(file = "data/ensemble_summary_stats.rds")

# -----------------------------------------------------------------------------

# Deal with seasonality
season_data <- ensemble_summary %>%
  mutate(date = str_sub(date, 1, 8) %>% lubridate::as_date()) %>%
  mutate(year = lubridate::year(date), month = lubridate::month(date),
         day = lubridate::day(date)) %>%
  filter(month >= 10 | month <= 4) %>%
  filter(!(month == 10 & day <= 15) & !(month == 4 & day >= 15)) %>%
  mutate(season = ifelse(month >= 10, year, year - 1))

# -----------------------------------------------------------------------------

# # deal with NAs (in NGR_fit function)
# season_data <- season_data %>%
#   filter(!is.na(sur))
#
# if(any(is.na(season_data$mean_wsur))){
#   min_t = season_data %>%
#     filter(is.na(mean_wsur)) %>%
#     pull(t) %>%
#     min()
#   if(min_t < 240){
#     stop("Error in ensemble summaries, should not have NA values")
#   }else{
#     season_data <- season_data %>%
#       filter(!is.na(mean_wsur))
#   }
# }

# -----------------------------------------------------------------------------

seasons = season_data$season %>% unique()
lead_times = seq(1, 10, length.out = 19)*24
cv_scenarios <- expand.grid(season = seasons, lead_time = lead_times) %>% as.data.frame()

wrapper_ngr_fits <- function(i, season_data, scenarios){

  season_rm = scenarios$season[i]
  lead_time = scenarios$lead_time[i]

  train_data <- season_data %>%
    dplyr::filter(season != season_rm) %>%
    dplyr::filter(!is.na(mean_wsur) & !is.na(sur) & !is.na(sd_wsur)) %>%
    dplyr::filter(t == lead_time)
  if(nrow(train_data) == 0)
    stop("Error in filtering for training data")

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

  season_rm = scenarios$season[i]
  lead_time = scenarios$lead_time[i]
  m_fit = ngr_fits[[i]]

  test_data <-  season_data %>%
    dplyr::filter(season == season_rm) %>%
    dplyr::filter(!is.na(mean_wsur) & !is.na(sur) & !is.na(sd_wsur)) %>%
    dplyr::filter(t == lead_time)

  if(nrow(test_data) == 0)
    stop("Error in filtering for test data")

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

  season_rm = scenarios$season[i]
  lead_time = scenarios$lead_time[i]
  par = ngr_predictions[i]

  y_obs <- season_data %>%
    dplyr::filter(season == season_rm) %>%
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


rank_df = NULL
for(lead_time in lead_times){
  i = which(cv_scenarios$lead_time == lead_time)
  combined_years = predictive_ecdf_obs[i] %>% unlist()
  df_lead_time = data.frame(
    lead_time = rep(lead_time, length(combined_years)),
    obs = combined_years)
  rank_df = rbind(rank_df, df_lead_time)
}

bins = 20
p = 1/bins
t = rank_df$lead_time[1]
n = nrow(rank_df %>% filter(lead_time == t))
var_hgt = pbinom(c(0.05, 0.95), size = n, prob = p)
ggplot() +
  geom_hline(data= NULL,
             yintercept = qbinom(c(0.05, 0.5, 0.95), n, p),
             linetype = "dotted") +
  geom_histogram(data = rank_df, aes(obs),
                 binwidth = 1/bins,
                 fill = "gray", alpha = 0.75) +
  facet_wrap(~lead_time) +
  ggtitle("Rank Histograms") +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_blank())

print("Would like to add pvalues for chi-sq test to these")

## ----------------------------------------------------------

library(Rcpp)
# sourceCpp("Helpers/crps_ensemble.cpp")
sourceCpp("code/tidalHelpers/Helpers/crps_ensemble.cpp")

wrapper_ensemble_members <- function(i, scenarios,
                                     ngr_predictions,
                                     num_members = init_var$num_members){

  season_rm = scenarios$season[i]
  lead_time = scenarios$lead_time[i]
  par = ngr_predictions[i]

  y_obs <- season_data %>%
    dplyr::filter(season == season_rm) %>%
    dplyr::filter(!is.na(mean_wsur) & !is.na(sur) & !is.na(sd_wsur)) %>%
    dplyr::filter(t == lead_time) %>%
    pull(sur)

  sampled_members = mapply(rnorm,
                           mean = par[[1]]$mu,
                           sd = par[[1]]$sigma,
                           MoreArgs = list(n = num_members))

  return(rbind(y_obs, sampled_members))

}

pars = lapply(1:nrow(cv_scenarios),
              function(i, ngr_predictions, cv_scenarios){
                l = ngr_predictions[[i]]
                mu = l$mu
                sigma = l$sigma
                season_rm = cv_scenarios$season[i]
                lead_time = cv_scenarios$lead_time[i]
                filtered_data <- season_data %>%
                  dplyr::filter(season == season_rm) %>%
                  dplyr::filter(!is.na(mean_wsur) & !is.na(sur) & !is.na(sd_wsur)) %>%
                  dplyr::filter(t == lead_time) #
                y_obs = filtered_data %>% pull(sur)
                dates = filtered_data %>% pull(date)

                df  = data.frame(obs = y_obs, mu , sigma,
                                 season = rep(season_rm, length(mu)),
                                 lead_time = rep(lead_time, length(mu)),
                                 dates = dates)

                return(df)
              }, ngr_predictions = ngr_predictions,
              cv_scenarios = cv_scenarios) %>%
  bind_rows()

# saveRDS(pars, file = "data/surge_pars.rds")

ensemble_members <- lapply(1:nrow(cv_scenarios),
                              wrapper_ensemble_members,
                              scenarios = cv_scenarios,
                              ngr_predictions = ngr_predictions) %>%
                      as.data.frame() %>% t() %>% as.data.frame()
names(ensemble_members) = c("obs",
      paste0("fc", formatC(1:50, width = 3, flag = "0")))

crps_summary = NULL
for(t in lead_times){
  crps_data = cbind(pars,
                  ensemble_members %>% dplyr::select(-obs)) %>%
    dplyr::filter(lead_time == t)
  # dim(crps_data)

  ens_members = crps_data %>%
    dplyr::select(matches("fc"))
  observation = crps_data %>% pull(obs)

  crps_val <- crps_ensemble(as.matrix(ens_members), as.numeric(observation))
  crps_summary <- rbind(crps_summary, summary(crps_val))
}

crps_summary <- crps_summary %>%
  as.data.frame() %>%
  dplyr::mutate(lead_time = lead_times)

ggplot(crps_summary) +
  # geom_ribbon(aes(x = lead_time , ymin = `Min.`, ymax = `Max.`),
  #             alpha = 0.1, fill = "blue") +
  geom_ribbon(aes(x = lead_time , ymin = `1st Qu.`, ymax = `3rd Qu.`),
              alpha = 0.2, fill = "blue") +
  geom_point(aes(x = lead_time, y = Median), col = "blue") +
  geom_point(aes(x = lead_time, y = Mean), col = "blue", shape = 3) +
  theme_bw() +
  ylim(0,0.25)

# -----------------------------------------------------------------------------

time1 = Sys.time()
print("Note to self, didn't account for NAs when I did my std dev.")
ensemble_data_filtered <- NULL
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
    dplyr::mutate(wsur = ifelse(wsur >= na_value, NA_real_, wsur)) %>%
    dplyr::filter(t %in% lead_times) %>%
    dplyr::mutate(date = date_val)

  ensemble_data_filtered = ensemble_data  %>%
    dplyr::select(sur, wsur, date, t, member) %>%
    rbind(ensemble_data_filtered, .)

}
time2 = Sys.time()

# Save out the mean and sd of the ensemble for ngr fitting
# saveRDS(ensemble_data_filtered,
#         file = "../../data/ensemble_filtered.rds")
#
# ensemble_data_filtered <- readRDS(file = "../../data/ensemble_filtered.rds")
ensemble_data_filtered <- readRDS(file = "data/ensemble_filtered.rds")
ensemble_winter <- ensemble_data_filtered %>%
  mutate(date = str_sub(date, 1, 8) %>% lubridate::as_date()) %>%
  mutate(year = lubridate::year(date), month = lubridate::month(date),
         day = lubridate::day(date)) %>%
  filter(month >= 10 | month <= 4) %>%
  filter(!(month == 10 & day <= 15) & !(month == 4 & day >= 15)) %>%
  mutate(season = ifelse(month >= 10, year, year - 1))

crps_summary_raw = NULL
for(lead_time in lead_times){

  ens_members_raw <- ensemble_winter %>%
    dplyr::filter(t == lead_time) %>%
    pivot_wider(names_from = member, values_from = wsur) %>%
    dplyr::select(matches("fc"))

  observation_raw = ensemble_winter %>%
    dplyr::filter(t == lead_time) %>%
    dplyr::select(date, sur) %>%
    distinct() %>%
    pull(sur)

  crps_raw <- crps_ensemble(as.matrix(ens_members_raw),
                        as.numeric(observation_raw))

  crps_summary_raw <- rbind(crps_summary_raw,
                            summary(crps_raw))
}

crps_summary_raw <- crps_summary_raw %>%
  as.data.frame() %>%
  dplyr::mutate(lead_time = lead_times)

ggplot() +
  # geom_ribbon(aes(x = lead_time , ymin = `Min.`, ymax = `Max.`),
  #             alpha = 0.1, fill = "blue") +
  geom_ribbon(data = crps_summary, aes(x = lead_time , ymin = `1st Qu.`, ymax = `3rd Qu.`),
              alpha = 0.2, fill = "blue") +
  geom_point(data = crps_summary,aes(x = lead_time, y = Median), col = "blue") +
  geom_point(data = crps_summary,aes(x = lead_time, y = Mean), col = "blue", shape = 3) +
  geom_ribbon(data = crps_summary_raw, aes(x = lead_time , ymin = `1st Qu.`, ymax = `3rd Qu.`),
              alpha = 0.2, fill = "red") +
  geom_point(data = crps_summary_raw, aes(x = lead_time, y = Median), col = "red") +
  geom_point(data = crps_summary_raw, aes(x = lead_time, y = Mean), col = "red", shape = 3) +
  theme_bw() +
  ylim(0,0.25)

# -----------------------------------------------------------------------------

# got a bit of a mess with the members
drawn_members <- ensemble_members %>%
  dplyr::bind_cols(pars) %>%
  mutate(t = lead_time, date = dates)

# get template
ensemble_wider <- ensemble_data_filtered %>%
  mutate(date = str_sub(date, 1, 8) %>% lubridate::as_date()) %>%
  filter(date %in% (drawn_members$dates %>% unique())) %>%
  # filter(date == "2012-10-28" & t == 24) %>%
  select(-sur) %>%
  pivot_wider(names_from = c("member"), values_from = "wsur")

ensemble_template <- apply(ensemble_wider %>% dplyr::select(-date, -t), 1, order) %>%
  t() %>%
  as.data.frame() %>%
  mutate(date = ensemble_wider$date) %>%
  mutate(t = ensemble_wider$t)

ecc_data <- full_join(drawn_members, ensemble_template,
                      by = c("t", "date")) %>%
  filter(!is.na(fc001) & !is.na(V1))

reordered <- lapply(1:nrow(ecc_data), function(i, ecc_data){
  uni_pp = ecc_data %>%
    select(starts_with("fc")) %>%
    slice(i) %>%
    as.numeric() %>%
    sort()
  template = ecc_data %>%
    select(starts_with("V")) %>%
    slice(i) %>%
    as.numeric()
  reorder = uni_pp[template]
  return(reorder)
}, ecc_data = ecc_data)

reordered_df = as.data.frame(reordered) %>%
  t() %>%
  as.data.frame()
names(reordered_df) = names(ensemble_members) %>% setdiff("obs")
reordered_df = reordered_df %>%
  mutate(date = ecc_data$date, t = ecc_data$t)

days = lubridate::as_date("2012-10-28") + 1:10

# day_obs <- drawn_members %>%
#   filter(date == "2015-11-03") %>%
#   select(t, obs)

day_ecc <- reordered_df %>%
  filter(date == "2012-10-28") %>%
  pivot_longer(cols = starts_with("fc"), names_to = "member", values_to = "wsur") %>%
  mutate(dep_type = "ecc") %>%
  select(-date)

day_raw <- ensemble_data_filtered %>%
  filter(date == "2012102800") %>%
  mutate(dep_type = "raw") %>%
  select(-sur, -date)

plot_data = bind_rows(day_ecc, day_raw)
ggplot(data = plot_data) +  #%>%
         #filter(member %in% paste0("fc02",1:9))) +
  geom_path(aes(x = t, y = wsur, group = member, col = member),
            alpha = 0.5) +
  # geom_path(data = day_obs, aes(x = t, y = obs)) +
  facet_wrap(~dep_type) +
  theme_bw() +
  theme(legend.position = "none")

# -----------------------------------------------------------------------------

