# Fit the GAMLSS

# -----------------------------------------------------------------------------

# packages

library(tidyverse)
library(gamlss)
library(patchwork)
library(Rcpp)
sourceCpp("code/tidalHelpers/Helpers/crps_ensemble.cpp")

# -----------------------------------------------------------------------------

# read in the data
data_dir = "~/Documents/No_Back_Up_Data/Leeuwarden_Data/"
combined_data <- readRDS(paste0(data_dir, "rainfall_forecast_data.rds"))

# -----------------------------------------------------------------------------

# Get ensemble only
member_data <- combined_data %>%
  dplyr::select(starts_with("ENS"))
num_members = ncol(member_data)

# Get predictors
nu_empirical = apply(member_data, 1, function(row){sum(row == 0)})
mean = apply(member_data, 1, function(row){row[which(row > 0)] %>% mean()})
mean[nu_empirical == num_members] = NA
sd = apply(member_data, 1, function(row){row[which(row > 0)] %>% sd()})
sd[nu_empirical == num_members] = NA
mean0 = apply(member_data, 1,mean)
sd0 = apply(member_data, 1, sd)
# Note: I Compared with and without zeroes for predictand - not a huge difference

# Add predictors from the ensemble and necessary filtering variables
predictor_data <- combined_data %>%
  mutate(nu_empirical, mean, sd, mean0, sd0) %>%
  mutate(Season = lubridate::month(Forecast_Date) >=3 &
           lubridate::month(Forecast_Date) <= 8,
        Group = lubridate::year(Forecast_Date) +
           -1*(lubridate::month(Forecast_Date) <=2))
warning("Hard coded end of Winter")
#
# ggplot(data= predictor_data %>% filter(Season == 0)) +
#   geom_hex(aes(x = sd, y = nu_empirical))

# -----------------------------------------------------------------------------

# iterative over these
lead_time_vec = predictor_data %>%
  pull(lead_time) %>%
  lubridate::hour() %>%
  unique() %>%
  paste(., "0", "0", sep = ":") %>%
  lubridate::hms()

init_vec = predictor_data %>%
  pull(Init) %>%
  lubridate::hour() %>%
  unique() %>%
  paste(., "0", "0", sep = ":") %>%
  lubridate::hms()

print("Something very odd, about taking unique() over a lubridate::hms() object")

season_vec = c(0,1)

all_iterations <- expand.grid(season = season_vec,
            Init = init_vec,
            lead_time = lead_time_vec)

# warning("Reduced the size of the iterations to get some quick results")
iterations <- all_iterations %>%
  # filter(season == 0,
  #        Init ==  lubridate::hms("0:0:0"),
  #        (lubridate::hour(lead_time)%%24 == 0)) %>%
  split(., 1:nrow(.))

# -----------------------------------------------------------------------------

lead_time_list <- lapply(iterations, function(l, predictor_data){
  # format the ens_data for fitting the gamlss
  lead_time_data <- predictor_data %>%
    filter(lead_time == l$lead_time,
           Init == l$Init,
           Season == l$season)
  return(lead_time_data)
}, predictor_data)

# # format the ens_data for fitting the gamlss
# lead_time_data <- predictor_data %>%
#   filter(lead_time == lead_time_val,
#          Init == init_val,
#          Season == season_val)

cv_list <- lapply(lead_time_list, function(lead_time_data){

  fit_data = lead_time_data %>%
    dplyr::select(RH6, mean0, sd0, nu_empirical, Group)

  groups = fit_data %>% pull(Group)
  fit_data = fit_data %>% dplyr::select(-Group)

  t1 <- Sys.time()
  cv_data <- tryCatch(CV_ZAGA(fit_data = fit_data, groups = groups),
                      error = function(e) return(NA))
  t2 <- Sys.time()
  print(t2 - t1)
  warning("Need to think about the forward selection order")

  restart <<- lead_time_data
  return(cv_data)

})

# read in the data
# saveRDS(cv_list, paste0(data_dir, "rainfall_gamlss_fits.rds"))
# saveRDS(lead_time_list, paste0(data_dir, "rainfall_lead_times.rds"))

lead_time_list <- readRDS(paste0(data_dir, "rainfall_lead_times.rds"))
cv_list <- readRDS(paste0(data_dir, "rainfall_gamlss_fits.rds"))

# -----------------------------------------------------------------------------

# Scoring
score_summary <- 1:length(cv_list) %>% as.list() %>%
  lapply(function(i){

  cv_data = cv_list[[i]]
  lead_time_data = lead_time_list[[i]]

  # get obs
  y_obs <- lead_time_data %>% pull(RH6)

  # simulate an ensemble
  pars = cv_data$results
  sim_ensemble <- mapply(rZAGA, mu = pars$mu,
                       sigma = pars$sigma,
                       nu = pars$nu,
                       USE.NAMES = TRUE,
                       MoreArgs = list(n = num_members)) %>% t()

  # get crps for emos
  crps_emos <- crps_ensemble(ens = sim_ensemble, obs = y_obs)

  # get raw ensemble
  raw_ensemble = lead_time_data %>%
    dplyr::select(starts_with("ENS")) %>%
    as.matrix()

  # get crps for emos
  crps_raw <- crps_ensemble(ens = raw_ensemble, obs = y_obs)

  # combine
  crps_data <- data.frame(crps_raw, crps_emos, group = lead_time_data$Group)

  # Get the best fits
  group_summaries <- crps_data %>%
    group_by(group) %>%
    summarise(mean = mean(crps_emos), median = median(crps_emos)) %>%
    ungroup()

  best_group <- group_summaries %>%
    slice(which.min(median)) %>%
    pull(group)

  # Pit histograms
  raw_rank <- apply(cbind(y_obs, raw_ensemble), 1,
                    rank, ties.method = "random")[1,]

  emos_rank <- apply(cbind(y_obs, sim_ensemble), 1,
                     rank, ties.method = "random")[1,]

  return(list(crps = crps_data, crps_summary = group_summaries,
              emos_rank = emos_rank, raw_rank = raw_rank))

})

saveRDS(score_summary, paste0(data_dir, "rainfall_score_summary.rds"))
score_summary <- readRDS(paste0(data_dir, "rainfall_score_summary.rds"))

# -----------------------------------------------------------------------------

group_summaries <- lapply(score_summary, function(l){l$crps_summary})

crps_data_all <- 1:length(score_summary) %>%
  lapply(function(i){
    l =score_summary[[i]]
    df = l$crps
    return(df)
  }) %>%
  do.call("rbind", .)
  as.data.frame() %>%
  setNames(lt_names) %>%
  pivot_longer(cols = 1:2, names_to = "lead_time", values_to = "CRPS")

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

# # Some models did not fit!!
# # Check the presence of outliers doesn't influence this
# col_sd <- apply(pars, 2, sd)
# col_median <- apply(pars, 2, median)
# outlier_criterion <- rep(c(col_median + col_sd*3), times = nrow(pars)) %>%
#   matrix(ncol = 3, byrow = TRUE)
# rm_rows <- which(pars[,1] > outlier_criterion[,1]) %>% unique()
# best_group_no_outliers <- crps_data[-rm_rows, ] %>%
#   group_by(group) %>%
#   summarise(mean = mean(crps_emos), median = median(crps_emos)) %>%
#   ungroup()

# -----------------------------------------------------------------------------
#
# # plot crps scores
# crps_plot <- ggplot(crps_data) +
#   geom_point(aes(x = crps_raw, y = crps_emos)) +
#   geom_abline(slope = 1, intercept = 0) +
#   coord_fixed() +
#   ylim(range(crps_raw))
# crps_plot
#
# library(plotly)
# ggplotly(crps_plot)
#
# sum(crps_raw < crps_emos)/length(crps_raw)

# -----------------------------------------------------------------------------



