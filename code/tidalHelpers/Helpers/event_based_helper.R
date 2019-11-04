# cluster code
# print("Build this into the package!!!")
# print("Only want the functions in here")
source("../source/surge_exploratory_analyis.R")

na_value = 99
init_var = utils_init()
main_data_dir = "/Users/katesaunders/Documents/No_Back_Up_Data/ENS/"
dates_vec = list.files(main_data_dir)
member_ref = get_ensemble_ref(init_var$num_members)
risk_level = -0.42

# -----------------------------------------------------------------------------
num_days = 100 #length(dates_vec)
peak_surge_ens <- peak_surge_obs <- vector("list", num_days)
peak_level_ens <- peak_level_obs <- vector("list", num_days)
duration_surge_ens <- duration_surge_obs <- vector("list", num_days)
duration_risk_ens <- duration_risk_obs <- vector("list", num_days)

for(i in 1:100){ # num_days

  date_val = dates_vec[i]  #"2011112900" # #"2012010200" #
  data_dir = paste(main_data_dir, date_val, "/", sep = "")

  # Get Ensemble data
  ensemble_data = combine_ensemble_data(date_val, lead_time = "00000",
                                        member_ref = member_ref,
                                        data_dir = data_dir)
  if(is.null(ensemble_data)) next

  # Deal with NA data
  ensemble_data = ensemble_data %>%
    NA_check()

  # Get Observed data
  obs_data <- ensemble_data %>%
    select(t, harm, sur, obs) %>%
    distinct()

  # "Question to self: Do I want to remove NA obs?" &
  # "How does this effect my analysis")

  ### EVENT BASED STATISTICS

  # peak surge levels (time and height)
  peak_surge_obs[[i]] <- get_peak(df = obs_data, type = "obs",
                                  var_name = "sur") %>%
                          left_join(ensemble_data) %>%
                          mutate(date = date_val)
  peak_surge_ens[[i]] <- get_peak(df = ensemble_data, type = "ens",
                                  var_name = "wsur") %>%
                          mutate(date = date_val)
  # peak water levels (time and height)
  peak_level_obs[[i]] <- get_peak(df = obs_data, type = "obs",
                                  var_name = "obs") %>%
                          left_join(ensemble_data) %>%
                          mutate(date = date_val)
  peak_level_ens[[i]] <- get_peak(df = ensemble_data, type = "ens",
                                  var_name = "wtot") %>%
                          mutate(date = date_val)

  # longest surge period of positive surge
  duration_surge_ens[[i]] <- get_duration(df = ensemble_data, type = "ens",
                                     threshold_value = 0, var_name = "wsur",
                                     var_summary = "max") %>%
                          mutate(date = date_val)
duration_surge_obs[[i]] <- get_duration(df = obs_data, type = "obs",
                                     threshold_value = 0, var_name = "sur",
                                     var_summary = "max") %>%
                            left_join(ensemble_data) %>%
                          mutate(date = date_val)

  # period of risk
  duration_risk_ens[[i]] <- get_duration(df = ensemble_data, type = "ens",
                                     threshold_value = risk_level,
                                     var_name = "wtot",
                                     var_summary = "total") %>%
                            mutate(date = date_val)

  duration_risk_obs[[i]] <- get_duration(df = obs_data, type = "obs",
                                     threshold_value = risk_level,
                                     var_name = "obs",
                                     var_summary = "total") %>%
                            mutate(date = date_val)

}

# look at the summaries
peak_sur_summary = peak_sur %>%
  mutate(var = if_else(member == "obs", "obs", "ens")) %>%
  filter(var != "obs") %>%
  group_by(date, var) %>%
  summarise(mean_wsur = mean(wsur),
            max_wsur = max(wsur)) %>%
  left_join(peak_sur %>% filter(member == "obs") %>% select(date, sur) %>% distinct())

ggplot(peak_sur_summary) +
  # geom_point(aes(x = sur, y = max_wsur), alpha = 0.2, col = "purple") +
  geom_point(aes(x = sur, y = mean_wsur), alpha = 0.2, col = "orange") +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw()

peak_lvl_summary = peak_lvl %>%
  group_by(date, var) %>%
  summarise(mean_obs = mean(obs),
            mean_wtot = mean(wtot),
            mean_wtoc = mean(wtoc)) %>%
  ungroup()

ggplot(peak_lvl_summary) +
  geom_point(aes(x = mean_obs, y = mean_wtot), col = "blue", alpha  = 0.2) +
  # geom_point(aes(x = mean_obs, y = mean_wtoc), col = "red", alpha  = 0.2) +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw()

sur_clusters_summary = sur_clusters %>%
  filter(member != "obs") %>%
  group_by(date) %>%
  summarise(mean_len = mean(lengths)) %>%
  left_join(sur_clusters %>%
              filter(member == "obs") %>%
              select(date, lengths)) %>%
  mutate(obs_len = lengths)

ggplot(sur_clusters_summary) +
  geom_point(aes(x = obs_len, y = mean_len)) +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw()

risk_period_summary = risk_period %>%
  group_by(date, var) %>%
  summarise(mean_period = mean(period)) %>%
  pivot_wider(names_from = var, values_from = mean_period)

ggplot(risk_period_summary) +
  geom_point(aes(x = obs, y = wtot), col = "blue") +
  geom_point(aes(x = obs, y = wtoc), col = "red") +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw()

#
# ### ---------------------------------------------------------------------------
#
# # Recuded size data sets
#   # Reduce to hourly observations
#   # Summary used to max water level not max surge
#   reduced_obs <- obs_data %>%
#     mutate(hour = floor(t)) %>%
#     group_by(hour) %>%
#     summarise(obs = max(obs)) %>%
#     left_join(obs_data)
#   dim(reduced_obs)
#   print("Careful possible to have multiple maxima in a given hour.")
#   # Due to harm1 + sur1 = harm2 + sur2 = max_obs
#
#   # Reduce to hourly ensemble data
#   grouped_ensemble <- ensemble_data %>%
#     mutate(hour = floor(t)) %>%
#     group_by(hour, member)
#
#   ensemble_tides <- ensemble_data %>%
#     select(t, wsur, htid, wtid) %>%
#     distinct() %>%
#     mutate(hour = floor(t))
#
#   wsur_reduced <- grouped_ensemble %>%
#     summarise(wsur = max(wsur)) %>%
#     left_join(ensemble_tides)
#
#   # Couldn't decide which one to maximise so combined all of them
#   reduced_ensemble = wsur_reduced %>%
#     mutate(wtot = wsur + htid, wtoc = wsur + wtid) %>%
#     left_join(obs_data)
#   dim(reduced_ensemble_data)
#
# ### ---------------------------------------------------------------------------
#
#   # Plot the trajectory
#   ggplot(data = reduced_ensemble) +
#     geom_line(aes(x = hour, y = wsur, group = member), col = "gray") +
#     # geom_line(aes(x = hour, y = harm), col = "black", size = 0.3) +
#     # geom_line(aes(x = hour, y = obs), col = "red", size = 0.3) +
#     geom_line(aes(x = hour, y = sur), col = "blue", size = 0.3) +
#     # geom_point(aes(x = hour, y = sur), col = "blue", size = 0.3) +
#     geom_hline(yintercept = 0, col = "gray", linetype = "dashed") +
#     geom_hline(yintercept = -0.42, linetype = "dashed") +
#     # geom_point(aes(x = hour, y = wtot), col = "gray") +
#     theme_bw() +
#     ylim(c(-4,4))
#
# ### ---------------------------------------------------------------------------


### --------------------------------------------------------------------------------

# for each of the g functions I want to pull out the data from the raw ensemble
# ie. the mean value
# Then I want to calculate the BSS so Sum (F - O)^2
# Could do this for different lead times (Not just the longest one)
# loop over my data
