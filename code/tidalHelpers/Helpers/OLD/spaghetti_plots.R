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
peak_sur = NULL
peak_lvl = NULL
sur_clusters = NULL
risk_period = NULL

for(date_val in dates_vec){

  date_val = dates_vec[i]  #"2011112900" # #"2012010200" #
  data_dir = paste(main_data_dir, date_val, "/", sep = "")

  # Get Ensemble data
  ensemble_data = combine_ensemble_data(date_val, lead_time = "00000",
                                      member_ref = member_ref,
                                      data_dir = data_dir)
  if(is.null(ensemble_data)) next
  ensemble_data = ensemble_data %>%
    mutate(obs = ifelse(obs >= na_value, NA_real_, obs)) %>%
    mutate(wtot = ifelse(wtot >= na_value, NA_real_, wtot)) %>%
    mutate(wtoc = ifelse(wtoc >= na_value, NA_real_, wtoc))
  dim(ensemble_data)


  # Get Observed data
  obs_data <- ensemble_data %>%
    select(t, harm, sur, obs) %>%
    distinct()
  dim(obs_data)

  print("Question to self: Do I want to remove NA obs?")
  print("How does this effect my analysis")

  # Get spaghetti summaries

  # Peak Surge level
  peak_sur_obs = obs_data %>%
    slice(which.max(sur[!is.na(sur)])) %>%
    mutate(date = date_val) %>%
    left_join(ensemble_data) %>%
    mutate(member = "obs")

  peak_sur_ens = ensemble_data %>%
    group_by(member) %>%
    slice(which.max(wsur[!is.na(wsur)])) %>%
    mutate(date = date_val) %>%
    ungroup() %>%
    left_join(obs_data)

  peak_sur_loop = rbind(peak_sur_obs, peak_sur_ens)
  peak_sur <- rbind(peak_sur, peak_sur_loop)
  if(!is.data.frame(peak_sur)) break

  # Peak water level
  peak_obs_obs = obs_data %>%
    slice(which.max(obs[!is.na(obs)])) %>%
    left_join(ensemble_data) %>%
    mutate(member = "obs", var= "obs")

  peak_wtot_ens = ensemble_data %>%
    group_by(member) %>%
    slice(which.max(wtot[!is.na(wtot)])) %>%
    left_join(obs_data) %>%
    ungroup() %>%
    mutate(var = "wtot")

  peak_wtoc_ens = ensemble_data %>%
    group_by(member) %>%
    slice(which.max(wtoc)) %>%
    left_join(obs_data) %>%
    ungroup() %>%
    mutate(var = "wtoc")

  peak_lvl_loop = rbind(peak_obs_obs,
                   peak_wtot_ens,
                   peak_wtoc_ens) %>%
    mutate(date = date_val)

  peak_lvl = rbind(peak_lvl, peak_lvl_loop)

  # ggplot() +
  #   geom_density(data = peak_wtot_ens, aes(wtot), alpha = 0.2, fill = "blue") +
  #   geom_density(data = peak_wtoc_ens, aes(wtoc), alpha = 0.2, fill = "red") +
  #   geom_vline(xintercept = peak_obs_obs$obs, linetype = "dashed") +
  #   geom_vline(xintercept = mean(peak_wtot_ens$wtot), linetype = "dotted", col = "blue") +
  #   geom_vline(xintercept = mean(peak_wtoc_ens$wtoc), linetype = "dotted", col = "red") +
  #   theme_bw()

  # Longest surge period of positive surge
  sur_clusters_obs = obs_data %>%
    mutate(bool_col = sur >= 0) %>%
    pull(bool_col) %>%
    get_clusters_above_threshold(cluster_col = ., date_col = obs_data$t) %>%
    slice(which.max(lengths)) %>%
    mutate(member = "obs")

  sur_clusters_ens = lapply(member_ref, function(ref){
    member_forecast <- ensemble_data %>%
      mutate(bool_col = wsur >= 0) %>%
      filter(member == ref)
    member_cluster <- member_forecast %>%
      pull(bool_col) %>%
      get_clusters_above_threshold(cluster_col = .,
                                   date_col = member_forecast$t) %>%
      slice(which.max(lengths)) %>%
      as.data.frame() %>%
      mutate(member = ref)
  }) %>%
    do.call("rbind", .)

  sur_clusters_loop = rbind(sur_clusters_obs, sur_clusters_ens) %>%
    mutate(date = date_val)
  sur_clusters = rbind(sur_clusters, sur_clusters_loop)

  # ggplot() +
  #   geom_histogram(data = sur_clusters_ens, aes(lengths),
  #                  alpha = 0.2, fill = "blue") +
  #   geom_vline(xintercept = sur_clusters_obs$lengths, linetype = "dashed") +
  #   geom_vline(xintercept = mean(sur_clusters_ens$lengths), linetype = "dotted") +
  #   theme_bw()

  # Risk
  time_scale = 1/nrow(obs_data)*max(obs_data$t)
  warning("Made naughty assumptions!")
  warning("Careful: assuemd forecast and obs period are the same")
  warning("Careful: used a row proxy instead of the real value to estiamte time_scale")

  risk_obs_obs = obs_data %>%
    filter(!is.na(obs)) %>%
    mutate(bool = obs > risk_level) %>%
    pull(bool) %>%
    sum()*time_scale
  risk_obs_obs = data.frame(member = "obs",
                            period = risk_obs_obs,
                            var = "obs")

  risk_wtot_ens = ensemble_data %>%
    filter(!is.na(wtot)) %>%
    mutate(bool = wtot > risk_level) %>%
    filter(bool == TRUE) %>%
    group_by(member, bool) %>%
    summarise(wtot = sum(bool)*time_scale) %>%
    mutate(period = wtot, var = "wtot") %>%
    select(-bool, -wtot) %>%
    ungroup()

  risk_wtoc_ens = ensemble_data %>%
    filter(!is.na(wtoc)) %>%
    mutate(bool = wtoc > risk_level) %>%
    filter(bool == TRUE) %>%
    group_by(member, bool) %>%
    summarise(wtoc = sum(bool)*time_scale) %>%
    mutate(period = wtoc, var = "wtoc") %>%
    select(-bool, -wtoc) %>%
    ungroup()

  risk_period_loop <- rbind(risk_obs_obs,
                       risk_wtoc_ens,
                       risk_wtot_ens) %>%
    mutate(date = date_val)
  risk_period = rbind(risk_period, risk_period_loop)

  # ggplot() +
  #   geom_histogram(data = risk_wtot_ens, aes(wtot), alpha = 0.2, fill = "blue") +
  #   geom_histogram(data = risk_wtoc_ens, aes(wtoc), alpha = 0.2, fill = "red") +
  #   geom_vline(xintercept = risk_obs_obs, linetype = "dashed") +
  #   geom_vline(xintercept = mean(risk_wtot_ens$wtot), linetype = "dotted", col = "blue") +
  #   geom_vline(xintercept = mean(risk_wtoc_ens$wtoc), linetype = "dotted", col = "red") +
  #   theme_bw()

  # Emsemble average - added on plots
  # should splitt wtot and wtoc onto two separate plots

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
