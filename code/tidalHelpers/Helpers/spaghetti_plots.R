init_var = utils_init()
main_data_dir = "/Users/katesaunders/Documents/No_Back_Up_Data/ENS/"

dates_vec = list.files(main_data_dir)
member_ref = get_ensemble_ref(init_var$num_members)

date_val =  "2011112900" #"2012010200" #dates_vec[1]

data_dir = paste(main_data_dir, date_val, "/", sep = "")

# Get Ensemble data
ensemble_data = combine_ensemble_data(date_val, lead_time = "00000",
                                      member_ref = member_ref,
                                      data_dir = data_dir)
dim(ensemble_data)

# Get Observed data
obs_data <- ensemble_data %>%
  select(t, harm, sur, obs) %>%
  distinct()

# Reduce to hourly observations
# Summary used to max water level not max surge
reduced_obs <- obs_data %>%
  mutate(hour = floor(t)) %>%
  group_by(hour) %>%
  summarise(obs = max(obs)) %>%
  left_join(obs_data)

dim(reduced_obs)
print("Careful possible to have multiple maxima in a given hour.")
# Due to harm1 + sur1 = harm2 + sur2 = max_obs

# Reduce to hourly ensemble data
grouped_ensemble <- ensemble_data %>%
  mutate(hour = floor(t)) %>%
  group_by(hour, member)

ensemble_tides <- ensemble_data %>%
  select(t, wsur, htid, wtid) %>%
  distinct() %>%
  mutate(hour = floor(t))

wsur_reduced <- grouped_ensemble %>%
  summarise(wsur = max(wsur)) %>%
  left_join(ensemble_tides)

# Couldn't decide which one to maximise so combined all of them
reduced_ensemble = wsur_reduced %>%
  mutate(wtot = wsur + htid, wtoc = wsur + wtid) %>%
  left_join(obs_data)
dim(reduced_ensemble_data)

# Plot the trajectory
ggplot(data = reduced_ensemble) +
  geom_line(aes(x = hour, y = wtoc, group = member), col = "gray") +
  # geom_line(aes(x = hour, y = harm), col = "black", size = 0.3) +
  geom_line(aes(x = hour, y = obs), col = "red", size = 0.3) +
  # geom_line(aes(x = hour, y = sur), col = "blue", size = 0.3) +
  # geom_point(aes(x = hour, y = sur), col = "blue", size = 0.3) +
  geom_hline(yintercept = 0, col = "gray", linetype = "dashed") +
  geom_hline(yintercept = -0.42, linetype = "dashed") +
  # geom_point(aes(x = hour, y = wtot), col = "gray") +
  theme_bw() +
  ylim(c(-4,4))

