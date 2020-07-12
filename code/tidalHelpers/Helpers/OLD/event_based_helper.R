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

num_days = length(dates_vec)
peak_surge_ens <- peak_surge_obs <- vector("list", num_days)
peak_level_ens <- peak_level_obs <- vector("list", num_days)
duration_surge_ens <- duration_surge_obs <- vector("list", num_days)
duration_risk_ens <- duration_risk_obs <- vector("list", num_days)

for(i in 1:num_days){

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

### --------------------------------------------------------------------------------

peak_surge_obs_all <- do.call("rbind", peak_surge_obs)
ggplot(peak_surge_obs_all %>%
         group_by(member) %>%
         summarise(mean_wsur = mean(wsur)) %>%
         ungroup()) +
  geom_point(aes(x = wsur, y = sur, col = t)) +
  scale_color_distiller(palette = "Spectral") +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted")

# for each of the g functions I want to pull out the data from the raw ensemble
# ie. the mean value
# Then I want to calculate the BSS so Sum (F - O)^2
# Could do this for different lead times (Not just the longest one)
# loop over my data
