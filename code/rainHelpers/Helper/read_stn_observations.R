### Get aggregated obs
data_dir = "../Leeuwarden_Data/"
output_dir = "../Summary_Data/"
years = 2008:2019
total_num_days_for_aggr <- 10
daily_obs <- Get_daily_obs(years, data_dir = data_dir)
aggr_daily_obs <- Wrapper_obs_aggr(
  total_num_days_for_aggr = total_num_days_for_aggr, daily_obs = daily_obs)

saveRDS(aggr_daily_obs, file = paste(output_dir,
                                   "Leeuwarden_Obs_Aggregated.rds", sep = ""))
