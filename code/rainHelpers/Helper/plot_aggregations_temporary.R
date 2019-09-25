### Plot aggregation against the ensemble
library(ggplot2)

### Get aggregated obs
data_dir = "../Leeuwarden_Data/"
years = 2008:2019
total_num_days_for_aggr <- 10
daily_obs <- Get_daily_obs(years, data_dir = data_dir)
aggr_daily_obs <- Wrapper_obs_aggr(
  total_num_days_for_aggr = total_num_days_for_aggr, daily_obs = daily_obs)

### Get ensemeble aggregations
data_dir = "../Leeuwarden_Data/"
ens_summary_file = paste(data_dir,
             "ENS_Median_Summary.rds", sep = "")
ens_summary = readRDS(ens_summary_file)

### Plot for aggregation period
aggr_day = 10
for(aggr_day in 1:10){
var_obs = paste("aggr_obs_", aggr_day, sep = "")
var_ens = paste("aggr_ens_", aggr_day, sep = "")

combine_aggr_df <- full_join(ens_summary, aggr_daily_obs, by = "Date")
col_1 = which(names(combine_aggr_df) == var_obs)
col_2 = which(names(combine_aggr_df) == var_ens)

select_vars <- combine_aggr_df %>%
  filter(Lead_Time == "0000") %>%
  select(col_1, col_2)
names(select_vars) <- c("obs", "ens")
plot_df <- select_vars #%>%
  # filter(obs != 0 & ens != 0)

max_val = max(combine_aggr_df$aggr_ens_10, combine_aggr_df$aggr_obs_10, na.rm = TRUE)
output_plot <- ggplot(data = plot_df) +
  geom_point(aes(x = obs, y = ens)) +
  geom_density2d(aes(x = obs, y = ens)) +
  geom_abline(slope = 1, intercept =0 , col = "red", linetype = "dotted") +
  theme_bw() +
  xlim(c(0,max_val/2 + max_val/2*(aggr_day > 5))) +
  ylim(c(0,max_val/2 + max_val/2*(aggr_day > 5))) +
  ggtitle(paste(aggr_day, "aggregation"))

print(output_plot)

}

lm_fit = lm(data = plot_df, obs ~ ens)
plot(lm_fit)

