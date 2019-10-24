library(tidyverse)
library(tidalHelpers)

# Threshold level
h_t_level = -0.42

### --------------------------------------------------------------------------

# Read in  the ensemble data
ens_summary_file = "Ensemble_Summary.rds"
init_var = tidalHelpers::utils_init()
ens_summary_dir = init_var$save_dir
ens_summary_path = paste(ens_summary_dir, ens_summary_file, sep = "")
ens_summary = readRDS(ens_summary_path)

# Filter out the observations
obs_data = ens_summary %>%
  dplyr::filter(t < 24) %>%
  dplyr::mutate(
    temp_date = gsub("-", " ", date) %>% lubridate::ymd()) %>%
  dplyr::mutate(
    temp_time = paste(0, round(t*60), 0, sep = " ") %>% lubridate::hms(roll = TRUE)) %>%
  dplyr::mutate(ldate =
                  lubridate::ymd_hms(temp_date + temp_time)) %>%
  dplyr::select(ldate, harm, obs, sur) %>%
  dplyr::mutate(obs = if_else(is.na(sur), NA_real_, obs))

print("Note: Should add handling of NA Surge values into preprocessing")

### --------------------------------------------------------------------------

# Identify periods of risk above the threshold level
bool_col = obs_data %>%
  mutate(bool_col = obs >= h_t_level) %>%
  pull(bool_col)
clusters_risk_threshold = get_clusters_above_threshold(cluster_col = bool_col,
                             date_col = obs_data$ldate)

# Get the maximum surge height per risk period
max_height = get_summary_per_cluster(
                clusters = clusters_risk_threshold,
                height_data = obs_data %>% select(harm, obs, sur),
                type = "max")

# Identify surge clusters above 0 m
sur_bool_col = obs_data %>%
  mutate(bool_col = sur >= 0) %>%
  pull(bool_col)
clusters_sur = get_clusters_above_threshold(cluster_col = sur_bool_col,
                                            date_col = obs_data$ldate)

# Get the maximum surge height for these clusters
max_height_sur = get_summary_per_cluster(
  clusters = clusters_sur,
  height_data = obs_data %>% select(harm, obs, sur),
  type = "max") %>%
  as.data.frame()

# For each of these surge periods
# get period sluice is non-operational
thres_exceeded = get_summary_per_cluster(
  clusters = clusters_sur,
  height_data = obs_data %>% pull(obs),
  type = "risktime",
  h_t_level = h_t_level)

# Climatology for surge duration
sur_climatology <- clusters_sur %>%
  mutate(max_sur = max_height_sur$sur) %>%
  dplyr::filter(values == TRUE) %>%
  mutate(lengths = lengths*10/60/24)

summary_by_month <- sur_climatology %>%
  mutate(month = date_start %>%
           lubridate::as_date() %>%
           lubridate::ymd() %>%
           lubridate::month()) %>%
  group_by(month) %>%
  summarise(avr = mean(max_sur),
            count = length(max_sur))

summary_by_day <- sur_climatology %>%
  mutate(day = date_start %>%
           lubridate::as_date() %>%
           lubridate::ymd() %>%
           lubridate::yday()) %>%
  group_by(day) %>%
  summarise(count = length(day))

plot_num_surge_by_date <- ggplot(summary_by_day) +
  # geom_histogram(aes(x = date_start), bins = 150) +
  geom_col(aes(x = day, y = count)) +
  theme_bw() +
  xlab("Day") +
  ylab("Number of Surges (Height > 0 m)")
plot_num_surge_by_date

plot_num_surge_by_month <- ggplot(summary_by_month) +
  geom_col(aes(x = month, y = count)) +
  theme_bw() +
  xlab("Year") +
  ylab("Number of Surges (Height > 0 m)")
plot_num_surge_by_month

plot_avr_surge_month <- ggplot(summary_by_month) +
  geom_col(aes(x = month, y = avr))
plot_avr_surge_month
#
# print("How does NA values effect the estimator??")
# sur_days =  sum(obs_data$sur > 0, na.rm = TRUE)
# num_days = (nrow(obs_data) - sum(is.na(obs_data$obs)))
# sur_ecdf <- function(time,
#                      sur_climatology,
#                      period_length,
#                      surge_prob = sur_days/num_days){
#
#   if(time == 0){
#     p = 1 - surge_prob^time # zero_mass
#   }else if(time < period_length){
#     p = (1- surge_prob) + ecdf(sur_climatology$lengths)(time) #days
#   }else{
#     p = surge_prob + (1 - ecdf(sur_climatology$lengths)(period_length))
#   }
#   return(p)
# }
# time_x = seq(0,7, length.out = 100)
# ecdf_y = sapply(time_x, sur_ecdf,
#                 sur_climatology = sur_climatology,
#                 period_length= 1)
# plot(time_x, ecdf_y)

