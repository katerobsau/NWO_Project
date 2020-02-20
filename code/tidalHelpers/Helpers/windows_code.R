season_ref = 2011
season_data_dir = "/Users/katesaunders/Documents/No_Back_Up_Data/Winter/"
season_all = readRDS(file = paste0(season_data_dir, "Winter", season_ref, ".rds"))

library(tidyverse)
library(zoo)

lead_times = season_all %>% pull(t) %>% unique()

bss_df <- NULL
for(centre_day in 2:9){

# centre_day = 9
window_centre = centre_day*24
band_days = 2
band_width = band_days*24
window = lead_times[which(lead_times >= window_centre - band_width &
                            lead_times < window_centre + band_width)]

# get obs
obs_data <- season_all %>%
  filter(t < 24) %>%
  select(t, date, harm, sur, obs) %>%
  distinct()

# estimate the rolling mean
### CAREFUL OF NAs
roll_mean = obs_data %>%
  mutate(var = sur > 0) %>%
  pull(var) %>%
  rollapply(length(window), sum, na.rm = TRUE, align = "left")
roll_mean = c(roll_mean, rep(NA, nrow(obs_data) - length(roll_mean)))

#  append rollign mean to obs and fix date
obs_data = obs_data %>%
  mutate(roll_mean = roll_mean) %>%
  mutate(roll_mean = roll_mean*10/60) %>%
  mutate(date = str_sub(date, 1, 8) %>%
           lubridate::as_date())

# climatology
obs_durations <- obs_data %>% filter(t == 0) %>% distinct() %>%
  pull(roll_mean)
d = seq(1, band_width*band_days*2)
bss_clim <- sapply(d, function(d, obs_durations){
  clim_p <- ecdf(obs_durations)(d)
  clim_var = clim_p*(1-clim_p)
  return(clim_var)
}, obs_durations = obs_durations)

# estimate period of surge for each member,
# move observation window to be correctly centred
window_data <- season_all %>%
  filter(t %in% window) %>%
  group_by(member, date) %>%
  summarise(duration = sum(wsur > 0)/sum(!is.na(wsur))*band_width*2,
            max = max(wsur),
            time_max = which.max(wsur)) %>%
  ungroup() %>%
  group_by(date) %>%
  summarise(q50 = quantile(duration, 0.5),
            q75 = quantile(duration, 0.9),
            q25 = quantile(duration, 0.1)) %>%
  ungroup() %>%
  mutate(date = str_sub(date, 1, 8) %>%
    lubridate::as_date()) %>%
  left_join(obs_data %>% filter(t ==0)) %>%
  mutate(shifted_roll_mean = lead(roll_mean, centre_day - band_days))

# # plot quantiles of forecast
ggplot(data = window_data) +
  geom_line(aes(x = date, y = q50),
col = "blue", linetype = "dashed") +
  geom_ribbon(aes(x = date, ymin = q25, ymax = q75),
              fill = "blue", alpha = 0.5) +
  geom_line(data = window_data,
            aes(x = date,
                y = shifted_roll_mean, group = t),
            col = "black") +
  theme_bw() +
  ylab("Duration") +
  xlab("Date") +
  ggtitle("Forecast Surge Duration",paste(band_days*2, "window"))

# # plot spaghetti
ggplot() +
  geom_line(data = window_data,
            aes(x = date, y = duration, group = member),
            col = "gray", alpha = 0.2) +
  geom_line(data = window_data,
            aes(x = date,
                y = lead(roll_mean, centre_day - band_days), group = t),
            col = "red") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# BSS
d = seq(1, band_width*band_days*2)
bss_scores <- sapply(d, function(d, df){

bss_values <- sapply(df$date %>% unique(),
              function(date_val, d, df){

  date_data = df %>%
    filter(date == date_val)
  if(nrow(date_data) !=  50) return(NA)

  obs_val = date_data %>% pull(shifted_roll_mean) %>% unique()
  obs_bool = (obs_val > d) %>% mean()

  if(any(is.na(obs_val))) return(NA)
  if(any(is.na(date_data$duration))) return(NA)

  p = (1 - ecdf(date_data$duration)(obs_val)) %>% mean()
  bss_val = (p - obs_bool)^2
  return(bss_val)
  }, df = df, d = d)

score = sum(bss_values, na.rm = TRUE)/sum(!is.na(bss_values))

return(score)

}, df=  window_data)

bss_df <- rbind(bss_df, data.frame(d, bss_for = bss_scores,
                                   bss_clim = bss_clim,
                                   centre = rep(centre_day, length(d))
                                   ))

}

ggplot(bss_df) +
  geom_path(aes(x = d, y = bss_for,
                group = centre, col = centre)) +
  geom_point(aes(x = d, y = bss_clim),
            col = "red") +
  theme_bw() +
  xlim(c(0, band_width*band_days*2)) +
  ylab("Brier Score") +
  xlab("Duration")

ggplot(bss_df) +
  geom_path(aes(x = d, y = 1 - bss_for/bss_clim,
                group = centre, col = centre)) +
  theme_bw() +
  xlim(c(0, band_width*band_days*2)) +
  ylab("Brier Score") +
  xlab("Duration")
