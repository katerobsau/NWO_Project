#surge data
surge_data <- readRDS(file = "data/ensemble_summary_stats.rds") %>%
  mutate(date = str_sub(date, 1, 8) %>% lubridate::as_date())

surge_dates = surge_data %>%
  dplyr::filter(!is.na(sur)) %>%
  dplyr::select(date) %>%
  dplyr::distinct() %>%
  dplyr::pull(date)
length(surge_dates)

# rainfall data
data_dir = "~/Documents/No_Back_Up_Data/Leeuwarden_Data/"
rainfall_data <- readRDS(paste0(data_dir, "rainfall_forecast_data.rds"))
rainfall_dates <- rainfall_data %>%
  dplyr::filter(!is.na(RH6) & !is.na(ENS1)) %>%
  dplyr::select(Date) %>%
  dplyr::distinct() %>%
  dplyr::pull(Date)
length(rainfall_dates)

# overlap dates
length(intersect(surge_dates, rainfall_dates))

# plot binary dates
ggplot() +
  geom_point(aes(x = surge_dates, y = rep("SUR", length(surge_dates))),
             col = "blue", alpha = 0.7) +
  geom_point(aes(x = rainfall_dates, y = rep("RF", length(rainfall_dates))),
             col = "blue", alpha = 0.7) +
  theme_bw()

# rainfall data
# gotta fix the lead time /init filtering multiple points per date

rf_obs <- ggplot(data = rainfall_data) +
  geom_col(aes(x = Date, y = RH6),
           col = "blue", alpha = 0.2)

rf_ens <-  ggplot(data = rainfall_data) +
  geom_col(aes(x = Date, y = HR),
           col = "blue", alpha = 0.2)

sur_obs <- ggplot(data = surge_data) +
  geom_col(aes(x = date, y = sur),
           col = "blue", alpha = 0.2)

sur_ens <- ggplot(data = surge_data) +
  geom_col(aes(x = date, y = mean_wsur),
           col = "blue", alpha = 0.2)


library(patchwork)
rf_obs / rf_ens / sur_obs / sur_ens + theme_bw()
