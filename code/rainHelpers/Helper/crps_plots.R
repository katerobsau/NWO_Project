# CRPS plot

data_dir = "~/Documents/No_Back_Up_Data/Leeuwarden_Data/"
lead_time_list <- readRDS(paste0(data_dir, "rainfall_lead_times.rds"))
score_summary <- readRDS(paste0(data_dir, "rainfall_score_summary.rds"))

crps_info <- 1:length(score_summary) %>%
  lapply(function(i){
    cv_data = score_summary[[i]]$crps
    forecast_details = lead_time_list[[i]] %>%
      dplyr::select(lead_time, Forecast_Date, Init, Season) %>%
      cbind(cv_data)
    return(forecast_details)
  }) %>%
  do.call("rbind", .)

plot_crps_info <- crps_info %>%
  dplyr::select(-Forecast_Date, -group) %>%
  filter(Init == 0, Season == 0) %>%
  pivot_longer(cols = c(crps_emos, crps_raw),
               names_to = "PP",
               values_to = "CRPS") %>%
  mutate(PP = if_else(PP == 'pp_rank', "Post-Processed", PP),
         PP = if_else(PP == 'raw_rank', "Raw", PP)) %>%
  mutate(lead_time1 = lubridate::hour(lead_time)) %>%
  group_by(lead_time1, Init, Season, PP) %>%
  summarise(Median = median(CRPS),
            Mean = mean(CRPS),
            CI_upper = quantile(CRPS, 0.95),
            CI_lower = quantile(CRPS, 0.05)) %>%
  ungroup() #%>%
  # pivot_longer(cols = c("Median", "Mean"), names_to = "Summary", values_to = "Stat")

ggplot_cols <- c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF")
crps_plot <- ggplot(data = plot_crps_info, group = PP) +
  geom_ribbon(aes(x = lead_time1, ymin = CI_lower, ymax = CI_upper,
                  fill = PP), alpha = 0.4) +
  geom_line(aes(x = lead_time1, y = Median, col = PP)) +
  geom_point(aes(x = lead_time1, y = Median, col = PP, shape = PP)) +
  geom_line(aes(x = lead_time1, y = Mean, col = PP)) +
  geom_point(aes(x = lead_time1, y = Mean, col = PP, shape = PP)) +
  scale_shape_manual("Type", labels = c("Post-processed", "Raw"), values = c(20,3)) +
  scale_color_manual("Type", labels = c("Post-processed", "Raw"), values = ggplot_cols[c(1,3)]) +
  scale_fill_manual("Type", labels = c("Post-processed", "Raw"), values = ggplot_cols[c(1,3)]) +
  theme_bw() +
  xlab("Lead Time") +
  ylab("CRPS Score")
crps_plot

save_dir = "/Users/katesaunders/Documents/Git/NWO_Project/data/"
saveRDS(crps_plot, paste0(save_dir, "plot_rainfall_crps.rds"))
