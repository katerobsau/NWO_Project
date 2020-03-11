# Rank Histograms

data_dir = "~/Documents/No_Back_Up_Data/Leeuwarden_Data/"
score_summary <- readRDS(paste0(data_dir, "rainfall_score_summary.rds"))
lead_time_list <- readRDS(paste0(data_dir, "rainfall_lead_times.rds"))

rank_info <- 1:length(score_summary) %>%
  lapply(function(i){
    pp_rank = score_summary[[i]]$emos_rank
    forecast_details = lead_time_list[[i]] %>%
      dplyr::select(lead_time, Forecast_Date, Init, Season) %>%
      mutate(pp_rank)
    return(forecast_details)
  }) %>%
  do.call("rbind", .)

raw_rank <- lapply(score_summary, function(l) l$raw_rank) %>% unlist()
rank_info$raw_rank = raw_rank

rank_info_long = rank_info %>%
  pivot_longer(cols = c(pp_rank, raw_rank),
               names_to = "PP",
               values_to = "Rank") %>%
  mutate(PP = if_else(PP == 'pp_rank', "Post-Processed", PP),
         PP = if_else(PP == 'raw_rank', "Raw", PP))

plot_rank_data <- rank_info_long %>%
  mutate(lead_time1 = lubridate::hour(lead_time)) %>%
  filter(Init == 0 & Season == 0, lead_time1 %in% c(6,12,24)) %>%
  dplyr::select(-Init, -Season, -Forecast_Date, -lead_time)
plot_rank_data$PP = as.factor(plot_rank_data$PP)
plot_rank_data$PP = factor(plot_rank_data$PP,levels(plot_rank_data$PP)[2:1])

breaks = seq(0.5, num_members + 1.5)
num_breaks = length(breaks)

cis = c(0.01, 0.99)
q <- plot_rank_data %>%
  filter(PP == "Raw") %>%
  group_by(PP, lead_time1) %>%
  count() %>%
  ungroup() %>%
  mutate(CI1 = qbinom(size = n, p = cis[1], prob = 1/num_breaks)) %>%
  mutate(CI2 = qbinom(size = n, p = cis[2], prob = 1/num_breaks))
q <- rbind(q, q)
q$PP = rep(levels(plot_rank_data$PP), times = nrow(q)/2)

rank_hist <- ggplot() +
  geom_hline(data = q, aes(yintercept = CI1), col = "red", linetype = "dashed") +
  geom_hline(data = q, aes(yintercept = CI2), col = "red", linetype = "dashed") +
  geom_histogram(data = plot_rank_data, aes(Rank), breaks = breaks, alpha = 0.7) +
  facet_grid(lead_time1 ~ PP) +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  ggtitle("Rank Histograms")
rank_hist

save_dir = "/Users/katesaunders/Documents/Git/NWO_Project/data/"
saveRDS(rank_hist, paste0(save_dir, "plot_rainfall_rank_histograms.rds"))
