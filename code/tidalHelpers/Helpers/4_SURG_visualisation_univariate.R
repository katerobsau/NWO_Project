warning("You are naughty ungroup your shit")

ensemble_data_extended <- readRDS(ens_score_path)
scores_bootstrapped <- readRDS(ens_score_bootsrap_path)

# ---------------------------------------------------------------------------------
#
# # Visualise the CRPS scores (ALL DAYS)
#
# crps_results <- ensemble_data_extended %>%
#   dplyr::select(INIT_TIME, LEAD_TIME, SEASON, CRPSS_SIM, CRPSS_RAW) %>%
#   tidyr::pivot_longer(cols = c("CRPSS_SIM", "CRPSS_RAW"),
#                       names_to = "METHOD", values_to = "CRPS") %>%
#   dplyr::filter(LEAD_TIME %in% daily_lead_times) %>%
#   dplyr::mutate(NUM_DAYS = lubridate::as.period(LEAD_TIME) %>%
#                   as.numeric("days"))
#
# ggplot(data = crps_results) +
#   geom_boxplot(aes(x = NUM_DAYS, y = CRPS,
#                    group = interaction(NUM_DAYS, METHOD),
#                    col = METHOD), outlier.alpha = 0) +
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   facet_wrap(~SEASON) +
#   xlab("Lead time in days") +
#   ylab("CRPSS") +
#   scale_x_continuous(breaks = days) +
#   ggtitle(location_ref) +
#   theme_bw() +
#   ylim(c(-1, 1))
#
# # Change the legend labels ? shift legend position
# warning("Assumed x axis breaks")
# warning("Shorted the y axis")
# warning("Omitted outlies")

# -----------------------------------------------------------------------------

crps_summary_results <- scores_bootstrapped %>%
  dplyr::select(INIT_TIME, LEAD_TIME, SEASON,
                CRPS_SIM, CRPS_RAW, CRPS_REF, SUMMARY) %>%
  dplyr::mutate(CRPSS_SIM = 1 - CRPS_SIM/CRPS_REF,
                CRPSS_RAW = 1 - CRPS_RAW/CRPS_REF) %>%
  dplyr::select(-CRPS_SIM, -CRPS_RAW, -CRPS_REF) %>%
  tidyr::pivot_longer(cols = c("CRPSS_SIM", "CRPSS_RAW"),
                      names_to = "METHOD", values_to = "CRPSS") %>%
  dplyr::mutate(NUM_DAYS = lubridate::as.period(LEAD_TIME) %>%
                  as.numeric("days"))

if(location_ref == "LAUW"){
  plot_title = "Lauwersoog"
}else if(location_ref == "HARL"){
  plot_title = "Harlingen"
}else if(location_ref == "LEEW"){
  plot_title = "Leeuwarden"
}

crpss_bootstrap_plot <- ggplot(data = crps_summary_results) +
  geom_boxplot(aes(x = NUM_DAYS, y = CRPSS,
                   group = interaction(NUM_DAYS, METHOD, SUMMARY),
                   col = interaction(SUMMARY,METHOD)),
               outlier.alpha = 0) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_wrap(~SEASON) +
  xlab("Lead time in days") +
  ylab("CRPSS") +
  scale_x_continuous(breaks = days) +
  ggtitle(paste(plot_title, ": Continuous Rank Probability Skill Score", sep = ""),
          "Grouped by Season") +
  theme_bw() +
  theme(legend.position = "bottom") +
  ylim(c(-0.1, 1)) +
  scale_color_manual(name="Summary",
                       values = c("#f78400", "#fcbf79",
                                  "#0074f7","#61abff"),
                      labels = c("Raw: Mean", "Raw: Median",
                               "PP: Mean", "PP: Median"))


# Change the legend labels ? shift legend position
warning("Assumed x axis breaks")
warning("Shorted the y axis -  bad for boxplots")
warning("Omitted outlies")

crpss_bootstrap_plot
# saveRDS(crpss_bootstrap_plot, crpss_bootstrap_plot_path)

# -----------------------------------------------------------------------------

# Visualise the rank histograms
rank_results <- ensemble_data_extended %>%
  dplyr::select(INIT_TIME, LEAD_TIME, SEASON,
                RANK_RAW, RANK_SIM) %>% # RANK_CLIM
  tidyr::pivot_longer(cols = c("RANK_RAW", "RANK_SIM"), #"RANK_CLIM"
                      names_to = "METHOD", values_to = "RANK") %>%
  dplyr::mutate(NUM_DAYS = lubridate::as.period(LEAD_TIME) %>%
                  as.numeric("days")) %>%
  dplyr::mutate(METHOD = if_else(METHOD == "RANK_RAW", "RAW", METHOD)) %>%
  dplyr::mutate(METHOD = if_else(METHOD == "RANK_SIM", "PP", METHOD))

# plot_list = vector("list", 9)
# for(lead_days in 1:9){
  # plot_list[[lead_days]] <-
lead_days = c(3,9)
rank_compare <- ggplot(data = rank_results %>%
                         dplyr::filter(NUM_DAYS %in% lead_days)) +
  geom_histogram(aes(RANK, fill = METHOD),
                 position = "dodge",
                 # fill = "white",
                 # alpha = 0.6,
                 binwidth = 1) +
  facet_grid(NUM_DAYS~SEASON) +
  scale_fill_manual(name="Method",
                     values = c("#0074f7", "#f78400"),
                     labels = c("PP", "Raw")) +
  xlab("Rank") +
  ylab("Count") +
  ggtitle(paste(plot_title, ": Rank Histogram", sep = ""),
      "Grouped by Season and Days Lead Time") +
  theme_bw() +
  theme(
    legend.position = c(0.78,0.9),
    legend.background = element_rect(fill = NA, colour = "gray"),
    legend.direction="horizontal")

rank_compare

saveRDS(rank_compare, rank_compare_plot_path)
#
# # Plot rank by lead times for Sim
# rank_hist <- ggplot(data = rank_results %>%
#                       dplyr::filter(SEASON == "WINTER") %>%
#                       dplyr::filter(Method == "RANK_SIM")) +
#   geom_histogram(aes(RANK), binwidth = 1) +
#   facet_wrap(~ NUM_LT, ncol = 3) +
#   ggtitle(paste("Rank Histogram:", "Winter at", location_ref)) +
#   theme_bw()
#
# rank_hist

# -----------------------------------------------------------------------------

# PLOT THE CLIMATE DISTRIBUTION
clim_samples_single_lead_time <- ensemble_data_extended %>%
  dplyr::filter(LEAD_TIME == 0, INIT_TIME == 0) %>%
  dplyr::select(FORECAST_DATE, SEASON, OBS)

clim_plot <- ggplot() +
  geom_density(data = clim_samples, aes(OBS, group = SEASON, col = SEASON)) +
  geom_density(data = clim_samples_single_lead_time, aes(OBS, group = SEASON, col = SEASON), linetype = "dotted") +
  theme_bw()
clim_plot

season_cols <- clim_samples %>%
  tidyr::pivot_wider(names_from = SEASON, values_from = OBS) %>%
  dplyr::select(-OBS_DATE)

clim_mean = colMeans(season_cols, na.rm = TRUE)
clim_median = apply(season_cols,2, median, na.rm = TRUE)
clim_sd = apply(season_cols,2, sd, na.rm = TRUE)
clim_skew = apply(season_cols,2, moments::skewness, na.rm = TRUE)
clim_kurt = apply(season_cols,2, moments::kurtosis, na.rm = TRUE)
clim_mean; clim_median; clim_sd; clim_skew; clim_kurt

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

cols = gg_color_hue(2)

clim_plot1 <- ggplot() +
  geom_density(data = clim_samples, aes(OBS, group = SEASON, col = SEASON)) +
  geom_density(data = clim_samples_single_lead_time, aes(OBS, group = SEASON, col = SEASON), linetype = "dotted") +
  geom_density(data = NULL, aes(rnorm(10000, clim_mean[1], clim_sd[1])), col = cols[1], linetype = "dashed") +
  geom_density(data = NULL, aes(rnorm(10000, clim_mean[2], clim_sd[2])), col = cols[2], linetype = "dashed") +
  theme_bw()

clim_plot1

# -----------------------------------------------------------------------------
#
# # Visualise the brier scores
# bs_preprocess <- ensemble_data_extended %>%
#   dplyr::select(INIT_TIME, LEAD_TIME, SEASON,
#                 starts_with("BS_SIM")) %>%
#   tidyr::pivot_longer(cols = starts_with("BS"),
#                       names_to = "THRESHOLD", values_to = "BS_SIM") %>%
#   dplyr::mutate(THRESHOLD = as.numeric(as.character(readr::parse_number(THRESHOLD)))) %>%
#   dplyr::mutate(NUM_LT = as.numeric(LEAD_TIME/60/60/24)) %>%
#   dplyr::filter(NUM_LT == trunc(NUM_LT))
#
# bs_results <- bs_preprocess %>%
#   dplyr::group_by(INIT_TIME, LEAD_TIME, NUM_LT, SEASON, THRESHOLD) %>%
#   dplyr::summarise(MEAN_BS_SIM = mean(BS_SIM, na.rm = TRUE)) %>%
#   dplyr::ungroup() %>%
#   left_join(seasonal_quantiles %>%
#               dplyr::mutate(THRESHOLD = as.numeric(as.character(THRESHOLD))),
#             by = c("SEASON", "THRESHOLD"))
#
# # Plot rank by lead times for Sim
# season_ref = "SUMMER"
# bs_plot <- ggplot(data = bs_results %>%
#                     dplyr::filter(!is.na(PROBS))) +
#   geom_line(aes(x = THRESHOLD, y = 1 - MEAN_BS_SIM/(1 - PROBS),
#                 col = SEASON)) +
#   facet_wrap(~ NUM_LT, ncol = 3) +
#   ggtitle(paste("Brier Skill Score:", season_ref, "at", location_ref)) +
#   theme_bw() +
#   ylim(c(0,1))
#
# bs_plot

# -----------------------------------------------------------------------------

# Visualise the brier scores
bs_preprocess <- scores_bootstrapped %>%
  dplyr::filter(SUMMARY == "MEAN") %>%
  dplyr::select(INIT_TIME, LEAD_TIME, SEASON,
                starts_with("BS_SIM")) %>%
  tidyr::pivot_longer(cols = starts_with("BS"),
                      names_to = "THRESHOLD", values_to = "BS_SIM") %>%
  dplyr::mutate(THRESHOLD = as.numeric(as.character(readr::parse_number(THRESHOLD)))) %>%
  dplyr::mutate(NUM_DAYS = lubridate::as.period(LEAD_TIME) %>%
                  as.numeric("days")) %>%
  left_join(seasonal_quantiles %>%
              dplyr::mutate(THRESHOLD = as.numeric(as.character(THRESHOLD))),
            by = c("SEASON", "THRESHOLD")) %>%
  dplyr::mutate(BSS = 1 - BS_SIM/(1-PROBS))
#
# bs_results <- bs_preprocess %>%
#   dplyr::group_by(INIT_TIME, LEAD_TIME, NUM_DAYS, SEASON, THRESHOLD, PROBS) %>%
#   dplyr::summarise(MEAN_BS_SIM = mean(BS_SIM, na.rm = TRUE)) %>%
#   dplyr::ungroup() %>%
#   dplyr::filter(!is.na(PROBS)) %>%
#   dplyr::mutate(BSS_MEAN = 1 - MEAN_BS_SIM/(1 - PROBS))

bss_plot <- ggplot(data = bs_preprocess) +
  geom_boxplot(aes(x = THRESHOLD, y = BSS,
                group = THRESHOLD, col = SEASON),
               outlier.alpha = 0) +
  geom_hline(yintercept = 0, col = "gray", alpha = 0.5, linetype = "dashed") +
  facet_wrap(~NUM_DAYS, ncol = 3) +
  ggtitle(paste("Brier Skill Score at", plot_title)) +
  theme_bw() #+
  # ylim(c(0,1))

bss_plot

warning("NA weirdness - I think it's the thresholds in different seasons")
warning("Need to rewrite the BSS code")

# -----------------------------------------------------------------------------

# # OLD INITIALISTION
#
# surge_dir = "/Users/katesaunders/Documents/No_Back_Up_Data/SURGE/"
# num_members = 50
# na_value = 99.99
# location_ref = "LAUW"
# element_ref = "SURG"
# print_my_warnings = FALSE
# overwrite_saved = FALSE
# # selected_season = "WINTER"
# # combine_winter = c(2015,2017)
# # omit_years = c(2010,2019)
# num_samples = 500
# fixed_vars = c("INIT_TIME", "LEAD_TIME", "SEASON")
#
# ens_pp_path = paste(getwd(), "/data/", location_ref, "_pp_ens.rds", sep = "")
# ens_score_path = paste(getwd(), "/data/", location_ref, "_score_ens.rds", sep = "")
# ens_score_bootsrap_path = paste(getwd(), "/data/", location_ref, "_score_bootstrap.rds", sep = "")
#
# # ---------------------------------------------------------------------------------
#
# crpss_bootstrap_plot_path = paste(getwd(), "/data/", location_ref, "_crpss_bootstrap_plot.rds", sep = "")
# rank_compare_plot_path = paste(getwd(), "/data/", location_ref, "_rankCompare_plot.rds", sep = "")
# # rank9_plot_path = paste(getwd(), "/data/", location_ref, "_rank9_plot.rds", sep = "")
# # rank3_plot_path = paste(getwd(), "/data/", location_ref, "_rank3_plot.rds", sep = "")
#
# # ---------------------------------------------------------------------------------
#
# library(tidyverse)
# library(ggplot2)

