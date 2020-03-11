library(tidyverse)
library(gamlss)
library(patchwork)

# -----------------------------------------------------------------------------

# Set up data directory
data_dir = "~/Documents/No_Back_Up_Data/Leeuwarden_Data/"

# -----------------------------------------------------------------------------

# Read in the ensemble data
ens_files <- list.files(paste0(data_dir, "ENS/"))
print(paste(length(ens_files), "ensemble files"))
file_ref_str <- lapply(ens_files, function(str){
  central_str = str_match_all(str, "[0-9]+")[[1]][2,1]
})
date_str <- lapply(file_ref_str, function(str){substr(str, 1,8)}) %>%
  unlist() %>%
  lubridate::as_date()
init_str <- lapply(file_ref_str, function(str){substr(str, 9,12)})
ens_data_all <- lapply(ens_files, function(str){
  paste0(data_dir, "ENS/", str) %>% read_csv()
})

# Function to prepocess the ensemble into the right from
format_ens <- function(ens_data){
  col_names = ens_data %>% pull(members)
  ens_data <- ens_data %>%
    dplyr::select(-members) %>%
    t() %>%
    as.data.frame() %>%
    (function(.){names(.) = col_names; return(.)}) %>%
    mutate(lead_time = rownames(.) %>% parse_number())
  return(ens_data)
}

# Preprocess step
ens_preprocessed0 <- lapply(ens_data_all, format_ens)
lens = lapply(ens_preprocessed0, nrow) %>% unlist()
ens_preprocessed <- ens_preprocessed0 %>%
  bind_rows() %>%
  dplyr::mutate(Forecast_Date = rep(date_str, times = lens),
                Init  = rep(init_str, times = lens) %>% as.numeric(),
                lead_time = paste(lead_time, 0, 0, sep = ":") %>% lubridate::hms(),
                Init = paste(Init/100, 0, 0, sep = ":") %>% lubridate::hms(),
                Hours_ahead = Init + lead_time,
                Obs_Time = as.character.POSIXt(Forecast_Date + Hours_ahead)) %>%
  dplyr::select(-Hours_ahead)

# -----------------------------------------------------------------------------

# Function to preprocess the observations
format_obs <- function(obs_data){
  print(obs_data$YYYYMMDD[1])
  obs_data <- obs_data %>%
    mutate(Date = YYYYMMDD %>% as.character() %>% lubridate::as_date()) %>%
    dplyr::select(Date, HH, RH) %>%
    mutate(HH = (floor((HH - 1)/6) + 1)*6) %>%
    group_by(Date, HH) %>%
    summarise(RH6 = sum(RH)) %>%
    ungroup() %>%
    mutate(FLAG = RH6 < 0,
           RH6 = ifelse(FLAG == TRUE, 0, RH6)) %>%
    mutate(Hours_ahead = paste(HH, 0, 0, sep = ":") %>% lubridate::hms(),
           Obs_Time = as.character.POSIXt(Date + Hours_ahead)) %>%
    dplyr::select(-Hours_ahead)
  return(obs_data)
}

# Read in the ensemble data
obs_files <- list.files(paste0(data_dir, "OBS/Subdaily/")) %>%
  paste0(data_dir, "OBS/Subdaily/", .)
print(paste(length(obs_files), "observation files"))
obs_data_all <- lapply(obs_files, function(file){read_csv(file, skip = 30)})
obs_preprocessed <- lapply(obs_data_all, format_obs) %>% bind_rows()

# -----------------------------------------------------------------------------

# Combine Forecast and Observations and get member_data
combined_data <- left_join(ens_preprocessed, obs_preprocessed,
                           by = "Obs_Time")

# File saved out so I don't need to rerun the above
# saveRDS(combined_data, paste0(data_dir, "rainfall_forecast_data.rds"))

# -----------------------------------------------------------------------------
#
# # plot the ensemble
# plot_ens <- ens_preprocessed %>%
#   pivot_longer(cols = 1:52, names_to = "Member", values_to = "RH")
#
# library(plotly)
# d <- highlight_key(plot_ens, ~Member)
# trajectory_plot <- ggplot(d) +
#   geom_line(aes(x = lead_time, y = RH, group = Member), col = "gray") +
#   theme_bw() +
#   theme(legend.position = "none")
# trajectory_interactive <- ggplotly(trajectory_plot, tooltip = "Member")
# highlight(trajectory_interactive, dynamic = TRUE,
#           color = c("orange", "purple", "forestgreen"))
#   # shft to hold lines
