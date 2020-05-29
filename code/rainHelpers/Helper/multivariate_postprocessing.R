# Multivariate rainfall post-processing

# got to work on reproducibility
num_members = 50
library(devtools)
install_github("katerobsau/depPPR", ref = "dev", force = TRUE)
library(depPPR)
library(scoringRules)
library(gamlss)
# library(depPPR, lib.loc = "/Users/katesaunders/Do cuments/Git/depPPR")

window_radius = 15
# min_na_threshold = 3

data_dir = "~/Documents/No_Back_Up_Data/Leeuwarden_Data/"
predictor_data <- readRDS(paste0(data_dir, "rainfall_predictor_data.rds"))
combined_data <- readRDS(paste0(data_dir, "rainfall_forecast_data.rds"))

#  ----------------------------------------------------------------------------

reduced_predictor_data = predictor_data %>%
  dplyr::filter(Init == lubridate::hms("0:0:0"),
  Season == 0)

Y_obs = reduced_predictor_data %>%
  dplyr::select(Date, HH, RH6) %>%
  dplyr::distinct() %>%
  pivot_wider(names_from = Date, values_from = RH6)
  # group_by(Forecast_Date) %>%
  # group_split(keep = FALSE)

warning("filterd by leadtime")
Y_obs_by_forecast = combined_data %>%
  # dplyr::select(lead_time, Forecast_Date, Init, Obs_Time, Date, RH6, HH)  %>%
  dplyr::filter(Init == lubridate::hms("0:0:0")) %>%
  dplyr::select(lead_time, Forecast_Date, RH6)  %>%
  dplyr::mutate(lead_time = lubridate::hour(lead_time)) %>%
  pivot_wider(names_from = Forecast_Date, values_from = RH6)

X_raw = reduced_predictor_data %>%
  dplyr::select(starts_with("ENS"), Forecast_Date) %>%
  group_by(Forecast_Date) %>%
  group_split(keep = FALSE)
names(X_raw) = reduced_predictor_data %>%
  pull(Forecast_Date) %>%
  unique() %>%
  lubridate::as_date()

# read in, saved from univariate post-processing
cv_list <- readRDS(paste0(data_dir, "rainfall_gamlss_fits.rds"))
lead_time_list <- readRDS(paste0(data_dir, "rainfall_lead_times.rds"))

all_pars <- 1:length(cv_list) %>% as.list() %>%
  lapply(function(i){
    # get raw ensemble
    pars = cv_list[[i]]$results
    ref_info = lead_time_list[[i]] %>%
      dplyr::select(lead_time, Forecast_Date, Season, Init)
    df = data.frame(pars,ref_info)
    return(df)
  }) %>%
  do.call('rbind', .)

pars <- all_pars %>%
  dplyr::filter(Init == lubridate::hms("0:0:0"),
                Season == 0) %>%
  group_by(Forecast_Date) %>%
  dplyr::select(mu, sigma, nu, Forecast_Date) %>%
  group_split(keep = FALSE)

# rainfall_pars <- saveRDS(all_pars, paste0(data_dir, "rainfall_pars.rds"))
rainfall_pars <- readRDS(paste0(data_dir, "rainfall_pars.rds"))

# -----------------------------------------------------------------------------

# ECC methods
set.seed(1)
Y_uni_eccR <- 1:length(X_raw) %>% lapply(function(i, num_members, pars){
  print(i)
  depPPR::sample_ecc_members(num_members = num_members,
                             function_type = rZAGA,
                             pars = pars[[i]],
                             ecc_type = 'R')
}, num_members, pars)
names(Y_uni_eccR) = names(X_raw)

Y_uni_eccQ <- 1:length(X_raw) %>% lapply(function(i, num_members, pars){
  print(i)
  depPPR::sample_ecc_members(num_members = num_members,
                             function_type = qZAGA,
                             pars = pars[[i]],
                             ecc_type = 'Q')
}, num_members, pars)
names(Y_uni_eccQ) = names(X_raw)

Y_multi_eccR <- 1:length(X_raw) %>%
  lapply(function(i, X_raw, Y_uni_eccR){
  print(i)
  tryCatch(depPPR::apply_ecc_template(as.matrix(X_raw[[i]]),
                                      as.matrix(Y_uni_eccR[[i]])),
           error =  function(e){print(e); return(NA)})
}, X_raw, Y_uni_eccR)
warning("Hacked in matrix qualifiers here")
warning("Try catch was suppressing error message needlessly")
warning("Same below")
names(Y_multi_eccR) = names(X_raw)

Y_multi_eccQ <- 1:length(X_raw) %>%
  lapply(function(i, X_raw, Y_uni_eccR){
    print(i)
    tryCatch(depPPR::apply_ecc_template(
      as.matrix(X_raw[[i]]),
      as.matrix(Y_uni_eccR[[i]])),
             error =  function(e){print(e); return(NA)})
  }, X_raw, Y_uni_eccQ)
names(Y_multi_eccQ) = names(X_raw)

#  ----------------------------------------------------------------------------

#Schaake

# Don't like the NA Check in the observations at the moment
# There is no NA in the rainfall data

na_obs = any(is.na(Y_obs))
if(na_obs == TRUE){stop("Handle NAs")}
warning("Need to handle possible missing dates and times - might just need to rewrite documentation")

possible_dates = names(Y_obs) %>% setdiff("HH")
X_raw_schaake = X_raw

# Get climate template
Y_climate <- lapply(possible_dates, function(date_val, dates){

  dates = lubridate::as_date(dates)
  sampled_dates <- depPPR::sample_schaake_dates(num_draws = num_members,
                                                dates = dates,
                                                date_val = lubridate::as_date(date_val),
                                                window = window_radius)
  if(all(is.na(sampled_dates))) warning(paste("No matching date", date_val))

  col_i = which(colnames(Y_obs_by_forecast) %in% as.character(sampled_dates))
  Y_climate <- Y_obs_by_forecast[, col_i]
  return(Y_climate)
}, dates = possible_dates)
names(Y_climate) = possible_dates
warning("lapply doesn't pass the dates propely")
warning("scoping is local")

Y_schaake <- lapply(1:length(Y_uni_eccQ), function(i){
  print(i)
  Y_schaake_shuffle <- tryCatch(depPPR::schaake_shuffle(as.matrix(Y_uni_eccQ[[i]]),
                                   as.matrix(Y_climate[[i]])),
           error =  function(e){ print(e); return(NA)})
  return(Y_schaake_shuffle)
})
names(Y_schaake) = names(X_raw)

# -----------------------------------------------------------------------------

# Scoring
num_obs_col = ncol(Y_obs_by_forecast)
obs_list <- lapply(as.list(2:num_obs_col), # 2 for lead time not robust
                   function(i, Y_obs){
                     return(Y_obs[,i] %>% unlist() %>% as.numeric())},
                   Y_obs = Y_obs_by_forecast)
names(obs_list) = names(Y_obs_by_forecast)[-1] # -1 for lead time

d = length(obs_list[[1]])
# w_vs <- matrix(NA, nrow = d, ncol = d)
# for(d1 in 1:d){for(d2 in 1:d){w_vs[d1,d2] <- 0.5^abs(d1-d2)}}

dependence_types = c("ECC-Q", "ECC-R",  "Schaake",
                     "Uni-PP", "Raw")

num_lead_times = Y_obs_by_forecast$lead_time %>% length()
lead_time_subsets = list(1:10, 11:20, 21:30, 31:40, 1:40)
len = length(lead_time_subsets)
plot_list = vector("list", len)
for(l in 1:len){

  filtering = lead_time_subsets[[l]]


score_df <- NULL
for(i in 1:length(obs_list)){

  print(names(obs_list)[i])
  y = obs_list[[i]] %>% unlist() %>% as.vector()
  y = y[filtering]
  if(any(is.na(y))) next

  date_val  = names(obs_list)[i]
  for(dep_type in dependence_types){

    if(dep_type == "ECC-Q") list_obj = Y_multi_eccQ
    if(dep_type == "ECC-R") list_obj = Y_multi_eccR
    if(dep_type == "Schaake") list_obj = Y_schaake
    if(dep_type == "Uni-PP") list_obj = Y_uni_eccR
    if(dep_type == "Raw") list_obj = X_raw

    j =  which(names(list_obj) == date_val)
    if(length(j) != 1) next

    dat = list_obj[[j]] %>% as.matrix()
    if(any(is.na(dat))) next
    if(nrow(dat) != d) next

    dat = dat[filtering, ]
    vs_score = vs_sample(y = y, dat = dat)# , w = w_vs)
    es_score = es_sample(y = y, dat = dat)
    score_df_i <- data.frame(vs  = vs_score,
                             es = es_score,
                             date = date_val,
                             dep_type = dep_type)
    score_df <- rbind(score_df, score_df_i)
  }

}

score_df %>%
  group_by(dep_type) %>%
  summarise(vs_med = median(vs),
            es_med = median(es),
            vs_mean = mean(vs),
            es_mean = mean(es)) %>%
  ungroup() %>%
  arrange(desc(vs_med))

plot_score_df <- score_df
x_axis_max <- quantile(score_df$vs, 0.99)

score_plot <- ggplot(plot_score_df, aes(vs, col = dep_type)) +
  # geom_boxplot(aes(y = vs)) +
  geom_density(aes(fill = dep_type), alpha = 0.2)+
  # aes(fill = vs),  position = "fill", alpha = 0.2
  # xlim(c(0, 10)) +
  scale_fill_discrete(name = "Dependence") +
  scale_color_discrete(name = "Dependence") +
  xlab("Variogram Score") +
  xlim(c(0, x_axis_max)) +
  ylab("Density") +
  ggtitle(paste(Y_obs_by_forecast$lead_time[filtering] %>% range(), collapse = "-")) +
  theme_bw()

plot_list[[l]] = score_plot

}

library(patchwork)
(plot_list[[1]] + plot_list[[2]]) /
(plot_list[[3]] + plot_list[[4]])
# -----------------------------------------------------------------------------
#
# d = nrow(day_obs)
# w_vs <- matrix(NA, nrow = d, ncol = d)
# for(d1 in 1:d){for(d2 in 1:d){w_vs[d1,d2] <- 0.5^abs(d1-d2)}}
#
# p_score_pp = vs_sample(y = day_obs$sur,
#                     dat = day_pp %>%
#                       pivot_wider(names_from = member, values_from = wsur) %>%
#                       select(starts_with('dm')) %>%
#                       as.matrix())#,
#                     # w = w_vs)
#
# p_score_ecc = vs_sample(y = day_obs$sur,
#                        dat = day_ecc %>%
#                          pivot_wider(names_from = member, values_from = wsur) %>%
#                          select(starts_with('rm')) %>%
#                          as.matrix())#,
#                        # w = w_vs)
#
# p_score_raw = vs_sample(y = day_obs$sur,
#                        dat = day_raw %>%
#                          pivot_wider(names_from = member, values_from = wsur) %>%
#                          select(starts_with('fc')) %>%
#                          as.matrix())#,
#                        # w = w_vs)
#
# p_score_raw
# p_score_pp
# p_score_ecc
#
# # -----------------------------------------------------------------------------
# library(plotly)
# d <- highlight_key(plot_data, ~member)
# compare_plot <- ggplot(d) +
#   geom_point(aes(x = lead_time, y = wsur, group = member),
#              col = "gray", alpha = 0.5) +
#   geom_line(aes(x = lead_time, y = wsur, group = member),
#             col = "gray", alpha = 0.5) +
#   geom_line(data = rbind(day_obs %>% mutate(dep_type = "ECC"),
#                          day_obs %>% mutate(dep_type = "Post-processed"),
#                          day_obs %>% mutate(dep_type = "Raw Ensemble")) %>%
#               mutate(member =  "Obs"),
#             aes(x = lead_time,
#                 y = sur),
#             col = "black", linetype =  "dotted") +
#   facet_wrap(~dep_type) +
#   theme_bw() +
#   theme(legend.position = "none")
# gg <- ggplotly(compare_plot, tooltip = "member")
# highlight(gg, dynamic = TRUE)
#
# saveRDS(plot_data, "/Users/katesaunders/Desktop/plot_data.rds")

# -----------------------------------------------------------------------------


