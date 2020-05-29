# had to run scripts from get_ensemble_summary_stats_by_lead_time.R and NGR_functions.R
# got to work on reproducibility

num_members = 50
library(devtools)
install_github("katerobsau/depPPR", ref = "dev", force = TRUE)
library(depPPR)
library(scoringRules)
# library(depPPR, lib.loc = "/Users/katesaunders/Documents/Git/depPPR")

window_radius = 15
min_na_threshold = 3

#  ----------------------------------------------------------------------------

surge_ensemble_editted <- readRDS(file = "data/ensemble_filtered.rds")
pars <- readRDS(file = "data/surge_pars.rds")

ensemble_winter <- surge_ensemble_editted %>%
  mutate(date = str_sub(date, 1, 8) %>% lubridate::as_date()) %>%
  mutate(year = lubridate::year(date), month = lubridate::month(date),
         day = lubridate::day(date)) %>%
  filter(month >= 10 | month <= 4) %>%
  filter(!(month == 10 & day <= 15) & !(month == 4 & day >= 15)) %>%
  mutate(season = ifelse(month >= 10, year, year - 1))

raw_ensemble <- ensemble_winter %>%
  dplyr::select(date, t, member, wsur) %>%
  pivot_wider(names_from = c("member"), values_from = "wsur") %>%
  mutate(lead_time = t) %>%
  dplyr::select(starts_with("fc"), "date", "lead_time")

par_info = pars %>% mutate(date = dates) %>%
  mutate(mean = mu, sd = sigma)

Y_obs <- ensemble_winter %>%
  dplyr::select(sur, date, t) %>%
  distinct() %>%
  pivot_wider(values_from = sur, names_from = date) %>%
  dplyr::select(-t)
# Need to handle t vs lead_time better

#  ----------------------------------------------------------------------------

# Raw Ensemble
dates <- raw_ensemble %>% pull(date) %>% unique()
X_raw <- lapply(dates, function(date_val){
  raw_ensemble %>%
    dplyr::filter(date == date_val) %>%
    dplyr::select(starts_with("fc")) %>%
    as.matrix()
})
names(X_raw) = dates

#  ----------------------------------------------------------------------------

# ECC methods
set.seed(1)
Y_uni_eccR <- lapply(dates, function(date_val){
  depPPR::sample_ecc_members(num_members = num_members,
                          function_type = rnorm,
                          pars = par_info %>%
                            dplyr::filter(date == date_val) %>%
                            dplyr::select(mean, sd),
                          ecc_type = 'R')
})
names(Y_uni_eccR) = dates

Y_uni_eccQ <- lapply(dates, function(date_val){
  depPPR::sample_ecc_members(num_members = num_members,
                          function_type = qnorm,
                          pars = par_info %>%
                            dplyr::filter(date == date_val) %>%
                            dplyr::select(mean, sd),
                          ecc_type = 'Q')
})
names(Y_uni_eccQ) = dates

warning("Need to handle missing lead times")
Y_multi_eccR <- lapply(1:length(X_raw), function(i){
  print(i)
  tryCatch(depPPR::apply_ecc_template(X_raw[[i]], Y_uni_eccR[[i]]),
           error =  function(e) return(NA))
})
names(Y_multi_eccR) = dates

warning("Need to handle missing lead times")
Y_multi_eccQ <- lapply(1:length(X_raw), function(i){
  print(i)
  tryCatch(depPPR::apply_ecc_template(X_raw[[i]], Y_uni_eccQ[[i]]),
           error =  function(e) return(NA))
})
names(Y_multi_eccQ) = dates

#  ----------------------------------------------------------------------------

#Schaake

#NA Check
num_na_by_col =colSums(is.na(Y_obs))
rm_col = which(num_na_by_col > min_na_threshold)
if(length(rm_col) >  0){Y_obs_rm = Y_obs[ ,-rm_col]}

# NA Interpolation
if(any(is.na(Y_obs_rm))) Y_obs_int  = depPPR::interpolate_missing_values(Y_obs_rm)

# Get possible dates (filter X_raw to match)
possible_dates = colnames(Y_obs_int)
# not sure why this is colnames() not names()  and similarly below
i = which(names(X_raw) %in% possible_dates)
X_raw_schaake = X_raw[i]

# Get climate template
Y_climate <- lapply(possible_dates, function(date_val){

  sampled_dates <- depPPR::sample_schaake_dates(num_draws = num_members, dates = dates,
                                      date_val = lubridate::as_date(date_val),
                                      window = window_radius)
  col_i = which(colnames(Y_obs_int) %in% as.character(sampled_dates))
  Y_climate <- Y_obs_int[, col_i]
  return(Y_climate)
})
names(Y_climate) = possible_dates

warning("Need to handle missing lead times")
Y_schaake <- lapply(1:length(Y_uni_eccQ[-rm_col]), function(i){
  print(i)
  tryCatch(depPPR::schaake_shuffle(Y_uni_eccQ[-rm_col][[i]], Y_climate[[i]]),
           error =  function(e) return(NA))
})
names(Y_schaake) = possible_dates

# -----------------------------------------------------------------------------

# Scoring
obs_list <- lapply(as.list(1:ncol(Y_obs)),
                  function(i, Y_obs){
                    return(Y_obs[,i] %>% unlist() %>% as.numeric())},
                  Y_obs)
names(obs_list) = dates

d = length(obs_list[[1]])
# w_vs <- matrix(NA, nrow = d, ncol = d)
# for(d1 in 1:d){for(d2 in 1:d){w_vs[d1,d2] <- 0.5^abs(d1-d2)}}

dependence_types = c("ECC-Q", "ECC-R",  "Schaake",
                     "Uni-PP", "Raw")

lead_times = unique(raw_ensemble$lead_time)

filtering = 11:19
d = length(filtering)
w_vs <- matrix(1, nrow = d, ncol = d)
max_d_diff = 10
# for(d1 in 1:d){for(d2 in 1:d){
#   d_dist = d1 - d2
#   if(d_dist > max_d_diff) d_dist = 0
#   w_vs[d1,d2] <- 0.5^abs(d_dist)
# }}

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
    if(dep_type == "Uni-PP") list_obj = Y_uni_eccQ
    if(dep_type == "Raw") list_obj = X_raw

    j =  which(names(list_obj) == date_val)
    if(length(j) != 1) next

    dat = list_obj[[j]]
    if(any(is.na(dat))) next
    if(nrow(dat) != length(lead_times)) next

    dat = dat[filtering, ]
    if(nrow(dat) != d) next
    vs_score = vs_sample(y = y, dat = dat , w = w_vs)
    es_score = es_sample(y = y, dat = dat)
    score_df_i <- data.frame(vs  = vs_score, es = es_score,
                           date = date_val,
                        dep_type = dep_type)
    score_df <- rbind(score_df, score_df_i)
  }

}

score_df %>%
  group_by(dep_type) %>%
  summarise(vs_med = median(vs),
            es_med = median(es)) %>%
  ungroup() %>%
  arrange(vs_med)

plot_score_df <- score_df  #%>%
  # filter(vs <= 10 & es <= 10)
x_axis_max <- quantile(score_df$es, 0.99)

ggplot(plot_score_df, aes(es, col = dep_type)) +
  # geom_boxplot(aes(y = vs)) +
  geom_density(aes(fill = dep_type), alpha = 0.2)+
  # aes(fill = vs),  position = "fill", alpha = 0.2
  scale_fill_discrete(name = "Dependence") +
  scale_color_discrete(name = "Dependence") +
  xlab("Energy Score") +
  xlim(c(0, x_axis_max)) +
  ylab("Density") +
  ggtitle(paste(lead_times[filtering] %>% range(), collapse = "-")) +
  theme_bw()

# -----------------------------------------------------------------------------
#
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


