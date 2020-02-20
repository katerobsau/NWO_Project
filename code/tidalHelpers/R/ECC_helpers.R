# ECC_functions

# draws the members from the forecast distribution
Draw_members <- function(num_draws, par_info, draw_type = "random"){

  # assumed a NGR fit so par_info has columns mu and sigma
  if(draw_type == "random")
  drawn_members <- mapply(rnorm, mean = par_info$mu, sd = par_info$sigma,
                          MoreArgs = list(n = num_draws)) %>%
    t() %>%
    as.data.frame()

  if(draw_type == "uniform"){
    quantiles = (1:num_draws) / (num_draws + 1)
    drawn_members <- mapply(qnorm, mean = par_info$mu, sd = par_info$sigma,
                            MoreArgs = list(p = quantiles)) %>%
      t() %>%
      as.data.frame()
  }

  # named the columns dm for drawn members
  col_names <- formatC(1:num_draws, flag = "0", width = 3) %>%
    paste0("dm", .)
  names(drawn_members) <- col_names

  # inherit the date and lead_time from the drawn members
  if(!all(c("date", "lead_time") %in% names(par_info)))
    stop("Column names not compatible")
  drawn_members <- drawn_members %>%
    mutate(date = par_info$date, lead_time = par_info$lead_time)
  return(drawn_members)

}

# assumes a rank template
Get_template  <- function(raw_ensemble){

  if(!all(c("date", "lead_time") %in% names(raw_ensemble)))
    stop("Column names not compatible")

  # rank members from the ensemble
  template = apply(
    raw_ensemble %>% dplyr::select(-date, -lead_time), 1, rank) %>%
    t() %>%
    as.data.frame()

  # name the columns of these ranked members "tm" for template
  col_names <- formatC(1:ncol(template), flag = "0", width = 3) %>%
    paste0("tm", .)
  names(template) <- col_names

  # Combine with lead times
  template <- template %>%
    mutate(date = raw_ensemble$date, lead_time = raw_ensemble$lead_time)

  return(template)
}

# Transforms the drawn members using the template
Apply_ECC <- function(ecc_data){

  reordered <- lapply(1:nrow(ecc_data), function(i, ecc_data){

    draws = ecc_data %>%
      select(starts_with("dm")) %>%
      slice(i) %>%
      as.numeric()

    template = ecc_data %>%
      select(starts_with("tm")) %>%
      slice(i) %>%
      as.numeric()

  reorder = sort(draws)[template]

  return(reorder)
  },ecc_data = ecc_data) %>%
    do.call("rbind", .) %>%
    as.data.frame()

  # label the columns as "rm" for reordered members
  col_names <- formatC(1:ncol(reordered), flag = "0", width = 3) %>%
    paste0("rm", .)
  names(reordered) <- col_names

  # append the date and lead_time columns
  if(!all(c("date", "lead_time") %in% names(raw_ensemble)))
    stop("Column names not compatible")
  reordered = reordered %>%
    mutate(date = ecc_data$date, lead_time = ecc_data$lead_time)

  return(reordered)
}

# -----------------------------------------------------------------------------

# Schaake
draw_dates <- function(num_draws, date_val, dates,
                       window = 7, repulsion = FALSE){

  # assumes all dates have valid observational data

  window_dates = seq(date_val - window, date_val + window, by = "days")
  years = lubridate::year(dates) %>% unique()
  all_window_dates <- lapply(years, function(year, window_dates){
      lubridate::year(window_dates) <- year
      return(window_dates)
  }, window_dates = window_dates) %>%
    do.call("c", .) %>%
    intersect(dates) %>%
    lubridate::as_date()

  if(length(all_window_dates) < num_draws) return(rep(NA, num_draws))

   sampled_dates = sample(all_window_dates, num_draws)

  return(sampled_dates)

}

rank_members <- function(M, ties.method =  "first"){

  M_ranked <- apply(M, 1, rank, ties.method =  "first") %>%
    t()

  return(M_ranked)

}

interpolate_temporal_missing_values <- function(M){

  M <- as.matrix(M)
  missing_ij <- which(is.na(M), arr.ind = T)

  fill_ij <- apply(missing_ij, 1, function(row_ij, M){

    r = row_ij[1]; c = row_ij[2]

    member_rep <- matrix(rep(M[,c], ncol(M)), nrow = nrow(M), byrow = FALSE)
    ordered_mse_members = (M - member_rep)^2 %>% colMeans(na.rm = TRUE) %>% order()
    select_member = 2
    closest_member = ordered_mse_members[select_member]

    fill_val= M[r, closest_member]
    while(is.na(fill_val) & select_member <= ncol(M)){
      select_member = select_member + 1
      closest_member = ordered_mse_members[select_member]
    }

    return(fill_val)

  }, M)

  M[missing_ij] = fill_ij

  # should probably exception handle all NA rows somewhere

  return(M)

}

order_members <- function(M){

  if(any(is.na(M))) stop("Matrix must not have missing values")

  B <- apply(Y, 1, order) %>% t()

  return(B)
}

sort_members <- function(M){

  if(is.matrix(M)) stop("M must be a matrix")

  M_sort <- apply(M, 1, sort) %>% t()

  return(M_sort)

}

reorder_members <- function(M, B){

  if(!is.matrix(M) | !is.matrix(B))
    stop("M and B should both be matrices")

  M_new <- sapply(1:nrow(M), function(i,M,B){
    M[i,][B[i,]]
  }, M = M, B = B) %>% t()

  return(M_new)

}

schaake_shuffle <- function(X, Y){

  Chi = sort_members(X)
  # gamma = sort_members(Y)

  # B = order_members(Y_inter)
  B_map = rank_members(Y)

  X_ss = reorder_members(Chi, B_map)

  return(X_ss)

}

### ----------------------------------------------------------

# member_dates <- lapply(dates[1:2], draw_dates, num_draws = num_draws, dates = dates,
#        window = 15)

good_dates <- obs_df %>%
  group_by(date) %>%
  summarise(count = sum(is.na(sur))) %>%
  filter(count < 3) %>%
  pull(date) %>%
  unique()
print("Warning: hard code for NA threshold")

dates = good_dates

# recycling code from ECC
shuffles <- vector("list")
for(i  in 1:length(dates)){

  print(i)
  date_val = dates[i]
  X = drawn_members %>% filter(date == date_val) %>% select(-date, -lead_time)
  set.seed(1)
  member_dates <- draw_dates(date_val = dates[i],
                           num_draws = num_draws, dates = dates,
                           window = 15)
  Y = obs_df %>%
    filter(date %in% member_dates) %>% # index hard code
    pivot_wider(values_from = sur, names_from = date) %>%
    select(-lead_time)

  # simple NA handling
  if(any(is.na(Y))){ Y = interpolate_temporal_missing_values(Y)  }
  if(any(is.na(Y))){ stop("Skipped as NAs present"); next }

  X_ss <- schaake_shuffle(X, Y)
  shuffles[[i]] = X_ss

}

#  let's score them!
shuffle_scores = NULL
for(i  in 1:length(dates)){

  date_val = dates[i]
  y = obs_df %>% filter(date == date_val) %>% pull(sur)
  if(any(is.na(y))) next
  if(is.null(shuffles[[i]])) {
    print("Why is this NULL?");
    next
  }
  p_score = vs_sample(y = y,
                      dat = shuffles[[i]])#,

  shuffle_scores = rbind(shuffle_scores,
                         data.frame(date =  date_val,
                         vs = p_score))

}

### ----------------------------------------------------------

# had to run scripts from get_ensemble_summary_stats_by_lead_time.R and NGR_functions.R
# got to work on reproducibility

num_members = 50

raw_ensemble <- ensemble_winter %>%
  dplyr::select(date, t, member, wsur) %>%
  pivot_wider(names_from = c("member"), values_from = "wsur") %>%
  mutate(lead_time = t) %>%
  dplyr::select(starts_with("fc"), "date", "lead_time")
par_info = pars %>% mutate(date = dates)

set.seed(1)

drawn_members <- Draw_members(num_draws = num_members,
                              par_info = par_info,
                              draw_type = "random")

ecc_template <- Get_template(raw_ensemble = raw_ensemble)

ecc_data <- full_join(drawn_members, ecc_template,
                      by = c("date", "lead_time")) %>%
  dplyr::filter(!is.na(dm001) & !is.na(tm001)) # Why did I do this?

ecc_reordered <- Apply_ECC(ecc_data = ecc_data)

### ----------------------------------------------------------

date_val = "2013-11-26" #ecc_reordered$date[i];

day_pp <- ecc_data %>%
  filter(date == date_val) %>%
  mutate(dep_type = "pp") %>% # pp for marginal post-processed, without dependence
  dplyr::select(-starts_with("tm")) %>%
  pivot_longer(cols = starts_with("dm"),
               names_to = "member", values_to = "wsur")

day_ecc <- ecc_reordered %>%
  filter(date == date_val) %>%
  mutate(dep_type = "ecc") %>% # ecc for post-processed with dependence
  pivot_longer(cols = starts_with("rm"),
               names_to = "member", values_to = "wsur")

day_raw <- raw_ensemble %>%
  filter(date == date_val) %>%
  mutate(dep_type = "raw") %>% # raw for raw ensemble
  pivot_longer(cols = starts_with("fc"),
               names_to = "member", values_to = "wsur")

day_obs <- ensemble_winter %>%
  filter(date == date_val) %>%
  mutate(lead_time = t) %>%
  select(date, lead_time, sur) %>%
  distinct()

plot_data = full_join(day_pp, day_ecc) %>%
  full_join(day_raw) %>%
  mutate(member = member %>% str_match_all("[0-9]+") %>% as.numeric()) %>%
  mutate(dep_type = as.factor(dep_type))
levels(plot_data$dep_type) = c(ecc = "ECC", pp = "Post-processed",
                               raw = "Raw Ensemble")
compare_plot <- ggplot(data = plot_data) +
  geom_point(aes(x = lead_time, y = wsur, group = member,
                 col = as.factor(member)),
            alpha = 0.5) +
  geom_line(aes(x = lead_time, y = wsur, group = member,
            col = as.factor(member)),
             alpha = 0.5) +
  geom_line(data = rbind(day_obs %>% mutate(dep_type = "ECC"),
                         day_obs %>% mutate(dep_type = "Post-processed"),
                         day_obs %>% mutate(dep_type = "Raw Ensemble")) %>%
              mutate(member =  "Obs"),
            aes(x = lead_time,
                y = sur), col = "black") +
  facet_wrap(~dep_type) +
  theme_bw() +
  theme(legend.position = "none")

compare_plot
# -----------------------------------------------------------------------------

d = nrow(day_obs)
w_vs <- matrix(NA, nrow = d, ncol = d)
for(d1 in 1:d){for(d2 in 1:d){w_vs[d1,d2] <- 0.5^abs(d1-d2)}}

p_score_pp = vs_sample(y = day_obs$sur,
                    dat = day_pp %>%
                      pivot_wider(names_from = member, values_from = wsur) %>%
                      select(starts_with('dm')) %>%
                      as.matrix())#,
                    # w = w_vs)

p_score_ecc = vs_sample(y = day_obs$sur,
                       dat = day_ecc %>%
                         pivot_wider(names_from = member, values_from = wsur) %>%
                         select(starts_with('rm')) %>%
                         as.matrix())#,
                       # w = w_vs)

p_score_raw = vs_sample(y = day_obs$sur,
                       dat = day_raw %>%
                         pivot_wider(names_from = member, values_from = wsur) %>%
                         select(starts_with('fc')) %>%
                         as.matrix())#,
                       # w = w_vs)

p_score_raw
p_score_pp
p_score_ecc

# -----------------------------------------------------------------------------
library(plotly)
d <- highlight_key(plot_data, ~member)
compare_plot <- ggplot(d) +
  geom_point(aes(x = lead_time, y = wsur, group = member),
             col = "gray", alpha = 0.5) +
  geom_line(aes(x = lead_time, y = wsur, group = member),
            col = "gray", alpha = 0.5) +
  geom_line(data = rbind(day_obs %>% mutate(dep_type = "ECC"),
                         day_obs %>% mutate(dep_type = "Post-processed"),
                         day_obs %>% mutate(dep_type = "Raw Ensemble")) %>%
              mutate(member =  "Obs"),
            aes(x = lead_time,
                y = sur),
            col = "black", linetype =  "dotted") +
  facet_wrap(~dep_type) +
  theme_bw() +
  theme(legend.position = "none")
gg <- ggplotly(compare_plot, tooltip = "member")
highlight(gg, dynamic = TRUE)

saveRDS(plot_data, "/Users/katesaunders/Desktop/plot_data.rds")

# -----------------------------------------------------------------------------
library(scoringRules)

obs_df <- ensemble_winter %>%
  mutate(lead_time = t) %>%
  dplyr::select(date, lead_time, sur) %>%
  distinct()

  # filter(date == date_val) %>%

dates_iter <- ecc_reordered %>% pull(date) %>% unique()
p_score_df <- NULL
unique_lead_times =obs_df$lead_time %>% unique()
window_len = 5
t_roll = sapply(1:(length(unique_lead_times)-window_len),
           function(i){unique_lead_times[i:(i+5)]}) %>%
  as.matrix() %>% t()
for(dep_type in c("ecc", "raw")){#, "pp","raw")){

  if(dep_type == "pp")
    dep_df <- ecc_data %>%
      select(starts_with("dm"), date, lead_time) %>%
      left_join(obs_df, by = c("date", "lead_time"))

  if(dep_type == "ecc")
    dep_df <- ecc_reordered %>%
      left_join(obs_df, by = c("date", "lead_time"))

  if(dep_type == "raw")
    dep_df <- raw_ensemble %>%
      left_join(obs_df, by = c("date", "lead_time"))

for(i in 1:length(dates_iter)){
  print(i)
  date_val = dates_iter[i]

  vs_dat = dep_df %>%
    filter(date == date_val)

  if(nrow(vs_dat) == 0) next

  # weighting matrix for variogram score
  # d = nrow(vs_dat)
  # w_vs <- matrix(NA, nrow = d, ncol = d)
  # for(d1 in 1:d){for(d2 in 1:d){w_vs[d1,d2] <- 0.5^abs(d1-d2)}}

  vs_orig = vs_dat
  for(r in 1:nrow(t)){
  vs_dat = vs_orig %>%
    filter(lead_time %in% t[r,])

  if(nrow(vs_dat) == 0) next
  p_score = vs_sample(y = vs_dat$sur,
                      dat = vs_dat %>% select(-lead_time, -date, -sur) %>% as.matrix())#,
                      # w = w_vs)

  p_score_df <- rbind(p_score_df,
                 data.frame(date = date_val,
                            vs = p_score,
                            dep_type,
                            t = r))
  }

}}
#
# p_score_df %>% filter(dep_type == "raw") %>% pull(vs) %>% summary()
# p_score_df %>% filter(dep_type == "pp") %>% pull(vs) %>% summary()
# p_score_df %>% filter(dep_type == "ecc") %>% pull(vs) %>% summary()


ggplot(p_score_df, aes(vs, col = dep_type)) +
  geom_density(aes(fill = dep_type), alpha = 0.2) +
  # aes(fill = vs),  position = "fill", alpha = 0.2
  xlim(c(0, 10)) +
  theme_bw()

ggplot(p_score_df, aes(vs, col = as.factor(dep_type))) +
  geom_density() +
  facet_wrap(~t) +
  # aes(fill = vs),  position = "fill", alpha = 0.2
  xlim(c(0, 10)) +
  theme_bw()

# -----------------------------------------------------------

# distances <- function(src, target) {
#   outer(seq_len(nrow(src)), seq_len(nrow(target)), sq_eucl_dist,
#         src = src, target = target)
# }
#
# OA <- distances(data_test$pred, data_train$pred) %>%
#   (function(.) assignments[apply(., 1, which.min)]) %>%
#   (function(.) data_train$obs[., ])

# library(magrittr)
# df = data.frame(x = 1:10, y = 1:10)
# df %>%
#   (function(.){
#     names(.) = c("a", "b");
#     # . = . + 1
#     return(.)}
#   )
# df
