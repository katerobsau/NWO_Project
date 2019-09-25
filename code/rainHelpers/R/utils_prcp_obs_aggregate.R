Get_obs_aggr <- function(num_days_aggr, daily_obs){

  if(num_days_aggr < 1 | !is.integer(num_days_aggr))
    stop("Please provide valid number of days")

  RH <- daily_obs$RH
  if(num_days_aggr == 1) return(RH)

  aggr_obs <- sapply(1:(num_days_aggr - 1), dplyr::lag, x = RH) %>%
    cbind(., RH) %>%
    rowSums()

  return(aggr_obs)

}

Wrapper_obs_aggr <- function(total_num_days_for_aggr, daily_obs){

  aggr_daily_obs <- sapply(1:total_num_days_for_aggr, Get_obs_aggr,
                           daily_obs = daily_obs) %>%
    as.data.frame()

  names(aggr_daily_obs) <- paste("aggr_obs", 1:total_num_days_for_aggr, sep = "_")

  aggr_daily_obs$Date <- daily_obs$Date

  return(aggr_daily_obs)

}
