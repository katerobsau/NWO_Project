Get_cols_for_lead_times <- function(ens_data, period_start = 0, period_end = 24){

  lead_times = names(ens_data) %>%
    str_extract(., "[0-9]+") %>%
    setdiff(NA) %>%
    as.numeric()

  if(period_end > lead_times[length(lead_times)]){
    warning("period_end specified greater than possible lead times")
  }

  col_names =
    lead_times[lead_times > period_start & lead_times <= period_end] %>%
    paste(., "h", sep = "")

  return(col_names)

}

Get_aggr_for_lead_times <- function(ens_data, col_names){

  aggr_rf <- ens_data %>%
      select(col_names) %>%
      do.call('cbind', .) %>%
      rowSums()

  return(aggr_rf)

}

Get_member_summary <- function(ens_data, summary_type){

  member_data <- ens_data %>%
    filter(!(members %in% c("HR", "CRTL"))) %>%
    select(-members)

  summary_stat <- switch(
    summary_type,
    mean = colMeans(member_data),
    median = apply(member_data, 2, median),
    highest = apply(member_data, 2, max),
    high_res = ens_data %>% filter(members == "HR"),
    ctrl = ens_data %>% filter(members == "CTRL")
  )

  return(summary_stat)

}




# Select_to_ts <- function(var_df){
#
#   var_ts <- var_df %>% unlist() %>% as.numeric()
#   return(var_ts)
#
# }
#
# Aggregate_prcp_obs <- function(df, num_days_aggregated, var_name = "prcp"){
#
#   var_select = df %>%
#     select(var_name)
#
#   if(num_days_aggregated == 1){
#     df$aggregated <- var_select
#     return(df)
#   }
#
#   var_ts <- var_select %>% Select_to_ts()
#   for(i in 1:(num_days_aggregated-1)){
#     num_cols = ncol(var_select)
#     var_select[,i+1] <- dplyr::lag(var_ts, n = i)
#   }
#   aggregated_var <- rowSums(var_select)
#   df$aggregated = aggregated_var
#
#   return(df)
#
# }


# Get_daily_aggr <- function(ens_data, num_days_aggregated = 2,
#                            lead_times = seq(0,240,6),
#                            offset_time = 8){
#
#   if(offset_time != 8){
#     warning("KNMI observations are read at 8 am, confirm offset_time")
#   }
#
#   lead_diff = diff(lead_times) %>% unique()
#   if(length(lead_diff) != 1){
#     stop("Assumed lead times are at fixed intervals in time")
#   }else if(24%%lead_diff != 0){
#     stop("Assumed difference in lead time divides by 24")
#   }
#
#   num_hours_aggregated = num_days_aggregated*24
#
#   start_lead_time = lead_times[which.min(abs(lead_times - offset_time))]
#
# }

# This function assumes observations are read at 8 am and lead times are divisors of 24
Aggregate_prcp_ens_single_day <- function(ens_data, num_days_aggregated = 2,
                                          lead_times = seq(0,240,6),
                                          offset_time = 8){

  if(offset_time != 8){
    warning("KNMI observations are read at 8 am, confirm offset_time")
  }

  lead_diff = diff(lead_times) %>% unique()
  if(length(lead_diff) != 1){
    stop("Assumed lead times are at fixed intervals in time")
  }else if(24%%lead_diff != 0){
    stop("Assumed difference in lead time divides by 24")
  }

  num_hours_aggregated = num_days_aggregated*24

  start_lead_time = lead_times[which.min(abs(lead_times - offset_time))]
  end_lead_time = start_lead_time + num_hours_aggregated - lead_diff
  if(end_lead_time > max(lead_times)){
    stop("error: period of aggregation is too long")
  }

  select_lead_times <- seq(start_lead_time, end_lead_time, lead_diff)
  select_lead_times <- paste(select_lead_times, "h", sep = "")

  select_cols <- sapply(select_lead_times, function(x, ens_data){
    ens_data %>%
      select(`x`)
    }, ens_data = ens_data) %>%
    Select_to_ts()
  df_select <- matrix(select_cols, nrow = nrow(ens_data), byrow = FALSE) %>%
    as.data.frame()

  if(any(df_select < 0)){
    warning("Rainfall forecast was negative in places")
  }

  # aggr_rf <- data.frame(members = ens_data$members, aggregated = rowSums(df_select))
  aggr_rf <- rowSums(df_select)

  return(aggr_rf)

}
