
get_clusters_above_threshold <- function(cluster_col, date_col, time_diff = 10,
                                         min_period = 60, min_cluster_size = 60){

  min_period = min_period/time_diff
  # assumes min_period is in minutes
  min_cluster_size = min_cluster_size/time_diff
  # assumes min_cluster_size is in minutes

  if(min_period > 0){
    merge_clusters = cluster_col %>%
      rle() %>%
      unclass() %>%
      as.data.frame() %>%
      mutate(values = ifelse(lengths < min_period & values == FALSE,
                             TRUE,
                             values))
    cluster_col = rep(merge_clusters$values, times = merge_clusters$lengths)
  }

  if(min_cluster_size > 0){
    rm_small_clusters = cluster_col %>%
      rle() %>%
      unclass() %>%
      as.data.frame() %>%
      mutate(values = ifelse(lengths < min_cluster_size & values == TRUE,
                             FALSE,
                             values))
    cluster_col = rep(rm_small_clusters$values, times = rm_small_clusters$lengths)
  }

  # Time period between row entries is 10 minutes
  clusters = cluster_col %>%
    rle() %>%
    unclass() %>%
    as.data.frame() %>%
    mutate(end = cumsum(lengths),
           start = c(1, dplyr::lag(end)[-1] + 1)) %>%
    mutate(date_end = date_col[end]) %>%
    mutate(date_start = date_col[start]) %>%
    arrange(desc(lengths)) %>%
    mutate(temp = paste(0, lengths*time_diff, 0, sep = ":")) %>%
    mutate(time = lubridate::hms(temp, roll = TRUE)) %>%
    select(-temp) %>%
    filter(values == TRUE)

  print("Need to specify a minimum interarrival time for discharge water")
  print("Need to check how rle() handles missing data")

  return(clusters)
}

get_summary_per_cluster <- function(clusters, height_data, type, ...){

  if(!(all(c("start", "end") %in% names(clusters))))
    stop("Need names columns for the start and end of the cluster")

  if(type == "max"){

    summary_details = apply(clusters, 1, function(r, height_data, col_names){
      i1 = r[which(col_names == "start")]
      i2 = r[which(col_names == "end")]
      height_data[i1:i2,] %>%
        apply(., 2, max)
    }, height_data, col_names = names(clusters)) %>%
      t()

  }else if(type == "risktime"){

    summary_details = apply(clusters, 1, function(r, height_data, h_t_level, col_names){
      i1 = r[which(col_names == "start")] %>% as.numeric()
      i2 = r[which(col_names == "end")] %>% as.numeric()
      perc = sum(height_data[i1:i2] > h_t_level)/sum(!is.na(height_data[i1:i2]))
    },
    height_data = height_data,
    col_names = names(clusters),
    h_t_level = h_t_level) %>%
      t()

  }else{
    stop("Type incorrectly specified")
  }

  return(summary_details)

}
