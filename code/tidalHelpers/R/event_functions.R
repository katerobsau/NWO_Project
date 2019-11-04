NA_check <- function(df){
  df = df %>%
    dplyr::mutate(harm = ifelse(harm >= na_value, NA_real_, harm)) %>%
    dplyr::mutate(sur = ifelse(sur >= na_value, NA_real_, sur)) %>%
    dplyr::mutate(obs = ifelse(obs >= na_value, NA_real_, obs)) %>%
    dplyr::mutate(wtot = ifelse(wtot >= na_value, NA_real_, wtot)) %>%
    dplyr::mutate(wtoc = ifelse(wtoc >= na_value, NA_real_, wtoc)) %>%
    dplyr::mutate(wsur = ifelse(wsur >= na_value, NA_real_, wsur))
  return(df)
}

get_peak <- function(df, type, var_name, member_ref){

  if(type == "ens"){

    if(missing(member_ref))
      member_ref = get_ensemble_ref(utils_init()$num_members)

    peak_data <- lapply(member_ref, function(member_name, var_name,
                                            df){

      member_data = df %>%
        dplyr::filter(member == member_name)

      peak_ind = member_data %>%
        pull(var_name) %>%
        which.max()

      if(length(peak_ind) > 1)
        warning("Multiple  peaks: Selecting the first")

      member_peak = member_data[peak_ind, ]

      return(member_peak)

    },var_name = var_name, df = df) %>%
      bind_rows()

  }else if(type == "obs"){

    peak_ind = df %>%
      pull(var_name) %>%
      which.max()

    if(length(peak_ind) > 1)
      warning("Multiple  peaks: Selecting the first")

    peak_data = df[peak_ind, ]

  }else{
    stop("Type reference is an invalid string")
  }

  return(peak_data)

}

get_duration <- function(df, type, threshold_value,
                         var_name, var_summary = "max", member_ref){

  if(type == "ens"){

    if(missing(member_ref))
      member_ref = get_ensemble_ref(utils_init()$num_members)

    cluster_summary <- lapply(member_ref, function(member_name,
                                               var_name,
                                               df,
                                               var_summary){

      date_col <- df %>% filter(member == member_name) %>% pull(t)
      var_col <- df %>% filter(member == member_name) %>% pull(var_name)
      bool_col = var_col > threshold_value

      member_clusters <- get_clusters_above_threshold(cluster_col = bool_col,
                                                      date_col = date_col) %>%
        mutate(member = member_name)

      if(var_summary == "max"){

        member_summary <- member_clusters %>%
          slice(which.max(lengths)) %>%
          as.data.frame() %>%
          mutate(member = member_name)

      }else if(var_summary == "total"){

        total_duration <- member_clusters %>%
          pull(lengths) %>%
          sum()

        member_summary <- data.frame(member = member_name,
                                     lengths = total_duration)
        # Need to think about missing data here

      }else{
        warning("var_summary provided is not a string match")
      }

    }, var_name = var_name, df = df, var_summary = var_summary) %>%
      do.call("rbind", .)

  }else if(type == "obs"){

    date_col <- df %>% pull(t)
    var_col <- df %>% pull(var_name)
    bool_col = var_col > threshold_value

    obs_clusters <- get_clusters_above_threshold(cluster_col = bool_col,
                                                    date_col = date_col) %>%
      mutate(member = "obs")

    if(var_summary == "max"){

      cluster_summary <- obs_clusters %>%
        slice(which.max(lengths)) %>%
        as.data.frame() %>%
        mutate(member = "obs")

    }else if(var_summary == "total"){

      total_duration <- obs_clusters %>%
        pull(lengths) %>%
        sum()

      cluster_summary <- data.frame(member = "obs",
                                   lengths = total_duration)
      # Need to think about missing data here

    }else{
      warning("var_summary provided is not a string match")
    }

  }else{

    stop("Type reference is an invalid string")

  }

  return(cluster_summary)

}

# A lot of reducancy above (would like to rewrite)

