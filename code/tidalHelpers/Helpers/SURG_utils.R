# -----------------------------------------------------------------------------

seperate_test_and_train_data <- function(df, g){

  y_train = df %>%
    dplyr::filter(GROUP_REF != g) %>%
    dplyr::filter(!is.na(OBS)) %>%
    dplyr::select(OBS) %>%
    as.matrix()

  X_train = df %>%
    dplyr::filter(GROUP_REF != g) %>%
    dplyr::filter(!is.na(OBS)) %>%
    dplyr::select(starts_with("PAR"))

  group_data = df %>%
    dplyr::filter(GROUP_REF == g)

  # raw_ens = df %>%
  #   dplyr::filter(GROUP_REF == g) %>%
  #   dplyr::select(starts_with("ENS"))

  X_test = df %>%
    dplyr::filter(GROUP_REF == g) %>%
    dplyr::select(starts_with("PAR"))

  y_test = df %>%
    dplyr::filter(GROUP_REF == g) %>%
    dplyr::select(OBS) %>%
    # dplyr::filter(!is.na(OBS)) %>%
    as.matrix()

  data_list = list(y_train = y_train, y_test = y_test,
                   X_train = X_train, X_test = X_test,
                   group_data = group_data)

  return(data_list)

}

# -----------------------------------------------------------------------------

loo_cv <- function(df, omit_groups = NULL){

  df_pred = NULL
  group_refs = unique(df$GROUP_REF) %>% setdiff(omit_groups)
  for(g in group_refs){

    loo_data <- seperate_test_and_train_data(df, g)
    model_fit <- fit_NGR(loo_data$y_train, loo_data$X_train)
    pred_from_fit <- predict_NGR(model_fit, loo_data$X_test)
    group_pred <- bind_cols(loo_data$group_data, pred_from_fit)
    df_pred <- bind_rows(df_pred, group_pred)

  }

  return(df_pred)

}

# -----------------------------------------------------------------------------

simulate_rnorm_ensemble <- function(pred, num_members = 50){

  sim_ensemble <- mapply(rnorm,
                         mean = pred$mu,
                         sd = pred$sigma,
                         USE.NAMES = TRUE,
                         MoreArgs = list(n = num_members)) %>%
    t() %>%
    as.data.frame() %>%
    set_names(paste("SIM", 1:num_members, sep = ""))

  return(sim_ensemble)

}

# -----------------------------------------------------------------------------

sample_climate_ensemble <- function(df, num_members = 50){

  # FIX ME!!! With equi-space quantiles
  clim_ensemble <- replicate(nrow(df),
                             sample(x = df$OBS[!is.na(df$OBS)], size = num_members)
  ) %>%
    t() %>%
    as.data.frame() %>%
    set_names(paste("CLIM", 1:num_members, sep = ""))
  return(clim_ensemble)

}

# -----------------------------------------------------------------------------

# FUNCTION FOR RENAMING THE DATA (PREPROCESS STEP)
name_vars <- function(df){
  # see surplo.1.pdf for further details
  col_names = c("t", "htid", "wtid",
                "wsur", "wtot", "wtoc",
                "wkal", "wtok", "wtkc",
                "harm", "obs", "sur")

  if(ncol(df) != length(col_names)) stop("data frame is not compatible for naming")
  names(df) = col_names

  # t: time with respect to dtg increased by h
  # htid: the astronomical tide harmonic analysis
  # wtid: the astronomical tide calculated by WAQUA
  # wsur: The surge calculated by WAQUA
  # wtot: the total water level <wsur> + <htid>
  # wtoc: the calculated water level <wsur> + <wtid>
  # wkal, wtok, wktc: kalmman calculation of the dual mode
  # harm: the astronomical tide
  # obs: observations from the WAQG_ODC database
  # sur: the observed surge <obs> - <harm>

  return(df)

}
