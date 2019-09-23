#' @export
#'
read_tide_data <- function(date_val, lead_time, member_type,
                           data_dir, pre_ext = "WAQE_PLT", post_ext = "LC"){

  print("Note sure which input is leadtime")

  if(member_type == "CTRL"){
    zero_pad = "0000"
  }else{
    zero_pad = "00"
  }

  file_name = paste(data_dir, pre_ext, "_", date_val, zero_pad, "_", lead_time, "_", post_ext, "_", member_type, sep = "")

  df <- read_table2(file_name, col_names = FALSE)

  return(df)

}

#' @export
#'
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

#' @export
#'
add_tide_category <- function(df){

  print("FIX CATG VAR")

  print("Double check harm is the right observation variable")

  if(!("harm" %in% names(df))) stop("Missing harm: the observed astronomical tide")

  ### Add variable for tide
  df <- df %>%
    mutate(catg = if_else( (harm - lag(harm)) > 0 , "in", "extrema" )) %>%
    mutate(catg = if_else( (harm - lag(harm)) < 0 , "out", catg ))
  # need to flag extrema properly

  df$catg[1] = df$catg[2]

  return(df)
}

#' @export
#'
add_peak_tide_category <- function(tide_data){

  print("FIX CATG VAR")

  temp = tide_data$catg
  i = which(temp == "extrema")
  prior_catg = temp[i - 1]
  prior_catg[prior_catg == "in" | prior_catg == "high"] = "high"
  prior_catg[prior_catg == "out" | prior_catg == "low"] = "low"
  temp[i] = prior_catg

  lag_val = 2
  while(any("extrema" %in% temp)){
    i = which(temp == "extrema")
    prior_catg = temp[i - lag_val]
    prior_catg[prior_catg == "in" | prior_catg == "high"] = "high"
    prior_catg[prior_catg == "out" | prior_catg == "low"] = "low"
    temp[i] = prior_catg
    lag_val = lag_val - 1
  }

  tide_data$catg = temp

  return(tide_data)
}

#' @export
#'
set_na <- function(df, na_value = 99.99){

  if(!("sur" %in% names(df))) stop("Missing sur: observational surge")
  if(!("wsur" %in% names(df))) stop("Missing wsur: forecast surge variable")

  df <- df %>%
    mutate(sur = if_else(sur == na_value, NA_real_, sur)) %>%
    mutate(wsur = if_else(wsur == na_value, NA_real_, wsur))

  return(df)
}

#' @export
#'
preprocess_tide_data <- function(df){

  df <- name_vars(df)

  df <- add_tide_category(df)

  # df <- add_peak_tide_category(df)
  print("Need to fix lag in tide category")

  df <- set_na(df)

  return(df)
}
