# DAILY READS
Read_daily_stn_obs <- function(file_name = "pgdcnNLE00152495.txt", file_dir){

  file_path = paste(file_dir, file_name, sep = "")
  obs_data <- read_table2(file = file_path, col_names = FALSE, skip = 19)
  names(obs_data) <- c("year", "month", "day", "prcp")
  return(obs_data)

}

# SUBDAILY READS
Get_subdaily_file_name <- function(start_year, prefix = "uurgeg",
                                   stn_number = 270, suffix = ".txt"){
  year_str = paste(start_year, "-", start_year + 9, sep = "")
  sub_daily_file = paste(prefix, stn_number, year_str, sep = "_") %>%
    paste(. , ".txt", sep = "")
  return(sub_daily_file)
}

Read_subdaily_stn_obs <- function(file_name, file_dir){
  # manually unzipped original files (was quicker)

  file_path = paste(file_dir, file_name, sep = "")
  obs_data <- read_csv(file_path, skip = 30) %>%
    select("# STN", "YYYYMMDD", "DR", "RH")

  return(obs_data)

}

Address_negative_obs <- function(subdaily_obs){

  subdaily_obs <- subdaily_obs %>%
    mutate(RH = if_else(RH < 0 & DR <= 0, 0, RH))

  num_neg = sum(subdaily_obs$RH < 0, na.rm = TRUE)

  if(num_neg > 0){

    warning("Negative RH observations are present")
    warning(paste("Cohercing", num_neg, "these to zero values"))

    subdaily_obs <- subdaily_obs %>%
      mutate(RH = if_else(RH < 0, 0, RH))

  }

  return(subdaily_obs)

}

Convert_subdaily_to_daily <- function(subdaily_obs){

  subdaily_obs <- subdaily_obs %>%
    group_by(YYYYMMDD) %>%
    summarize(RH = sum(RH))

  return(subdaily_obs)

}

Use_std_date_format <- function(obs){

  ymd_format <- obs$YYYYMMDD %>%
    as.character() %>%
    lubridate::as_date()

  obs <- obs %>%
    mutate(Date = ymd_format) %>%
    select(-YYYYMMDD)

  return(obs)

}

Get_daily_obs <- function(years, data_dir){

  print("Data is binned into files files for each decade")

  start_years = years[which(years%%10 == 1)]
  if(start_years[1] != years[1]){
    start_years = c(years[1] - years[1]%%10 + 1, start_years)
  }

  daily_obs <- lapply(start_years, function(start_year, data_dir){

    decadal_obs <- Get_subdaily_file_name(start_year = start_year) %>%
      Read_subdaily_stn_obs(file_name = . , file_dir = data_dir) %>%
      Address_negative_obs(.) %>%
      Convert_subdaily_to_daily(.) %>%
      Use_std_date_format(.)

  }, data_dir = data_dir) %>%
    do.call(rbind, .)

  return(daily_obs)

}

# Add_hours_to_date_format <- function(subdaily_obs){
#
#   ymd_format <- subdaily_obs$YYYYMMDD %>%
#     as.character() %>%
#     lubridate::as_date()
#
#   dates = unique(ymd_format)
#   if(!all(table(ymd_format) == 24))
#     stop("For certain dates there are not 24 hours")
#
#   hms_format = paste(1:24, rep(0, 24), rep(0,24), sep = ":") %>%
#     lubridate::hms()
#
#   subdaily_obs$Date = ymd_format + hms_format
#
#   subdaily_obs <- select(subdaily_obs, -YYYYMMDD)
#
#   return(subdaily_obs)
#
# }
