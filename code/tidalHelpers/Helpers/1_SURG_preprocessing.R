# FILE STRUCTURE
# Stn location > CTRL, ENS, HR
# ENS > date folders (double zero at end) > 50 ensemble forecasts in each folder
# CTRL > 50 ctrl forecasts (LAUW not HAR)
# HR > (should have) 50 HR forecasts but empty at the moment (LAUW not HAR)

# ITERATIONS
# iterations are needed over: date, lead time, members
# althought currently lead time is only 00000

# DATA NEEDED
# Observations for scoring
# Ensemble summary statistics(/covariates) for model fitting
# Members for dependence template

warning("Couple of assumptions/hardcodes based on this file structure")

# --------------------------------------------------------------------------------

#GET DATES FOLDERS

date_folders = list.files(tidal_ens_dir)

# --------------------------------------------------------------------------------

# START OF DATE WRAPPER
# For each folder read in the member files and combine them
missing_member_files = NULL
for(date_index in 1:length(date_folders)){

  print(date_index)

  # -----------------------------------------

  # GET DATE IN LUBRIDATE FORMAT
  date_ref = substr(date_folders[date_index],1,8) %>%
    lubridate::ymd()

  # -----------------------------------------

  # GET NAMES OF THE FILES CONTAINING THE MEMBER FORECASTS FROM DATE FOLDER
  # date_index = 1
  date_folder = paste(tidal_ens_dir, date_folders[date_index], "/", sep = "")
  if(!dir.exists(date_folder)) stop("Can't find the forecast folder")
  save_path = paste(tidal_processed_dir, date_folders[date_index], ".rds", sep = "")
  if(file.exists(save_path) & !overwrite_saved){
    print(paste("Already reformatted and saved the ensemble from ", date_ref))
    next
  }
  ens_forecast_files = paste(date_folder, list.files(date_folder), sep = "")
  if(length(ens_forecast_files) == 0){
    print(paste("No ensemble files for", date_ref))
    missing_member_files = c(missing_member_files, date_ref)
    next
  }

  # -----------------------------------------

  # CHECK NUMBER OF INIT TIMES LISTED IN THE FORECAST FILENAME STRINGS
  # Currently only coded for 0000, so need to check this assumption holds
  get_init_time <- function(str){
    str_match_all(str, "[0-9]+") %>%
      unlist() %>%
      (function(x){return(x[3])})
  }
  if(print_my_warnings)
    warning("Hard coded reference to intialisation time string")
  init_times <- sapply(ens_forecast_files, get_init_time) %>% unique()
  if(length(init_times) != 1)
    stop("Need to add function to loop over lead times")

  # -----------------------------------------

  # FOR EACH MEMBER READ THE ENSEMBLE FORECAST
  # ADD A MEMBER REFERENCE TO THE DATA (FOR PLOTTING/TIDYING)
  if(length(ens_forecast_files) != num_members){
    print(paste("Missing some member forecasts for", date_ref))
    missing_member_files = c(missing_member_files, date_ref)
    next
  }
  ensemble_list = vector("list", num_members)
  for(member_index in 1:num_members){
    ens_forecast_file = ens_forecast_files[member_index]
    if(!file.exists(ens_forecast_file)) stop("Can't find the file")
    ensemble_list[[member_index]] <- read_table2(ens_forecast_file,
                                                 col_names = FALSE) %>%
      name_vars() %>%
      dplyr::mutate(member = member_index)
  }
  if(print_my_warnings)
    warning("Column names were missing so they needed to be manually added")

  # -----------------------------------------

  # COMBINE THE ENSEMBLE FORECAST
  ensemble_combined = data.table::rbindlist(ensemble_list) %>%
    # do.call(rbind, ensemble_list) %>%
    dplyr::select(-htid, -wtoc, -wkal, -wtok, -wtkc) %>%
    dplyr::mutate(wtid = dplyr::if_else(wtid == na_value, NA_real_, wtid),
                  wsur = dplyr::if_else(wsur == na_value, NA_real_, wsur),
                  wtot = dplyr::if_else(wtot == na_value, NA_real_, wtot),
                  harm = dplyr::if_else(harm == na_value, NA_real_, harm),
                  sur = dplyr::if_else(sur == na_value, NA_real_, sur),
                  obs = dplyr::if_else(harm == na_value, NA_real_, obs))
  print("HACKED A FIX FOR NA OBS IN HERE NEED TO DOUBLE CHECK THIS CODE SNIPPET ")
  if(print_my_warnings){
    warning("Column names that were not necessary were disgarded")
    warning("Changed a numeric value for missing to NA_real_")
  }

  # -----------------------------------------

  # # VISUALISE ENSEMBLE FORECAST - SPAGHETTI PLOT
  # library(ggplot2)
  # ggplot(ensemble_combined) +
  #   geom_line(aes(x = t, y = wsur, group = member),  col = "gray") +
  #   geom_line(aes(x = t, y = sur), col = "red", linetype = "dotted")

  # -----------------------------------------

  # CONVERT THE DATA IN THE STANDARD FORMAT
  ensemble_data <- ensemble_combined %>%
    dplyr::mutate(FORECAST_DATE = date_ref,
                  INIT_TIME = lubridate::duration(hours = as.numeric(init_times)),
                  LEAD_TIME = lubridate::duration(hours = t),
                  LOCATION = location_ref,
                  ELEMENT = element_ref,
                  MEMBER = paste("ENS", member, sep = "")
    ) %>%
    dplyr::select(-t, -member, -wtot, -obs) %>%
    # removing totals (wtot/obs) as we can get that from the other columns
    tidyr::pivot_wider(names_from = MEMBER, values_from = wsur) %>%
    dplyr::rename(OBS = sur, HARM = harm, WTID = wtid)
  if(print_my_warnings)
    warning("May need to adjust init time to numeric in mutate call here for new init times")

  # -----------------------------------------

  # ADD THE ENSEMBLE SUMMARY STATISTICS OF MEAN AND SD
  # These columns must start with PAR_*
  ensemble_summary = ensemble_data %>%
    dplyr::select(starts_with("ENS")) %>%
    dplyr::mutate(PAR_MEAN = rowMeans(.), PAR_SIGMA = apply(., 1, sd)) %>%
    dplyr::select(PAR_MEAN, PAR_SIGMA)

  ensemble_data <- cbind(ensemble_data, ensemble_summary)

  # -----------------------------------------

  # SAVE THE FORMATTED ENSEMBLE DATA
  saveRDS(ensemble_data, save_path)

} # WRAPPER END (/FOR LOOP END)

# --------------------------------------------------------------------------------

# COMBINE IT ALL TOGETHER (~ 10 minutes)
# Read the member files back in and combine them together
Sys.time()
formatted_files <- list.files(tidal_processed_dir) %>%
  paste(tidal_processed_dir, . , sep = "")
list_ensemble_data <- vector("list", length(formatted_files))
for(i in 1:length(formatted_files)){
  print(i)
  list_ensemble_data[[i]] <- readRDS(formatted_files[i])
}
all_ensemble_data <- data.table::rbindlist(list_ensemble_data)
  #do.call("rbind", list_ensemble_data)
Sys.time()

saveRDS(all_ensemble_data, paste(tidal_dir, "ENS_",location_ref, ".rds", sep = ""))

# --------------------------------------------------------------------------------

# SAVE A SECOND SMALLER ENSEMBLE THAT HAS BEEN FILTERED
# FILTER IT DOWN (KEEP HOURLY) AND REMOVE LATE LEAD TIMES
max_lead_time = 60*60*24*10
lead_times_numeric = unique(all_ensemble_data$LEAD_TIME)
index_keep_times = which(lead_times_numeric%%(60*60) == 0 &
                           lead_times_numeric < max_lead_time)
filter_times = lead_times_numeric[index_keep_times] %>%
  lubridate::as.duration()
reduced_ensemble_data <- all_ensemble_data %>%
  dplyr::filter(LEAD_TIME %in% filter_times)

saveRDS(reduced_ensemble_data, paste(tidal_dir, "reduced_ENS_",location_ref, ".rds", sep = ""))

# could do it in R - but I just manually zipped up the individual days

# # --------------------------------------------------------------------------------
# OLD INITIALISATION INPUTS
#
# # INSTALL PACKAGES
# library(readr)
# library(magrittr)
# library(stringr)
# library(dplyr)
# library(tidyr)
# library(data.table)
#
# # --------------------------------------------------------------------------------
#
# # INITIAL DIRECTORIES / VARIABLES
# station_name = "harlingen" #"lauwersoog"
# location_ref = "HARL" #"LAUW"
# tidal_dir = "/Users/katesaunders/Documents/No_Back_Up_Data/SURGE/"
# tidal_ens_dir = paste(tidal_dir, station_name, "_raw/ENS/", sep = "")
# tidal_processed_dir = paste(tidal_dir, station_name, "_processed/", sep = "")
# num_members = 50
# na_value = 99.99
# element_ref = "SURG"
# print_my_warnings = FALSE
# overwrite_saved = TRUE
#
# # --------------------------------------------------------------------------------
#
# # FUNCTION FOR RENAMING THE DATA
# name_vars <- function(df){
#   # see surplo.1.pdf for further details
#   col_names = c("t", "htid", "wtid",
#                 "wsur", "wtot", "wtoc",
#                 "wkal", "wtok", "wtkc",
#                 "harm", "obs", "sur")
#
#   if(ncol(df) != length(col_names)) stop("data frame is not compatible for naming")
#   names(df) = col_names
#
#   # t: time with respect to dtg increased by h
#   # htid: the astronomical tide harmonic analysis
#   # wtid: the astronomical tide calculated by WAQUA
#   # wsur: The surge calculated by WAQUA
#   # wtot: the total water level <wsur> + <htid>
#   # wtoc: the calculated water level <wsur> + <wtid>
#   # wkal, wtok, wktc: kalmman calculation of the dual mode
#   # harm: the astronomical tide
#   # obs: observations from the WAQG_ODC database
#   # sur: the observed surge <obs> - <harm>
#
#   return(df)
#
# }


