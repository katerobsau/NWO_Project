# --------------------------------------------------------------------------------

# FILE STRUCTURE
# Stn location > CTRL, ENS, HR
# ENS > date folders (double zero at end) > 50 ensemble forecasts in each folder
# CTRL > 50 ctrl forecasts
# HR > (should have) 50 HR forecasts but empty at the moment

# ITERATIONS
# iterations are needed over: date, lead time, members
# althought currently lead time is only 00000

# DATA NEEDED
# Observations for scoring
# Ensemble summary statistics(/covariates) for model fitting
# Members for dependence template

# --------------------------------------------------------------------------------

# INSTALL PACKAGES
library(readr)
library(magrittr)
library(stringr)
library(dplyr)

# --------------------------------------------------------------------------------

# INITIAL DIRECTORIES / VARIABLES
tidal_ens_dir = "/Users/katesaunders/Documents/No_Back_Up_Data/SURGE/lauwersoog_raw/ENS/"
tidal_processed_dir = "/Users/katesaunders/Documents/No_Back_Up_Data/SURGE/lauwersoog_processed/"
num_members = 50
na_value = 99.99
location_ref = "LAUW"
element_ref = "SURG"
print_my_warnings = FALSE
overwrite_saved = FALSE

# --------------------------------------------------------------------------------

# FUNCTION FOR RENAMING THE DATA
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

# --------------------------------------------------------------------------------

#GET DATES FOLDERS
date_folders = list.files(tidal_ens_dir)

# --------------------------------------------------------------------------------

# START OF DATE WRAPPER
missing_member_files = NULL
for(date_index in 1:length(date_folders)){
  print(date_index)

  # -----------------------------------------

  # GET DATE IN LUBRIDATE FORMAT
  date_ref = substr(date_folders[date_index],1,8) %>%
    lubridate::ymd()

  # -----------------------------------------

  # GET FILE NAMES OF THE FORECASTS FROM DATE FOLDER
  # date_index = 1
  date_folder = paste(tidal_ens_dir, date_folders[date_index], "/", sep = "")
  if(!dir.exists(date_folder)) stop("Can't find the forecast folder")
  save_path = paste(tidal_processed_dir, date_folders[date_index], ".rds")
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

  # CHECK NUMBER OF LEAD TIMES LISTED IN THE FORECAST FILENAME STRINGS
  get_lead_time <- function(str){
    str_match_all(str, "[0-9]+") %>%
      unlist() %>%
      (function(x){return(x[3])})
  }
  if(print_my_warnings)
    warning("Hard coded reference to lead time string")
  lead_times <- sapply(ens_forecast_files, get_lead_time) %>% unique()
  if(length(lead_times) != 1) stop("Need to add function to loop over lead times")

  # -----------------------------------------

  # FOR EACH MEMBER READ THE ENSEMBLE FORECAST
  # ADD A MEMBER REFERENCE TO THE DATA (FOR PLOTTING)
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
  ensemble_combined = do.call(rbind, ensemble_list) %>%
    dplyr::select(-htid, -wtoc, -wkal, -wtok, -wtkc) %>%
    dplyr::mutate(wtid = dplyr::if_else(wtid == na_value, NA_real_, wtid),
                  wsur = dplyr::if_else(wsur == na_value, NA_real_, wsur),
                  wtot = dplyr::if_else(wtot == na_value, NA_real_, wtot))
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

  # GET THE DATA IN THE STANDARD FORMAT
  ensemble_data <- ensemble_combined %>%
    dplyr::mutate(FORECAST_DATE = date_ref,
                  INIT_TIME = lubridate::duration(hours = as.numeric(lead_times)),
                  LEAD_TIME = lubridate::duration(hours = t),
                  LOCATION = location_ref,
                  ELEMENT = element_ref,
                  MEMBER = paste("ENS", member, sep = "")
    ) %>%
    dplyr::select(-t, -member, -wtot, -obs) %>%
    # removing totals (wtot/obs) as we can get that from the other columns
    pivot_wider(names_from = MEMBER, values_from = wsur) %>%
    dplyr::rename(OBS = sur, HARM = harm, WTID = wtid)
  if(print_my_warnings)
    warning("May need to adjust lead time to numeric in mutate call here for new lead times")

  # -----------------------------------------

  # ADD THE ENSEMBLE SUMMARY STATISTICS
  ensemble_summary = ensemble_data %>%
    dplyr::select(starts_with("ENS")) %>%
    dplyr::mutate(ENS_MEAN = rowMeans(.), ENS_SIGMA = apply(., 1, sd)) %>%
    dplyr::select(ENS_MEAN, ENS_SIGMA)

  ensemble_data <- cbind(ensemble_data, ensemble_summary)

  # -----------------------------------------

  # SAVE THE FORMATTED ENSEMBLE DATA
  saveRDS(ensemble_data, save_path, sep = "")

} # WRAPPER END

# --------------------------------------------------------------------------------

# COMBINE IT ALL TOGETHER
formatted_files <- list.files(tidal_processed_dir) %>%
  paste(tidal_processed_dir, . , sep = "")
list_ensemble_data <- vector("list", length(formatted_files))
for(i in 1:length(formatted_files)){
  list_ensemble_data[[i]] <- readRDS(formatted_files[i])
}
all_ensemble_data <- do.call("rbind", list_ensemble_data)
saveRDS(all_ensemble_data, paste(tidal_processed_dir, "ENS_",location_ref, ".rds", sep = ""))

# could do it in R - but I just manually zipped up the individual days

# --------------------------------------------------------------------------------


