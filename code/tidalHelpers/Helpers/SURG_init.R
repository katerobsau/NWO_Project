# USER SPECIFIED INPUTS
station_name = "harlingen" #"lauwersoog"
location_ref = "HARL" #"LAUW"
element_ref = "SURG"
fixed_vars = c("INIT_TIME", "LEAD_TIME", "SEASON")
num_members = 50
na_value = 99.99  #na value in the ensemble
num_samples = 500 #bootstrap scores
print_my_warnings = FALSE
overwrite_saved = FALSE

# INPUTS (HARD CODES)
combine_winter = c(2015,2017)
omit_years = c(2010,2019)

# INSTALL PACKAGES
library(tidyverse) # ggplot2 #dplyr #magrittr #readr #stringr #tidyr
library(data.table)
library(gamlss)
library(Rcpp)
library(moments)

# FUNCTIONS
sourceCpp("code/tidalHelpers/Helpers/crps_ensemble.cpp")
source("code/tidalHelpers/Helpers/LAUW_fitting_utils.R")
# gotta source this! NGR_functions.R

# DIRECTORY INPUTS
tidal_dir = "/Users/katesaunders/Documents/No_Back_Up_Data/SURGE/"
tidal_ens_dir = paste(tidal_dir, station_name, "_raw/ENS/", sep = "")
tidal_processed_dir = paste(tidal_dir, station_name, "_processed/", sep = "")

surge_dir = "/Users/katesaunders/Documents/No_Back_Up_Data/SURGE/"

ensemble_path <- paste(surge_dir, "reduced_ENS_",location_ref, ".rds", sep = "")
pred_file_path = paste(getwd(), "/data/", location_ref, "_pred.rds", sep = "")
ens_pp_path = paste(getwd(), "/data/", location_ref, "_pp_ens.rds", sep = "")
ens_score_path = paste(getwd(), "/data/", location_ref, "_score_ens.rds", sep = "")
ens_score_bootsrap_path = paste(getwd(), "/data/", location_ref, "_score_bootstrap.rds", sep = "")

ens_score_bootsrap_path = paste(getwd(), "/data/", location_ref, "_score_bootstrap.rds", sep = "")
crpss_bootstrap_plot_path = paste(getwd(), "/data/", location_ref, "_crpss_bootstrap_plot.rds", sep = "")
rank_compare_plot_path = paste(getwd(), "/data/", location_ref, "_rankCompare_plot.rds", sep = "")

