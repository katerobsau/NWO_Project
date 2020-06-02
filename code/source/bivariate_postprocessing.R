### Bivariate post-processing

### ISSUES:
# * Would like to streamline/change column names earlier
# * Having the members and pars separate could also create consistency problems

## packages
# got to work on reproducibility
num_members = 50
library(devtools)
install_github("katerobsau/depPPR", ref = "dev", force = TRUE)
library(depPPR)
library(tidyverse)
library(scoringRules)
library(gamlss)

## SURGE ----------------------------------------------------------------------

### INPUT DATA
surge_ensemble_editted <- readRDS(file = "data/ensemble_filtered.rds")
surge_pars_raw <- readRDS(file = "data/surge_pars.rds")
warning("I filtered this data by season first before saving these, and used reduced lead times")
warning("Needed to manually add init time")

### ENSEMBLE
surge_ensemble <- surge_ensemble_editted %>%
  dplyr::select(OBS = sur,
                LEAD_TIME = t,
                FORECAST_DATE = date,
                FRC_SUR = wsur,
                MEMBER = member) %>%
  dplyr::mutate(ELEMENT = "SUR") %>%
  dplyr::mutate(MEMBER = paste("ENS", parse_number(MEMBER), sep = "")) %>%
  pivot_wider(names_from = MEMBER, values_from = FRC_SUR) %>%
  dplyr::mutate(INIT_TIME = 0) %>%
  dplyr::mutate(INIT_TIME = paste(INIT_TIME, ":0:0", sep = "")) %>%
  dplyr::mutate(INIT_TIME = lubridate::as.duration(lubridate::hms(INIT_TIME))) %>%
  dplyr::mutate(LEAD_TIME = paste(LEAD_TIME, ":0:0", sep = "")) %>%
  dplyr::mutate(LEAD_TIME = lubridate::as.duration(lubridate::hms(LEAD_TIME))) %>%
  dplyr::mutate(FORECAST_DATE = substr(FORECAST_DATE,1, 8)) %>%
  dplyr::mutate(FORECAST_DATE = lubridate::as_date(FORECAST_DATE))

### UNIVARIATE PARS
surge_pars <- surge_pars_raw %>%
  dplyr::select(LEAD_TIME = lead_time,
                FORECAST_DATE = dates,
                mean = mu, sd = sigma) %>%
  dplyr::mutate(ELEMENT = "SUR") %>%
  dplyr::mutate(INIT_TIME = 0) %>%
  dplyr::mutate(INIT_TIME = paste(INIT_TIME, ":0:0", sep = "")) %>%
  dplyr::mutate(INIT_TIME = lubridate::as.duration(lubridate::hms(INIT_TIME))) %>%
  dplyr::mutate(LEAD_TIME = paste(LEAD_TIME, ":0:0", sep = "")) %>%
  dplyr::mutate(LEAD_TIME = lubridate::as.duration(lubridate::hms(LEAD_TIME))) %>%
  dplyr::mutate(FORECAST_DATE = lubridate::as_date(FORECAST_DATE))

surge_data <- full_join(
  surge_ensemble,
  surge_pars,
  by = c("FORECAST_DATE", "INIT_TIME", "LEAD_TIME", "ELEMENT"))

## RAINFALL -------------------------------------------------------------------

### DATA INPUTS
rainfall_data_dir = "~/Documents/No_Back_Up_Data/Leeuwarden_Data/"
rainfall_ensemble_editted <- readRDS(paste0(rainfall_data_dir, "rainfall_predictor_data.rds"))
rainfall_pars_raw <- readRDS(paste0(rainfall_data_dir, "rainfall_pars.rds"))

### ENSEMBLE
warning("currently ignoring high res and crtl runs in gamlss fit")
warning("also ignored flagged obs, ie -1s")
rainfall_ensemble = rainfall_ensemble_editted %>%
  dplyr::select(INIT_TIME = Init,
                LEAD_TIME = lead_time,
                FORECAST_DATE = Forecast_Date,
                starts_with("ENS"),
                OBS = RH6) %>%
  dplyr::mutate(ELEMENT = "PRCP") %>%
  dplyr::mutate(INIT_TIME = lubridate::as.duration(INIT_TIME),
                LEAD_TIME = lubridate::as.duration(LEAD_TIME))
  # dplyr::rename_at(vars(starts_with("ENS")), funs(str_replace(., "ENS" ,"ENS_RF")))

### UNIVARIATE PARS
rainfall_pars <- rainfall_pars_raw %>%
  dplyr::select(INIT_TIME = Init,
                LEAD_TIME = lead_time,
                FORECAST_DATE  =  Forecast_Date,
                mu = mu, sigma = sigma, nu = nu) %>%
  dplyr::mutate(ELEMENT = "PRCP") %>%
  dplyr::mutate(INIT_TIME = lubridate::as.duration(INIT_TIME),
                LEAD_TIME = lubridate::as.duration(LEAD_TIME))

rainfall_data <- full_join(
  rainfall_ensemble,
  rainfall_pars,
  by = c("FORECAST_DATE", "INIT_TIME", "LEAD_TIME", "ELEMENT"))

## SURGE + RAINFALL ENSEMBLE --------------------------------------------------

bivar_ensemble = bind_rows(surge_data, rainfall_data) %>%
  dplyr::mutate(INIT_TIME = lubridate::as.duration(INIT_TIME)) %>%
  dplyr::mutate(LEAD_TIME = lubridate::as.duration(LEAD_TIME))

# bivar_ensemble = bind_rows(
#   surge_data %>%
#     dplyr::mutate(INIT_TIME = as.character(INIT_TIME),
#                   LEAD_TIME = as.character(LEAD_TIME)),
#   rainfall_data %>%
#     dplyr::mutate(INIT_TIME = as.character(INIT_TIME),
#                   LEAD_TIME = as.character(LEAD_TIME))) %>%
#   dplyr::mutate(INIT_TIME = ifelse(INIT_TIME == "0S", "0H 0M 0S", INIT_TIME)) %>%
#   dplyr::mutate(INIT_TIME = lubridate::hms(INIT_TIME),
#                 LEAD_TIME = lubridate::hms(LEAD_TIME))

## SAMPLE FROM UNIVARIATE POST-PROCESSSED PDF ---------------------------------

# surge
surge_sampled_ens <- depPPR::sample_ecc_members(
  num_members = num_members,
  pars = bivar_ensemble %>%
    filter(ELEMENT == "SUR") %>% # filtered by element
    dplyr::select(mean, sd),
  ecc_type = "R",
  function_type = rnorm) %>%
  as.data.frame() # converted it from a matrix type
names(surge_sampled_ens) <-
  depPPR::create_member_names("SMP", num_members, width = 0) # added names not in pipeline
surge_sampled_ens <- bind_cols(bivar_ensemble %>% filter(ELEMENT == "SUR"),
                               surge_sampled_ens) # joined the important stuff back in

# rainfall
rainfall_sampled_ens <- depPPR::sample_ecc_members(
  num_members = num_members,
  pars = bivar_ensemble %>%
    filter(ELEMENT == "PRCP") %>% # filtered by element
    dplyr::select(mu, sigma, nu),
  ecc_type = "R",
  function_type = rZAGA) %>%
  as.data.frame()
names(rainfall_sampled_ens) <-
  depPPR::create_member_names("SMP", num_members, width = 0) # added names not in pipeline
rainfall_sampled_ens <- bind_cols(bivar_ensemble %>% filter(ELEMENT == "PRCP"),
                                  rainfall_sampled_ens) # joined the important stuff back in

bivar_sampled_ens = bind_rows(surge_sampled_ens, rainfall_sampled_ens) %>%
  dplyr::mutate(INIT_TIME = lubridate::as.duration(INIT_TIME)) %>%
  dplyr::mutate(LEAD_TIME = lubridate::as.duration(LEAD_TIME))

# bivar_sampled_ens = bind_rows(
#   surge_sampled_ens %>%
#     dplyr::mutate(INIT_TIME = as.character(INIT_TIME),
#                   LEAD_TIME = as.character(LEAD_TIME)),
#   rainfall_sampled_ens %>%
#     dplyr::mutate(INIT_TIME = as.character(INIT_TIME),
#                   LEAD_TIME = as.character(LEAD_TIME))) %>%
#   dplyr::mutate(INIT_TIME = ifelse(INIT_TIME == "0S", "0H 0M 0S", INIT_TIME)) %>%
#   dplyr::mutate(INIT_TIME = lubridate::hms(INIT_TIME),
#                 LEAD_TIME = lubridate::hms(LEAD_TIME))

# Ideally, I'd like to do this all in one hit
# Problem have to select the parameters (could do it in a list)

# sur_par_names = c("mean", "sd")
# sur_function_type = rnorm
# prcp_par_names = c("mu", "sigma", "nu")
# prcp_function_type = rZAGA

### ---------------------------------------------------------------------------

### TEMPLATE STEP: ECC
bivar_sampled_ens1 <- bivar_sampled_ens %>%
  dplyr::filter(!is.na(ENS1)) %>%
  dplyr::filter(!is.na(SMP50))

X_raw = bivar_sampled_ens1 %>%
  dplyr::select(starts_with("ENS")) %>%
  as.matrix()

Y_forecast = bivar_sampled_ens1 %>%
  dplyr::select(starts_with("SMP")) %>%
  as.matrix()

ECC_forecast <- depPPR::apply_ecc_template(X_raw, Y_forecast)

ECC_forecast1 <- as.data.frame(ECC_forecast)
names(ECC_forecast1) <-
  depPPR::create_member_names("ECC", num_members, width = 0) # added names not in pipeline
ECC_forecast2 <- bind_cols(bivar_sampled_ens1 %>%
                            dplyr::select(-starts_with("ENS"),
                                          -starts_with("SMP")),
                  ECC_forecast1) # joined the important stuff back in

### Some comments on this code snippet:

# Okay it makes sense to remove NAs
# But because I do it here the naming is bad!
# matrix operations also make it complicated to pass these things
# in a pipe
# making extra work
# would be could to make this internal even
# so the dataframe with the ENS and the SMP data is passed
# and the output the named ECC

### ---------------------------------------------------------------------------

### TEMPLATE STEP: Schaake

# Get the observation for each forecast day
obs_data_long <- bivar_sampled_ens1 %>%
  dplyr::select(-starts_with("ENS"), -starts_with("SMP"),
                -mean, -sd, -mu, -sigma, -nu) #%>%
  # dplyr::mutate(LEAD_TIME = as.character(LEAD_TIME),
                # INIT_TIME = as.character(INIT_TIME))
obs_data <- obs_data_long %>%
  # pivot_wider(names_from = c(FORECAST_DATE, INIT_TIME), values_from = OBS)
  pivot_wider(names_from = c(FORECAST_DATE), values_from = OBS)

# Check NA values
na_cols = which(colSums(is.na(obs_data)) == 0)
warning("Need to check missing in each group")
warning("Currently done in a very hacky way")

# Get the possible dates for a template
possible_dates = setdiff(names(na_cols), names(bivar_sampled_ens1)) %>%
  lubridate::as_date()
print(paste(length(possible_dates), "days for climate sampling"))
print("I'd like to plot these (ie. surge, prcp, both)")

# This is where we need to split the ensemble into groups
# This will give us the set of data we will post-process together
bivar_split <- bivar_sampled_ens1 %>%
  dplyr::select(-starts_with("ENS"),
                -mu, -sigma, -mean, -sd, -nu) %>%
  # dplyr::mutate(INIT_TIME = as.character(INIT_TIME)) %>%
  group_by(FORECAST_DATE, INIT_TIME) %>%
  group_split(keep = TRUE)
print(length(bivar_split))
warning("Need a way of call the fixed variables togther")

## Get the dates
window_len = 30
schaake_dates <- lapply(bivar_split, function(l){
                        depPPR::sample_schaake_dates(
                          num_draws = num_members,
                          dates = possible_dates,
                          date_val = lubridate::as_date(l$FORECAST_DATE[1]),
                          window = window_len)
                        })
warning("Again hacky with the dates!!!")
warning("Also need to do pass arguements properly")
###"What happens if there aren't any dates?? - Gotta think about this"
### Sample handles it and gives an error
### Need to handle this in some way probably

## Get the observations for each of the schaake days
schaake_template <- lapply(schaake_dates, function(l, obs_data){
    if(all(is.na(l))) return(NA)
    date_cols = as.character(l)
    samp_ens = obs_data[date_cols]
    names(samp_ens) = depPPR::create_member_names("TMP", length(date_cols))
    samp_ens = cbind(obs_data %>%
                       dplyr::select(INIT_TIME, LEAD_TIME, ELEMENT),
                     samp_ens)
}, obs_data = obs_data)
warning("Want to select columns reference columns better ")
warning("Also manually handling the NAs here")

## 3 / APPLY THE SCHAAKE SHUFFLE

## Get the template
schaake_shuffled <- lapply(1:length(bivar_split),
                           function(i){
                             print(i)
                             if(length(schaake_template[[i]]) == 1) return(NULL)
                             X1 <-  bivar_split[[i]]
                             Y1 <- schaake_template[[i]]
                             combined <- inner_join(X1, Y1)
                              X = combined %>%
                                dplyr::select(starts_with("SMP")) %>%
                                as.matrix()
                              Y = combined %>%
                                dplyr::select(starts_with("TMP")) %>%
                                as.matrix()
                            shuffled <- tryCatch(
                              depPPR::schaake_shuffle(X,Y),
                              error =  function(e){ print(e); return(NA)}) %>%
                              as.data.frame()
                            names(shuffled) <- depPPR::create_member_names("SCH", num_members)
                            shuffled <- shuffled %>%
                              cbind(combined %>%
                                      dplyr::select(FORECAST_DATE, INIT_TIME, ELEMENT,
                                                    LEAD_TIME, OBS))
                           return(shuffled)
                           })
warning("Need to generalist template strings, ie SCH /ECC = TMP")
warning("Combining is a mess cause of dates")
warning("USed a tryCathc but gotta handle NA in Y")

### ---------------------------------------------------------------------------

both_var <- lapply(schaake_shuffled, function(l){
  if(length(l) > 1){
    bool_val <- c("SUR", "PRCP") %in% unique(l$ELEMENT) %>%
                all()
    return(bool_val)
  }
  return(-1)
}) %>% unlist()
table(both_var)
t_i = which(both_var == TRUE)

### ---------------------------------------------------------------------------

# NEXT STEPS::
# 1/ Score the dates I do have
# 2/ Simple question - how many of these paired days have rainfall?

# TUTORIAL STUFF:::
# 1/ Write up ECC
# 2/ Write up Schaake
# 3/ Require writing up group split tutorial or background
# ?/ Will this method be too slow for large data

# COMPLETE::
# 1/ (DONE) When do I have suitable data?
#(This might require going back over the preprocessed stuff for storm surge)
# 2/ (NOPE - ALL GOOD!) Do I need more data?? - Email Kiri - This will be a blocker :(
# 3/ (DONE THANK FUCK!!!) Fix the god damn date issue!!!

### ---------------------------------------------------------------------------

# # ### GROUP VARS - iniatilisation, seasons
# warning("Surge pars were filtered in univariate post-processing")
# warning("Must redo at some stage")
