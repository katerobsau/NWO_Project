dates_formatted = str_sub(dates_vec, 1, 8) %>%
  lubridate::as_date() %>%
  as.data.frame()
names(dates_formatted) = "date"
winter_days = dates_formatted %>%
  mutate(old_date = dates_vec) %>%
  mutate(year = lubridate::year(date), month = lubridate::month(date),
         day = lubridate::day(date)) %>%
  filter(month >= 10 | month <= 4) %>%
  filter(!(month == 10 & day <= 15) & !(month == 4 & day >= 15)) %>%
  mutate(season = ifelse(month >= 10, year, year - 1))
seasons = winter_days %>% pull(season) %>% unique()

for(s in 1:length(seasons)){

  loop_dates = winter_days %>%
    filter(season == seasons[s]) %>%
    pull(old_date)

  season_all <- NULL
  for(i in 1:length(loop_dates)){

  date_val = dates_vec[i]  #"2011112900" # #"2012010200" #
  data_dir = paste(main_data_dir, date_val, "/", sep = "")

  # Get Ensemble data
  ensemble_data = combine_ensemble_data(date_val,
                                        lead_time = "00000",
                                        member_ref = member_ref,
                                        data_dir = data_dir)
  if(is.null(ensemble_data)) next

  # Deal with NA data
  ensemble_data = ensemble_data %>%
    NA_check() %>%
    mutate(date = date_val)

  season_all = rbind(season_all, ensemble_data)

  }

  season_data_dir = "/Users/katesaunders/Documents/No_Back_Up_Data/Winter/"
  saveRDS(season_all,
          file = paste0(season_data_dir, "Winter", seasons[s], ".rds"))

}
