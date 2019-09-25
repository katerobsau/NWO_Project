# gets the ensemble data and writes it to a condensed format

wrapper_unzip_and_condense_ens_data <- function(){

  stn_str = "06270"
  zip_dir = "/Volumes/My Passport/Leeuwarden/"
  output_dir = "../Leeuwarden_Data/"
  lead_times = c("0000", "1200")

zip_strings <- Get_zip_strings_from_folder(zip_dir = zip_dir)

input_variables <- expand.grid(lead_time = lead_times, zip_str = zip_strings) %>%
  as.data.frame()

date_str_df <- Get_all_date_strs_for_zip_str(zip_strings)

input_variables <- left_join(input_variables, date_str_df, by = "zip_str")

for(i in 1:nrow(input_variables)){

  date_str = input_variables$date_str[i] %>% as.character()
  lead_time = input_variables$lead_time[i] %>% as.character()

  unzip_element_data(zip_dir = zip_dir, date_str = date_str,
                   lead_time = lead_time, stn_str = stn_str,
                   output_dir = output_dir)

}

return(NULL)

}


# script_run = mapply(unzip_element_data,
#                     input_variables[1:3, ], USE.NAMES = TRUE,
#                     MoreArgs = list(zip_dir = zip_dir, stn_str = stn_str,
#                                     output_dir = output_dir))
# THINK THE ISSUE WITH MAPPLY IS IN THE FORMAT STRINGS VS FACTORS
