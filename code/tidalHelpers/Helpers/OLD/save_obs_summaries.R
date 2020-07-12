print("Remember to load read_tide_data()")
date_seq = seq(lubridate::as_date("2011-01-01"),lubridate::as_date("2019-12-31"), by = "days")
t_vals = c(0, c(1:10)*24)

ctrl_data_dir = "/Volumes/My Passport/WAQUA/harlinge_data/CTRL/"
# ctrl_data_dir = "/Users/katesaunders/Dropbox/TU Delft/Data/CTRL/"
print("WRONG DATA DIRECTRY!!!!!!!")

output_dir = "/Users/katesaunders/Documents/Git/StormSurge/Summary_Data/"

ctrl_data <- lapply(date_seq, function(date_val, data_dir){
  date_str = date_val %>% as.character()
  date_str = gsub(pattern = "-", replacement = "", x = date_val)
  tide_data <- tryCatch(read_tide_data(date_val = date_str, 
                                       lead_time = "00000", 
                                       member_type = "CTRL", 
                                       data_dir = data_dir),
                        error = function(e){return(NULL)})
  if(is.null(tide_data)) return(NULL)
  
  tide_data <- preprocess_tide_data(tide_data)
  
  tide_data <- tide_data %>% mutate(date = as_date(date_val))
  
  return(tide_data)    
  
}, data_dir = ctrl_data_dir) 

ctrl_data_filter <- ctrl_data %>% 
  bind_rows() %>% 
  filter(t %in% t_vals)

file_name = paste(output_dir, "Surge_Obs_Summary.rds", sep = "")
saveRDS(ctrl_data_filter, file = file_name)

