summary_dir = tidalHelpers::utils_init()$save_dir
output_dir = summary_dir

print("Fix this so it is automated!")
date_seq = seq(lubridate::as_date("2011-01-01"),
               lubridate::as_date("2019-12-31"), by = "days")

# read in the summaries for a data range
get_summaries = sapply(date_seq, function(date_val, summary_dir){
  date_str = date_val %>% as.character()
  date_str = gsub(pattern = "-", replacement = "", x = date_val)
  file_name = paste(summary_dir, date_str, "00", ".csv", sep = "")
  summary_data <-  tryCatch(read.csv(file_name, sep = ""),
                              error = function(e){return(NULL)})
  if(is.null(summary_data)) return(NULL)
  summary_data <- summary_data %>%
    mutate(date = date_val)
}, summary_dir = summary_dir)

combine_summaries <- get_summaries %>% bind_rows()

file_name = paste(output_dir, "Ensemble_Summary.rds", sep = "")
saveRDS(combine_summaries, file = file_name)

