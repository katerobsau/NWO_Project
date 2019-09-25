data_dir = "../Summary_Data/"
summary_type = "median"
ens_ref = "ECME"

ens_files <- list.files(data_dir)
ens_files = ens_files[grepl(ens_ref, ens_files) == TRUE]
ens_path = paste(data_dir, ens_files, sep = "")

### ---------------------------------------------------------------------------
### Aggregate

time_1 <- Sys.time()
total_num_days_for_aggr = 10
num_files = length(ens_files)
summary_data = NULL
for(i in 1:num_files){

  print(ens_files[i])
  ens_data <- read_csv(ens_path[i])

  aggr_ens_data <- sapply(1:total_num_days_for_aggr, function(x){
    aggr_ens_data <-
      Get_cols_for_lead_times(period_end = x*24, ens_data = ens_data) %>%
      Get_aggr_for_lead_times(ens_data = ens_data, col_names = .)
  return(aggr_ens_data)
  }) %>% as.data.frame()

  names(aggr_ens_data) = paste("aggr_ens", 1:total_num_days_for_aggr, sep = "_")
  aggr_ens_data$members = ens_data$members

### ---------------------------------------------------------------------------
### Summary

  date_summary <- Get_member_summary(ens_data = aggr_ens_data, summary_type = summary_type)
  summary_data <- rbind(summary_data, date_summary)

}

time_2 <- Sys.time()

### ---------------------------------------------------------------------------

#Append file references

file_lead_time = lapply(ens_files, function(x){
  strsplit(x, split = "_")[[1]][3] %>%
    substr(9,12)
}) %>%
  do.call("c", .)

file_date = lapply(ens_files, function(x){
  strsplit(x, split = "_")[[1]][3] %>%
    substr(1,8) %>%
    lubridate::as_date()
}) %>%
  do.call("c", .)

summary_data <- as.data.frame(summary_data)
summary_data$Date = file_date
summary_data$Lead_Time = file_lead_time

saveRDS(summary_data, file = paste(data_dir,
                                   "Ens_Summary_Median.rds", sep = ""))
