#' @export
#'
write_ensemble_summary_to_file <- function(main_data_dir, save_dir, var_names,
                                           overwrite = TRUE){

  init_var = utils_init()
  if(missing(main_data_dir)){main_data_dir = init_var$main_data_dir}
  if(!dir.exists(main_data_dir)){
    stop("The main data directory is incorrectly defined")
  }
  if(missing(save_dir)){save_dir = init_var$save_dir}
  if(missing(var_names)){var_names = init_var$var_names}
  rm_names = init_var$rm_names

  dates_vec = list.files(main_data_dir)
  member_ref = get_ensemble_ref(init_var$num_members)

  for(date_val in dates_vec){

    print(date_val)
    data_dir = paste(main_data_dir, date_val, "/", sep = "")

    save_path = paste(save_dir, date_val, ".csv", sep = "")
    if(!overwrite & file.exists(save_path)) next

    ensemble_data = combine_ensemble_data(date_val, lead_time = "00000",
                                          member_ref = member_ref,
                                          data_dir = data_dir)
    if(is.null(ensemble_data)) next

    lead_time_col <- ensemble_data %>% select(t) %>% distinct()
    ensemble_summary <- lead_time_col
    for(var_name in var_names){
      var_summary = get_ensemble_summary(ensemble_data, var_name)
      ensemble_summary <-full_join(ensemble_summary, var_summary)
    }

    ensemble_summary_all <- ensemble_summary %>%
      left_join(ensemble_data %>%
                  select(-var_names, -rm_names, -member, - catg) %>%
                  distinct())

    write.table(x = ensemble_summary_all, file = save_path)

  }

}
