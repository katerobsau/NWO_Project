#' @export
#'
get_ensemble_ref <- function(num_members){

  member_str = formatC(x = 1:num_members, width = 3, flag = "0")
  member_ref = sapply(member_str, function(str){paste("fc", str, sep = "")}) %>% as.character()

  return(member_ref)

}

#' @export
#'
combine_ensemble_data <- function(date_val, lead_time, member_ref,
                                  data_dir, pre_ext = "WAQE_PLT", post_ext = "LC"){

  # print("Should change read so the column specification doesn't always appear")
  # print("Need to fix file structure so I can read CTRL, HRES and the members together cleanly")

  check_files_exist = sapply(member_ref, function(member_type){
    file_name = paste(data_dir, pre_ext, "_", date_val, "00", "_", lead_time, "_", post_ext, "_", member_type, sep = "")
    bool_val = file.exists(file_name)
    return(bool_val)
  })

   if(sum(check_files_exist) != length(member_ref)){
     return(NULL)
   }

    member_tide_data_list <- lapply(member_ref %>% as.list(), function(member_ref){

    tide_data <- read_tide_data(date_val = date_val, lead_time = lead_time,
                                member_type = member_ref,
                                data_dir = data_dir, pre_ext = pre_ext, post_ext = post_ext)

    tide_data <- preprocess_tide_data(tide_data)

    tide_data <- mutate(tide_data, member = member_ref)

    return(tide_data)

    })

  member_tide_data <- member_tide_data_list %>% bind_rows()

  return(member_tide_data)

}

#' @export
#'
get_ensemble_summary <- function(member_tide_data, var_name, mean = TRUE, sd = TRUE){

  member_sum = member_tide_data %>%
    select(t, var_name)
  names(member_sum) = c("t", "v")

  member_sum = member_sum %>%
    group_by(t) %>%
    summarise(mean = mean(v), sd = sd(v))

  names(member_sum) = c("t", paste("mean", var_name, sep = "_"), paste("sd", var_name, sep  = "_"))
  return(member_sum)

}


