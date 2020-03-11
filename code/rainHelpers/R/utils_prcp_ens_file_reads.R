Get_file_name <- function(date_str, lead_time, stn_str,
                          ens_prefix = "ECME", script_prefix = "WP6",
                          tail_suffix = "LC"){

  file_name = paste(ens_prefix, "_", script_prefix, "_",
                    date_str, lead_time, "_", stn_str,
                    "_", tail_suffix, sep = "")

  return(file_name)

}

Get_zip_strings_from_folder <- function(zip_dir){

  file_names = list.files(zip_dir)
  date_ref <- lapply(file_names, function(x){
    tail_str = strsplit(x, split = "_")[[1]][3]
    date_ref = gsub("\\D", "", tail_str)
    return(date_ref)
  }) %>% unlist()

  return(date_ref)
}

Get_date_str_from_zip_str <- function(zip_str){

  year_str = substr(zip_str,1,4) %>% as.character()
  month_str = substr(zip_str,5,6) %>% as.character()
  start_date = paste(year_str, month_str, "01", sep = "") %>%
    lubridate::as_date()
  num_days = lubridate::days_in_month(start_date)
  day_seq = seq(1,num_days) %>%
    str_pad(width = 2, pad = "0")
  date_str = paste(year_str, month_str, day_seq, sep = "")

  return(date_str)

}

Get_zip_str_from_date_str <- function(date_str){

  year_str = substr(date_str,1,4) %>% as.character()
  month_str = substr(date_str,5,6) %>% as.character()
  day_str = substr(date_str,7,8) %>% as.character()
  zip_str = paste(year_str, month_str, sep = "")

  return(zip_str)

}

Get_all_date_strs_for_zip_str <- function(zip_strings){

  date_strings <- sapply(zip_strings, Get_date_str_from_zip_str)

  date_df <- date_strings %>%
    unlist() %>%
    as.data.frame()
  names(date_df) = "date_str"

  date_df <- date_df %>%
    mutate(zip_str = Get_zip_str_from_date_str(date_str))

  return(date_df)
}

Get_zip_file_name <- function(zip_str,
                          ens_prefix = "ECME", script_prefix = "WP6",
                          zip_suffix = ".zip"){

  zip_file_name = paste(ens_prefix, "_", script_prefix, "_",
                    zip_str, zip_suffix, sep = "")

  return(zip_file_name)

}

Get_zip_path <- function(zip_dir, zip_file_name){

  zip_path <- paste(zip_dir, zip_file_name, sep = "")
  return(zip_path)

}

Get_zip_contents <- function(zip_path, file_name){

  unz_connection <- unz(description = zip_path,
                        filename = file_name)
  rf_data <- readr::read_csv(unz_connection)

  rf_data = rf_data[[1]]

  return(rf_data)

}

Get_element_list <- function(unzipped_data, date_str,
                             element_code = "13021"){

  # find the row that matches our element
  search_str <- paste(element_code, date_str, sep = "  ")
  row_match = grep(search_str, unzipped_data)

  if(length(row_match) > 1){
      warning("More than one element matched")
      return(NULL)
  }
  if(length(row_match) == 0){
    warning("No element matched")
    return(NULL)
  }


  # get row indexes for data
  start_ind = row_match + 1
  date_match = grep(date_str,  unzipped_data)
  end_ind = date_match[date_match > start_ind][1] - 1
  element_range <- start_ind:end_ind

  element_list <- unzipped_data[element_range]

  return(element_list)

}

Convert_element_to_data_frame <- function(element_list){

  len = length(element_list)

  element_vec <- lapply(element_list, function(x){
    strsplit(x, split = "\\s+")[[1]] %>% as.numeric()
  }) %>% unlist()
  element_data <- matrix(element_vec, nrow = 52, byrow = TRUE) %>%
    as.data.frame()

  lead_times = paste(seq(6,240,6), "h", sep = "")
  names(element_data) = lead_times

  warning("Guessed the second row was control")
  members = c("HR", "CRTL", paste("ENS", 1:50, sep = ""))

  element_data$members = members

  return(element_data)

}

Write_element_data <- function(element_data, output_dir, file_name){

  output_path  = paste(output_dir, file_name, ".csv", sep = "")
  write_csv(x = element_data, path = output_path, append = FALSE)

}

