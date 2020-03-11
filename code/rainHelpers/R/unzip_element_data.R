unzip_element_data <- function(zip_dir, date_str, lead_time, stn_str,
                               output_dir){

  print(date_str)

  zip_str <- Get_zip_str_from_date_str(date_str = date_str)
  zip_file_name <- Get_zip_file_name(zip_str = zip_str)
  zip_path <- paste(zip_dir = zip_dir, zip_file_name = zip_file_name, sep = "")

  file_name <- Get_file_name(date_str = date_str,
                             lead_time = lead_time,
                             stn_str = stn_str)

  unzipped_data <- tryCatch(Get_zip_contents(zip_path = zip_path, file_name = file_name),
                            error = function(e) return(NULL))
  if(is.null(unzipped_data)) return(NULL)

  element_list <- Get_element_list(unzipped_data = unzipped_data,
                                 date_str = date_str)

  if(is.null(element_list)) return(NULL)

  element_data <- Convert_element_to_data_frame(element_list = element_list)

  Write_element_data(element_data = element_data, output_dir = output_dir,
                     file_name = file_name)

}
