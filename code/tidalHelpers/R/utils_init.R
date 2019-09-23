#' @export
#'
utils_init <- function(){

  ### Three indistinguisable members
  members <- c("CTRL", "HRES", "ENS")

  ### There are 50 ensemble members
  num_members = 50

  ### There are two initialisation times
  init_times <- c("00000", "12000")
  print("Check this")

  ### Directory of ensemble data
  main_data_dir = "/Volumes/My Passport/WAQUA/harlinge_data/ENS/"
  warning(
    paste0("Currently main directory of ensemble files is ", main_data_dir))

  ### Save directory for output files
  save_dir = "/Users/katesaunders/Documents/No_Back_Up_Data/Tidal_ENS_Summaries/"
  warning(
    paste0("Set save directory for output ensemble files is ", save_dir))

  ### Variable names in ensemble files
  var_names = c("wsur", "wtot", "wtoc")

  ### Variable names in ensemble files
  const_names = c("wtid", "htid", "harm", "obs", "sur")

  ## Nonthing variables
  rm_names = c("wkal", "wtok", "wtkc")

  init_values = list(members = members,
                     num_members = num_members,
                     init_times = init_times,
                     main_data_dir = main_data_dir,
                     save_dir = save_dir,
                     var_names = var_names,
                     const_names = const_names,
                     rm_names = rm_names)

  return(init_values)

}
