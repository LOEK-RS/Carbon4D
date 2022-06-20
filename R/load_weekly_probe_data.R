#' load weekly probe data from a local folder into r
#' @description
#' @param path path to the local saved 
#' @details 
#' @return 
#' @author
#' Maiken Baumberger
#' @examples
#' \dontrun{
#' load_weekly_probe_data
#' }
#' @export load_weekly_probe_data
#' @aliases load_weekly_probe_data


load_weekly_probe_data <- function(path){
  csv_files_weekly_probe_data = list.files(paste0(path,"/ProbeDataWeekly"), pattern = ".csv",full.names = T)
  csv_names_weekly_probe_data = stringr::str_remove(list.files(paste0(path,"/ProbeDataWeekly"), pattern = ".csv"), ".csv")
  csv_list_weekly_probe_data = lapply(csv_files_weekly_probe_data, read.csv) 
  names(csv_list_weekly_probe_data) = csv_names_weekly_probe_data
  assign("csv_list_weekly_probe_data", csv_list_weekly_probe_data, envir = .GlobalEnv)
}
