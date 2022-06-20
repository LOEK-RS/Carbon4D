#' load longterm probe data from a local folder into r
#' @description
#' @param path path to the local saved 
#' @details 
#' @return 
#' @author
#' Maiken Baumberger
#' @examples
#' \dontrun{
#' load_longterm_probe_data
#' }
#' @export load_longterm_probe_data
#' @aliases load_longterm_probe_data


load_longterm_probe_data <- function(path){
  csv_files_longterm_probe_data = list.files(paste0(path,"/ProbeDataLongterm"), pattern = ".csv",full.names = T)
  csv_names_longterm_probe_data = stringr::str_remove(list.files(paste0(path,"/ProbeDataLongterm"), pattern = ".csv"), ".csv")
  csv_list_longterm_probe_data = lapply(csv_files_longterm_probe_data, read.csv) 
  names(csv_list_longterm_probe_data) = csv_names_longterm_probe_data
  assign("csv_list_longterm_probe_data", csv_list_longterm_probe_data, envir = .GlobalEnv)
}
