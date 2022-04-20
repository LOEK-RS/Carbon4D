#' load probe data from a local folder into r
#' @description
#' @param path path to the local saved 
#' @details 
#' @return 
#' @author
#' Maiken Baumberger
#' @examples
#' \dontrun{
#' load_probe_data
#' }
#' @export load_probe_data
#' @aliases load_probe_data


load_probe_data <- function(path){
  csv_files = list.files(paste0(path,"/ProbeData"), pattern = ".csv",full.names = T)
  csv_names = stringr::str_remove(list.files(paste0(path,"/ProbeData"), pattern = ".csv"), ".csv")
  csv_list = lapply(csv_files, read.csv) 
  names(csv_list) = csv_names
  assign("csv_list", csv_list, envir = .GlobalEnv)
}
