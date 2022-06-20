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
  csv_list_longterm_probe_data = lapply(csv_files_longterm_probe_data,function(i){read.csv(i, header=F,skip = 1)})
  colnames <- c("datetime","T_org","T_05","T_15","T_25","T_35","T_45","T_55","T_65","T_75","T_85","T_95","T_105","T_115","M_org","M_05","M_15","M_25","M_35","M_45","M_55","M_65","M_75","M_85","M_95","M_105","M_115")
  csv_list_longterm_probe_data <- lapply(csv_list_longterm_probe_data, setNames, nm = colnames)
  names(csv_list_longterm_probe_data) = csv_names_longterm_probe_data
  assign("csv_list_longterm_probe_data", csv_list_longterm_probe_data, envir = .GlobalEnv)
}
