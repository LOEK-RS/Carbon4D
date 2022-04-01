#' load carbon4d data from a local folder into r
#' @description
#' @param path path to the local saved 
#' @details 
#' @return 
#' @author
#' Maiken Baumberger
#' @examples
#' \dontrun{
#' load_probe_meta_data
#' }
#' @export load_probe_meta_data
#' @aliases load_probe_meta_data


load_probe_meta_data <- function(path){
  ProbeMetaData = read.csv(path)
  assign("ProbeMetaData", ProbeMetaData, envir = .GlobalEnv)
}
