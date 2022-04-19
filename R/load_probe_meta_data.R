#' load probe meta data from a local folder into r
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
  ProbeMetaData = read.csv(paste0(path,"/ProbeMetaData/ProbeMetaData.csv"))
  assign("ProbeMetaData", ProbeMetaData, envir = .GlobalEnv)
}
