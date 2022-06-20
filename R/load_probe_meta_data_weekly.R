#' load probe meta data weekly from a local folder into r
#' @description
#' @param path path to the local saved 
#' @details 
#' @return 
#' @author
#' Maiken Baumberger
#' @examples
#' \dontrun{
#' load_probe_meta_data_weekly
#' }
#' @export load_probe_meta_data_weekly
#' @aliases load_probe_meta_data_weekly


load_probe_meta_data_weekly <- function(path){
  ProbeMetaDataWeekly = read.csv(paste0(path,"/ProbeMetaDataWeekly/ProbeMetaDataWeekly.csv"))
  assign("ProbeMetaDataWeekly", ProbeMetaDataWeekly, envir = .GlobalEnv)
}
