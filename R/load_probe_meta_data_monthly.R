#' load probe meta data monthly from a local folder into r
#' @description
#' @param path path to the local saved 
#' @details 
#' @return 
#' @author
#' Maiken Baumberger
#' @examples
#' \dontrun{
#' load_probe_meta_data_monthly
#' }
#' @export load_probe_meta_data_monthly
#' @aliases load_probe_meta_data_monthly


load_probe_meta_data_monthly <- function(path){
  ProbeMetaDataMonthly = read.csv(paste0(path,"/ProbeMetaDataMonthly/ProbeMetaDataMonthly.csv"))
  assign("ProbeMetaDataMonthly", ProbeMetaDataMonthly, envir = .GlobalEnv)
}
