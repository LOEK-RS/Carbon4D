#' load probe meta data taunus from a local folder into r
#' @description
#' @param path path to the local saved 
#' @details 
#' @return 
#' @author
#' Maiken Baumberger
#' @examples
#' \dontrun{
#' taunus_load_probe_meta_data
#' }
#' @export taunus_load_probe_meta_data
#' @aliases taunus_load_probe_meta_data


load_probe_meta_data_longterm <- function(path){
  ProbeMetaDataLongterm = read.csv(paste0(path,"/TaunusProbeMetaData/TaunusProbeMetaData.csv"))
  assign("TaunusProbeMetaData", TaunusProbeMetaData, envir = .GlobalEnv)
}
