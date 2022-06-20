#' load probe meta data longterm from a local folder into r
#' @description
#' @param path path to the local saved 
#' @details 
#' @return 
#' @author
#' Maiken Baumberger
#' @examples
#' \dontrun{
#' load_probe_meta_data_longterm
#' }
#' @export load_probe_meta_data_longterm
#' @aliases load_probe_meta_data_longterm


load_probe_meta_data_longterm <- function(path){
  ProbeMetaDataLongterm = read.csv(paste0(path,"/ProbeMetaDataLongterm/ProbeMetaDataLongterm.csv"))
  assign("ProbeMetaDataLongterm", ProbeMetaDataLongterm, envir = .GlobalEnv)
}
