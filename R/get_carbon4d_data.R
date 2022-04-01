#' get carbon4d data from github to a local folder
#' @description
#' @param path path to the local saved 
#' @details 
#' @return 
#' @author
#' Maiken Baumberger
#' @examples
#' \dontrun{
#' get_carbon4d_data
#' }
#' @export get_carbon4d_data
#' @aliases get_carbon4d_data



get_carbon4d_data <- function(path){
  if(length(dir()) ==0) {git2r::clone("https://github.com/MaikenBaumberger/Carbon4dData.git",path)}
  else {git2r::pull(path)}
}


