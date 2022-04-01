#' mapview plot of the locations of the monthly porbe data
#' @description
#' @param path path to the local saved 
#' @details 
#' @return A mapview output
#' @author
#' Maiken Baumberger
#' @examples
#' \dontrun{
#' plot_plots()
#' }
#' @export plot_plots
#' @aliases plot_plots




plot_monthly_porbe_plots <- function(path){
  ProbeMetaData = read.csv(path)
  assign("ProbeMetaData", ProbeMetaData, envir = .GlobalEnv)
  plots <- ProbeMetaData
  plots_sf <- sf::st_as_sf(plots,coords = c("lon","lat"), crs = 4326)
  mapview::mapview(plots_sf)
}



