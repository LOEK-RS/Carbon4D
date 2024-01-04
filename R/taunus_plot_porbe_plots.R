#' mapview plot of the locations of the taunus porbe data
#' @description mapview plot of the locations of the taunus porbe data
#' @param path path to the local saved 
#' @return A mapview output
#' @author Maiken Baumberger
#' @examples taunus_plot_porbe_plots()
#' \dontrun{
#' taunus_plot_porbe_plots()
#' }
#' @export taunus_plot_porbe_plots
#' @aliases taunus_plot_porbe_plots




taunus_plot_porbe_plots <- function(path){
  TaunusProbeMetaData = read.csv(paste0(path,"/TaunusProbeMetaData/TaunusProbeMetaData.csv"))
  assign("TaunusProbeMetaData", TaunusProbeMetaData, envir = .GlobalEnv)
  plots <- TaunusProbeMetaData
  plots_sf <- sf::st_as_sf(plots,coords = c("lon","lat"), crs = 4326)
  mapview::mapview(plots_sf)
}
