#' mapview plot of the locations of the monthly porbe data
#' @description mapview plot of the locations of the longterm porbe data
#' @param path path to the local saved 
#' @return A mapview output
#' @author Maiken Baumberger
#' @examples plot_longterm_porbe_plots(path="C:/Users/maike/Desktop/Carbon4D/DownloadGitData/ProbeMetaDataLongterm/ProbeMetaDataLongterm.csv")
#' \dontrun{
#' plot_longterm_porbe_plots()
#' }
#' @export plot_longterm_porbe_plots
#' @aliases plot_longterm_porbe_plots




plot_longterm_porbe_plots <- function(path){
  ProbeMetaDataLongterm = read.csv(paste0(path,"/ProbeMetaDataLongterm/ProbeMetaDataLongterm.csv"))
  assign("ProbeMetaDataLongterm", ProbeMetaDataLongterm, envir = .GlobalEnv)
  plots <- ProbeMetaDataLongterm
  plots_sf <- sf::st_as_sf(plots,coords = c("lon","lat"), crs = 4326)
  mapview::mapview(plots_sf)
}
