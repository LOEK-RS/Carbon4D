#' mapview plot of the locations of the monthly porbe data
#' @description mapview plot of the locations of the monthly porbe data
#' @param path path to the local saved 
#' @return A mapview output
#' @author Maiken Baumberger
#' @examples plot_monthly_porbe_plots(path="C:/Users/maike/Desktop/Carbon4D/DownloadGitData/ProbeMetaData/ProbeMetaData.csv")
#' \dontrun{
#' plot_monthly_porbe_plots()
#' }
#' @export plot_monthly_porbe_plots
#' @aliases plot_monthly_porbe_plots




plot_monthly_porbe_plots <- function(path){
  ProbeMetaData = read.csv(path)
  assign("ProbeMetaData", ProbeMetaData, envir = .GlobalEnv)
  plots <- ProbeMetaData
  plots_sf <- sf::st_as_sf(plots,coords = c("lon","lat"), crs = 4326)
  mapview::mapview(plots_sf)
}



