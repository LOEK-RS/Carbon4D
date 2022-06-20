#' mapview plot of the locations of the monthly porbe data
#' @description mapview plot of the locations of the weekly porbe data
#' @param path path to the local saved 
#' @return A mapview output
#' @author Maiken Baumberger
#' @examples plot_weekly_porbe_plots(path="C:/Users/maike/Desktop/Carbon4D/DownloadGitData/ProbeMetaData/ProbeMetaData.csv")
#' \dontrun{
#' plot_weekly_porbe_plots()
#' }
#' @export plot_weekly_porbe_plots
#' @aliases plot_weekly_porbe_plots




plot_weekly_porbe_plots <- function(path){
  ProbeMetaDataMonthly = read.csv(paste0(path,"/ProbeMetaDataMonthly/ProbeMetaDataMonthly.csv"))
  assign("ProbeMetaDataMonthly", ProbeMetaDataMonthly, envir = .GlobalEnv)
  plots <- ProbeMetaDataMonthly
  plots_sf <- sf::st_as_sf(plots,coords = c("lon","lat"), crs = 4326)
  mapview::mapview(plots_sf)
}
