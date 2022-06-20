#' mapview plot of the locations of the monthly porbe data
#' @description mapview plot of the locations of the weekly porbe data
#' @param path path to the local saved 
#' @return A mapview output
#' @author Maiken Baumberger
#' @examples plot_weekly_porbe_plots(path="C:/Users/maike/Desktop/Carbon4D/DownloadGitData/ProbeMetaDataWeekly/ProbeMetaDataWeekly.csv")
#' \dontrun{
#' plot_weekly_porbe_plots()
#' }
#' @export plot_weekly_porbe_plots
#' @aliases plot_weekly_porbe_plots




plot_weekly_porbe_plots <- function(path){
  ProbeMetaDataWeekly = read.csv(paste0(path,"/ProbeMetaDataWeekly/ProbeMetaDataWeekly.csv"))
  assign("ProbeMetaDataWeekly", ProbeMetaDataWeekly, envir = .GlobalEnv)
  plots <- ProbeMetaDataWeekly
  plots_sf <- sf::st_as_sf(plots,coords = c("lon","lat"), crs = 4326)
  mapview::mapview(plots_sf)
}
