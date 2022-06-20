#' mapview plot of the locations of the monthly porbe data
#' @description mapview plot of the locations of the monthly porbe data
#' @param path path to the local saved 
#' @return A mapview output
#' @author Maiken Baumberger
#' @examples plot_monthly_porbe_plots(path="C:/Users/maike/Desktop/Carbon4D/DownloadGitData/ProbeMetaDataMonthly/ProbeMetaDataMonthly.csv")
#' \dontrun{
#' plot_monthly_porbe_plots()
#' }
#' @export plot_monthly_porbe_plots
#' @aliases plot_monthly_porbe_plots




plot_monthly_porbe_plots <- function(path){
  ProbeMetaDataMonthly = read.csv(paste0(path,"/ProbeMetaDataMonthly/ProbeMetaDataMonthly.csv"))
  assign("ProbeMetaDataMonthly", ProbeMetaDataMonthly, envir = .GlobalEnv)
  plots <- ProbeMetaDataMonthly
  plots_sf <- sf::st_as_sf(plots,coords = c("lon","lat"), crs = 4326)
  mapview::mapview(plots_sf)
}