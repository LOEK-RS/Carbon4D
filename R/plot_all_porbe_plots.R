#' mapview plot of the locations of all porbe data
#' @description mapview plot of the locations of the weekly porbe data
#' @param path path to the local saved 
#' @return A mapview output
#' @author Maiken Baumberger
#' @examples plot_all_porbe_plots(path="C:/Users/maike/Desktop/Carbon4D/DownloadGitData/ProbeMetaDataWeekly/ProbeMetaDataWeekly.csv")
#' \dontrun{
#' plot_all_porbe_plots()
#' }
#' @export plot_all_porbe_plots
#' @aliases plot_all_porbe_plots




plot_all_porbe_plots <- function(path){
  ProbeMetaDataWeekly = read.csv(paste0(path,"/ProbeMetaDataWeekly/ProbeMetaDataWeekly.csv"))
  ProbeMetaDataMonthly = read.csv(paste0(path,"/ProbeMetaDataMonthly/ProbeMetaDataMonthly.csv"))
  ProbeMetaDataLongterm = read.csv(paste0(path,"/ProbeMetaDataLongterm/ProbeMetaDataLongterm.csv"))
  assign("ProbeMetaDataWeekly", ProbeMetaDataWeekly, envir = .GlobalEnv)
  assign("ProbeMetaDataMonthly", ProbeMetaDataMonthly, envir = .GlobalEnv)
  assign("ProbeMetaDataLongterm", ProbeMetaDataLongterm, envir = .GlobalEnv)
  plotsWeekly <- ProbeMetaDataWeekly
  plotsMonthly <- ProbeMetaDataMonthly
  plotsLongterm <- ProbeMetaDataLongterm
  plots_weekly <- sf::st_as_sf(plotsWeekly,coords = c("lon","lat"), crs = 4326)
  plots_monthly <- sf::st_as_sf(plotsMonthly,coords = c("lon","lat"), crs = 4326)
  plots_longterm <- sf::st_as_sf(plotsLongterm,coords = c("lon","lat"), crs = 4326)
  mapview::mapview(plots_weekly,col.regions="red")+mapview::mapview(plots_monthly,col.regions="blue")+mapview::mapview(plots_longterm,col.regions="purple")
}


