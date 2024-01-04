#' calibrate probe meta data 
#' @description function to calibrate the cleaned probe data
#' @param path_row_data location of soil probe data table
#' @param probe name of probe/ file
#' @details 
#' @return 
#' @author Maiken Baumberger
#' @examples
#' \dontrun{
#' calibrate_probe_data
#' }
#' @export calibrate_probe_data
#' @aliases calibrate_probe_data



calibrate_probe_data <- function(path_row_data, 
                             probe){

  
  TD <- read.csv(paste0(path_row_data,(sprintf("/%s_clean.csv",probe))))
  
  TD$datetime<-as.POSIXct(strptime(TD$datetime,"%Y-%m-%d %H:%M:%S",tz="UTC"))
  
  TDbackup <- TD
  
  TD[15:27] <- TD[15:27]
  
  
  TD[15:27] <- (0.1957*(TD[15:27]^0.404))+0.02852
  
  TD[15:27] <- ((TD[15:27]+0.227)/0.446)^(1/0.271)
  
  plot(TDbackup$M_15,ylim=c(0,50))
  
  points(TD$M_15, col="red")

  write.csv(TD,(paste0(path_row_data,(sprintf("/%s_clean_loam.csv",probe)))),row.names=FALSE)
  
}
