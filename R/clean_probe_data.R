#' clean probe meta data 
#' @description
#' @param path path to the local saved 
#' @details 
#' @return 
#' @author
#' Maiken Baumberger
#' @examples
#' \dontrun{
#' clean_probe_data
#' }
#' @export clean_probe_data
#' @aliases clean_probe_data


path_row_data = "C:/Users/maike/Desktop/Carbon4D/Taunus"
probe = "S03_015"
startdatetime = "2023-12-14 00:00:00"
enddatetime = "2024-01-03 00:00:00"
organic = 0
excess = 30

# 
# probe <- "S15_014"
# path_row_data <- "C:/Users/maike/Desktop/Carbon4D/DatenbereinigungIrrimax/IrrimaxDataComplete"
# 
# paste0(path_row_data,(sprintf("/%s.csv",probe)))


clean_probe_data <- function(path_row_data, 
                             probe, 
                             startdatetime, 
                             enddatetime,
                             organic,
                             excess){
  
  # cat("\014")
  # rm(list = ls())
  # 
  library(dplyr)
  library(stringr)
  # 
  # graphics.off()
  # 
  # #set working directory and read data
  # 
  # setwd("C:/Users/maike/Desktop/Carbon4D/DatenbereinigungIrrimax/IrrimaxDataComplete")
  # 
  # 
  # 
  # probe <- "S15_014"
  # startdatetime <- "2022-12-23 00:00:00"
  # enddatetime <- "2023-07-26 23:59:00"
  # organic <- 12
  # excess <- 8
  # 
  

  #cleandata <- function(Dateiname,Sondenname,...){
  #        }
  
  
  bottomPortion <- as.numeric(str_extract(excess,"\\d$"))*0.1
  topPortion <- 1-bottomPortion
  TotalRemSensors <- trunc(excess/10)

  
  #calculating the number of sensores/columnes which are not completly in the soil
  #The ceiling function rounds a numeric input up to the next higher integer.
  #The trunc function truncates (i.e. cuts off) the decimal places of a numeric input.
  #numberRemovingSensors <- ceiling(excess/10)
  
  
  #select data set
  
  TD <- read.csv(paste0(path_row_data,(sprintf("/%s.csv",probe))))
  
  
  #fill missing columnes down to 120 cm with NA
  
  if (ncol(TD)<27) {
    TD[(ncol(TD)+1):27]<-NA
  }
  
  
  TD$datetime<-as.POSIXct(strptime(TD$Date.Time,"%Y/%m/%d %H:%M:%S",tz="UTC"))
  T<- subset(TD,datetime >= as.POSIXct(startdatetime,"%Y-%m-%d %H:%M:%S",tz="UTC") &
               datetime <= as.POSIXct(enddatetime,"%Y-%m-%d %H:%M:%S",tz="UTC"))
  
  
  #Zeitumstellung SOmmer und Winterzeit
  #T$datetime <- T$datetime-60*60
  
  
  
  #figure of row data
  #################################################################
  ##colors
  #pal <- colorRampPalette(c("red", "yellow"))
  #pal(10)
  
  par(fig=c(0,0.6,0.4,1), new=TRUE)
  
  plot(T$datetime,T$T1.5.,type="l",col="#FF0000",
       ylab = "Temperature [°C]",xlab=" ",xaxt='n',
       ylim=c(-5,20),las=1)
  lines(T$datetime,T$T2.15.,col="#FF1C00")
  lines(T$datetime,T$T3.25.,col="#FF3800")
  lines(T$datetime,T$T4.35.,col="#FF5500")
  lines(T$datetime,T$T5.45.,col="#FF7100")
  lines(T$datetime,T$T6.55.,col="#FF8D00")
  lines(T$datetime,T$T7.65.,col="#FFAA00")
  lines(T$datetime,T$T8.75.,col="#FFC600")
  lines(T$datetime,T$T9.85.,col="#FFE200")
  lines(T$datetime,T$T10.95.,col="#EDF31A")
  lines(T$datetime,T$T11.105.,col="#DCE734")
  lines(T$datetime,T$T12.115.,col="#CBDB4F")
  legend("topright", legend=c("0-10 cm", "10-20 cm ", "20-30 cm ", "30-40 cm ",
                              "40-50 cm ", "50-60 cm ", "60-70 cm ","70-80 cm",
                              "80-90 cm","90-100 cm","100-110 cm", "110-120 cm"),
         col=c("#FF0000", "#FF1C00","#FF3800","#FF5500","#FF7100","#FF8D00",
               "#FFAA00","#FFC600","#FFE200","#EDF31A","#DCE734","#CBDB4F"), lty=1, cex=1,)
  
  par(fig=c(0,0.6,0,0.6), new=TRUE)
  
  plot(T$datetime,T$A1.5.,type="l",col="#6495ED",
       ylab = "Soil Water Content [mm]",xlab = "Local Time",
       ylim =c(0,50),las=1)
  lines(T$datetime,T$A2.15.,col="#75A0D2")
  lines(T$datetime,T$A3.25.,col="#86ACB8")
  lines(T$datetime,T$A4.35.,col="#97B89E")
  lines(T$datetime,T$A5.45.,col="#A8C483")
  lines(T$datetime,T$A6.55.,col="#BACF69")
  lines(T$datetime,T$A7.65.,col="#CBDB4F")
  lines(T$datetime,T$A8.75.,col="#DCE734")
  lines(T$datetime,T$A9.85.,col="#EDF31A")
  lines(T$datetime,T$A10.95.,col="#FFE200")
  lines(T$datetime,T$A11.105.,col="#FFC600")
  lines(T$datetime,T$A12.115.,col="#FFAA00")
  legend("topright", legend=c("0-10 cm", "10-20 cm ","20-30 cm ", "30-40 cm ",
                              "40-50 cm ", "50-60 cm ", "60-70 cm ","70-80 cm",
                              "80-90 cm","90-100 cm","100-110 cm", "110-120 cm"),
         col=c("#6495ED","#75A0D2","#86ACB8","#97B89E","#A8C483","#BACF69",
               "#CBDB4F","#DCE734","#EDF31A","#FFE200","#FFC600","#FFAA00"), lty=1, cex=1)
  
  par(fig=c(0.6,1,0,1),new=TRUE)
  
  plot(mean(T$T1.5.),5,ylim=rev(range(c(0,120)))
       ,xlim=c(0,10),ylab="Depth [cm]",xlab="Temperature [°C]"
       ,col="red",pch = 19,las=1)
  points(mean(T$T2.15.),15,col="red",pch = 19)
  points(mean(T$T3.25.),25,col="red",pch = 19)
  points(mean(T$T4.35.),35,col="red",pch = 19)
  points(mean(T$T5.45.),45,col="red",pch = 19)
  points(mean(T$T6.55.),55,col="red",pch = 19)
  points(mean(T$T7.65.),65,col="red",pch = 19)
  points(mean(T$T8.75.),75,col="red",pch = 19)
  points(mean(T$T9.85.),85,col="red",pch = 19)
  points(mean(T$T10.95.),95,col="red",pch = 19)
  points(mean(T$T11.105.),105,col="red",pch = 19)
  points(mean(T$T12.115.),115,col="red",pch = 19)
  
  par(new=TRUE)
  
  plot(mean(T$A1.5.),5,
       ylab = "",xlab = "",
       ylim=rev(range(c(0,120))),xlim=c(0,50),
       axes=FALSE,col="blue",pch = 19)
  mtext("Soil Water Content [mm]",side=3,col="black",line=3) 
  axis(3, xlim=c(0,50), col="black",col.axis="black",las=1)
  points(mean(T$A1.5.),5,col="blue",pch = 19)
  points(mean(T$A2.15.),15,col="blue",pch = 19)
  points(mean(T$A3.25.),25,col="blue",pch = 19)
  points(mean(T$A4.35.),35,col="blue",pch = 19)
  points(mean(T$A5.45.),45,col="blue",pch = 19)
  points(mean(T$A6.55.),55,col="blue",pch = 19)
  points(mean(T$A7.65.),65,col="blue",pch = 19)
  points(mean(T$A8.75.),75,col="blue",pch = 19)
  points(mean(T$A9.85.),85,col="blue",pch = 19)
  points(mean(T$A10.95.),95,col="blue",pch = 19)
  points(mean(T$A11.105.),105,col="blue",pch = 19)
  points(mean(T$A12.115.),115,col="blue",pch = 19)
  
  
  
  #################################################################
  
  
  
  #remove time stamp, battery status and all columnes of sensors which are totally not in the soil
  
  T[1:(TotalRemSensors*2+3)]<-NULL
  
  datetime <- T$datetime
  
  T$datetime <- NULL
  
  if (ncol(T)<24) {
    T[(ncol(T)+1):24]<-NA
  }
  
  T$datetime <- datetime
  
  
  
  #set column to NA if the sensor didn`t match with the mineral soil 
  
  
  if (str_extract(excess,"\\d$")!=0) {
    T[1:2] <- NA
  }
  
  
  
  #create new table
  
  df <- data.frame(T$datetime)
  
  names(df) <- c("datetime")
  
  T$datetime <- NULL
  
  
  
  
  
  
  #####################################################
  
  #create a column for organic layer if there is one otherwise the first coulumn will filled with T_05
  
  #no organic
  #1,2,3,4
  
  if  (organic == 0) {
    df$T_org <- NA
    df$T_05 <- T[,2] # is NA if the sensor didn`t match with the mineral soil 
    df$T_15 <- T[,4]
    df$T_25 <- T[,6]
    df$T_35 <- T[,8]
    df$T_45 <- T[,10]
    df$T_55 <- T[,12]
    df$T_65 <- T[,14]
    df$T_75 <- T[,16]
    df$T_85 <- T[,18]
    df$T_95 <- T[,20]
    df$T_105 <- T[,22]
    df$T_115 <- T[,24]
  }
  
  #sensor is complete in 10 cm organic layer
  #5,5a
  
  if (organic==10 
      && as.numeric(str_extract(excess,"\\d$"))==0) {
    df$T_org <- T[,2]
    df$T_05 <- T[,4]
    df$T_15 <- T[,6]
    df$T_25 <- T[,8]
    df$T_35 <- T[,10]
    df$T_45 <- T[,12]
    df$T_55 <- T[,14]
    df$T_65 <- T[,16]
    df$T_75 <- T[,18]
    df$T_85 <- T[,20]
    df$T_95 <- T[,22]
    df$T_105 <- T[,24]
    df$T_115 <- NA #T[,26]
  } 
  
  #6
  
  if (organic==10 
      && as.numeric(str_extract(excess,"\\d$"))!=0) {
    df$T_org <- NA
    df$T_05 <- NA
    df$T_15 <- T[,6]
    df$T_25 <- T[,8]
    df$T_35 <- T[,10]
    df$T_45 <- T[,12]
    df$T_55 <- T[,14]
    df$T_65 <- T[,16]
    df$T_75 <- T[,18]
    df$T_85 <- T[,20]
    df$T_95 <- T[,22]
    df$T_105 <- T[,24]
    df$T_115 <- NA #T[,26]
  } 
  
  
  #organic layer is less than 10 cm 
  #10,10a,11
  
  if  (organic < 10 
       && organic > 0 
       && as.numeric(str_extract(excess,"\\d$"))+organic==10) {
    df$T_org <- NA
    df$T_05 <- T[,4]
    df$T_15 <- T[,6]
    df$T_25 <- T[,8]
    df$T_35 <- T[,10]
    df$T_45 <- T[,12]
    df$T_55 <- T[,14]
    df$T_65 <- T[,16]
    df$T_75 <- T[,18]
    df$T_85 <- T[,20]
    df$T_95 <- T[,22]
    df$T_105 <- T[,24]
    df$T_115 <- NA #T[,26]
  }
  
  #11,12
  if  (organic < 10 
       && organic > 0 
       && as.numeric(str_extract(excess,"\\d$"))+organic<10) {
    df$T_org <- NA
    df$T_05 <- NA
    df$T_15 <- T[,4]
    df$T_25 <- T[,6]
    df$T_35 <- T[,8]
    df$T_45 <- T[,10]
    df$T_55 <- T[,12]
    df$T_65 <- T[,14]
    df$T_75 <- T[,16]
    df$T_85 <- T[,18]
    df$T_95 <- T[,20]
    df$T_105 <- T[,22]
    df$T_115 <- T[,24]
  }
  
  #13
  if  (organic < 10 
       && organic > 0 
       && as.numeric(str_extract(excess,"\\d$"))+organic>10) {
    df$T_org <- NA
    df$T_05 <- NA
    df$T_15 <- T[,6]
    df$T_25 <- T[,8]
    df$T_35 <- T[,10]
    df$T_45 <- T[,12]
    df$T_55 <- T[,14]
    df$T_65 <- T[,16]
    df$T_75 <- T[,18]
    df$T_85 <- T[,20]
    df$T_95 <- T[,22]
    df$T_105 <- T[,24]
    df$T_115 <- NA #T[,26]
  }
  
  #organic layer is more than 10 cm 
  #7
  
  if (organic > 10 && as.numeric(str_extract(excess,"\\d$"))==0){
    df$T_org <- T[,2]
    df$T_05 <- NA
    df$T_15 <- T[,6]
    df$T_25 <- T[,8]
    df$T_35 <- T[,10]
    df$T_45 <- T[,12]
    df$T_55 <- T[,14]
    df$T_65 <- T[,16]
    df$T_75 <- T[,18]
    df$T_85 <- T[,20]
    df$T_95 <- T[,22]
    df$T_105 <- T[,24]
    df$T_115 <- NA #T[,26]
  }
  
  #8
  if (organic > 10 
      && as.numeric(str_extract(excess+organic,"\\d$"))==0){
    df$T_org <- T[,4]
    df$T_05 <- T[,6]
    df$T_15 <- T[,8]
    df$T_25 <- T[,10]
    df$T_35 <- T[,12]
    df$T_45 <- T[,14]
    df$T_55 <- T[,16]
    df$T_65 <- T[,18]
    df$T_75 <- T[,20]
    df$T_85 <- T[,22]
    df$T_95 <- T[,24]
    df$T_105 <- NA #T[,26]
    df$T_115 <- NA #T[,28]
  }
  
  
  
  #9
  
  if (organic > 10 
      && as.numeric(str_extract(excess+organic,"\\d$"))!=0 
      && as.numeric(str_extract(excess,"\\d$"))!=0
      && organic-(10-as.numeric(str_extract(excess,"\\d$")))<=10){
    df$T_org <- NA
    df$T_05 <- NA
    df$T_15 <- T[,6]
    df$T_25 <- T[,8]
    df$T_35 <- T[,10]
    df$T_45 <- T[,12]
    df$T_55 <- T[,14]
    df$T_65 <- T[,16]
    df$T_75 <- T[,18]
    df$T_85 <- T[,20]
    df$T_95 <- T[,22]
    df$T_105 <- T[,24]
    df$T_115 <- NA #T[,26]
  }
  
  #9a
  
  if (organic > 10 
      && as.numeric(str_extract(excess+organic,"\\d$"))!=0 
      && as.numeric(str_extract(excess,"\\d$"))!=0
      && organic-(10-as.numeric(str_extract(excess,"\\d$")))>=10){
    df$T_org <- T[,4]
    df$T_05 <- NA
    df$T_15 <- T[,8]
    df$T_25 <- T[,10]
    df$T_35 <- T[,12]
    df$T_45 <- T[,14]
    df$T_55 <- T[,16]
    df$T_65 <- T[,18]
    df$T_75 <- T[,20]
    df$T_85 <- T[,22]
    df$T_95 <- T[,24]
    df$T_105 <- NA #T[,26]
    df$T_115 <- NA #T[,28]
  }
  
  
  
  #fill missing columnes down to 120 cm with NA
  
  if (ncol(df)<14) {
    df[(ncol(df)+1):14]<-NA
  }
  
  
  #######################################################################
  
  
  #create a column for organic layer if there is one otherwise the first coulumn will filled with T_05
  
  #no organic
  #1,2,3,4
  
  #no organic
  #1,2,3,4
  
  if  (organic == 0) {
    df$M_org <- NA
    df$M_05 <- T[,1] # is NA if the sensor didn`t match with the mineral soil 
    df$M_15 <- T[,3]
    df$M_25 <- T[,5]
    df$M_35 <- T[,7]
    df$M_45 <- T[,9]
    df$M_55 <- T[,11]
    df$M_65 <- T[,13]
    df$M_75 <- T[,15]
    df$M_85 <- T[,17]
    df$M_95 <- T[,19]
    df$M_105 <- T[,21]
    df$M_115 <- T[,23]
  }
  
  #sensor is complete in 10 cm organic layer
  #5,5a
  
  if (organic==10 
      && as.numeric(str_extract(excess,"\\d$"))==0) {
    df$M_org <- T[,1]
    df$M_05 <- T[,3]
    df$M_15 <- T[,5]
    df$M_25 <- T[,7]
    df$M_35 <- T[,9]
    df$M_45 <- T[,11]
    df$M_55 <- T[,13]
    df$M_65 <- T[,15]
    df$M_75 <- T[,17]
    df$M_85 <- T[,19]
    df$M_95 <- T[,21]
    df$M_105 <- T[,23]
    df$M_115 <- NA #T[,25]
  } 
  
  #6
  
  if (organic==10 
      && as.numeric(str_extract(excess,"\\d$"))!=0) {
    df$M_org <- NA
    df$M_05 <- NA
    df$M_15 <- T[,5]
    df$M_25 <- T[,7]
    df$M_35 <- T[,9]
    df$M_45 <- T[,11]
    df$M_55 <- T[,13]
    df$M_65 <- T[,15]
    df$M_75 <- T[,17]
    df$M_85 <- T[,19]
    df$M_95 <- T[,21]
    df$M_105 <- T[,23]
    df$M_115 <- NA #T[,25]
  } 
  
  
  #organic layer is less than 10 cm 
  #10,10a,11
  
  if  (organic < 10 
       && organic > 0 
       && as.numeric(str_extract(excess,"\\d$"))+organic==10) {
    df$M_org <- NA
    df$M_05 <- T[,3]
    df$M_15 <- T[,5]
    df$M_25 <- T[,7]
    df$M_35 <- T[,9]
    df$M_45 <- T[,11]
    df$M_55 <- T[,13]
    df$M_65 <- T[,15]
    df$M_75 <- T[,17]
    df$M_85 <- T[,19]
    df$M_95 <- T[,21]
    df$M_105 <- T[,23]
    df$M_115 <- NA #T[,25]
  }
  
  #11,12
  if  (organic < 10 
       && organic > 0 
       && as.numeric(str_extract(excess,"\\d$"))+organic<10) {
    df$M_org <- NA
    df$M_05 <- NA
    df$M_15 <- T[,3]
    df$M_25 <- T[,5]
    df$M_35 <- T[,7]
    df$M_45 <- T[,9]
    df$M_55 <- T[,11]
    df$M_65 <- T[,13]
    df$M_75 <- T[,15]
    df$M_85 <- T[,17]
    df$M_95 <- T[,19]
    df$M_105 <- T[,21]
    df$M_115 <- T[,23]
  }
  
  #13
  if  (organic < 10 
       && organic > 0 
       && as.numeric(str_extract(excess,"\\d$"))+organic>10) {
    df$M_org <- NA
    df$M_05 <- NA
    df$M_15 <- T[,5]
    df$M_25 <- T[,7]
    df$M_35 <- T[,9]
    df$M_45 <- T[,11]
    df$M_55 <- T[,13]
    df$M_65 <- T[,15]
    df$M_75 <- T[,17]
    df$M_85 <- T[,19]
    df$M_95 <- T[,21]
    df$M_105 <- T[,23]
    df$M_115 <- NA #T[,25]
  }
  
  #organic layer is more than 10 cm 
  #7
  
  if (organic > 10 
      && as.numeric(str_extract(excess,"\\d$"))==0){
    df$M_org <- T[,1]
    df$M_05 <- NA
    df$M_15 <- T[,5]
    df$M_25 <- T[,7]
    df$M_35 <- T[,9]
    df$M_45 <- T[,11]
    df$M_55 <- T[,13]
    df$M_65 <- T[,15]
    df$M_75 <- T[,17]
    df$M_85 <- T[,19]
    df$M_95 <- T[,21]
    df$M_105 <- T[,23]
    df$M_115 <- NA #T[,25]
  }
  
  #8
  if (organic > 10 
      && as.numeric(str_extract(excess+organic,"\\d$"))==0){
    df$M_org <- T[,3]
    df$M_05 <- T[,5]
    df$M_15 <- T[,7]
    df$M_25 <- T[,9]
    df$M_35 <- T[,11]
    df$M_45 <- T[,13]
    df$M_55 <- T[,15]
    df$M_65 <- T[,17]
    df$M_75 <- T[,19]
    df$M_85 <- T[,21]
    df$M_95 <- T[,23]
    df$M_105 <- NA #T[,25]
    df$M_115 <- NA #T[,27]
  }
  
  
  #9
  
  if (organic > 10 
      && as.numeric(str_extract(excess+organic,"\\d$"))!=0 
      && as.numeric(str_extract(excess,"\\d$"))!=0
      && organic-(10-as.numeric(str_extract(excess,"\\d$")))<=10){
    df$M_org <- NA
    df$M_05 <- NA
    df$M_15 <- T[,5]
    df$M_25 <- T[,7]
    df$M_35 <- T[,9]
    df$M_45 <- T[,11]
    df$M_55 <- T[,13]
    df$M_65 <- T[,15]
    df$M_75 <- T[,17]
    df$M_85 <- T[,19]
    df$M_95 <- T[,21]
    df$M_105 <- T[,23]
    df$M_115 <- NA #T[,25]
  }
  
  #9a
  
  if (organic > 10 
      && as.numeric(str_extract(excess+organic,"\\d$"))!=0 
      && as.numeric(str_extract(excess,"\\d$"))!=0
      && organic-(10-as.numeric(str_extract(excess,"\\d$")))>=10){
    df$M_org <- T[,3]
    df$M_05 <- NA
    df$M_15 <- T[,7]
    df$M_25 <- T[,9]
    df$M_35 <- T[,11]
    df$M_45 <- T[,13]
    df$M_55 <- T[,15]
    df$M_65 <- T[,17]
    df$M_75 <- T[,19]
    df$M_85 <- T[,21]
    df$M_95 <- T[,23]
    df$M_105 <- NA #T[,25]
    df$M_115 <- NA #T[,27]
  }
  
  
  
  if (ncol(df)<27) {
    df[(ncol(df)+1):27]<-NA
  }
  
  

  # ##############################################################
  
  colnames(df) <-c("datetime","T_org","T_05","T_15","T_25","T_35","T_45","T_55","T_65","T_75","T_85","T_95","T_105","T_115",
                   "M_org","M_05","M_15","M_25","M_35","M_45","M_55","M_65","M_75","M_85","M_95","M_105","M_115")
  
  
  
  df_backup <- df
  
  
  
  ################################################################
  
  
  d <- data.frame(df$datetime)
  
  names(d) <- c("datetime")
  
  if (as.numeric(str_extract(excess+organic,"\\d$"))!=0){
    d$T_org <- df$T_org
    d$T_05 <- df$T_05*topPortion+df$T_15*bottomPortion
    d$T_15 <- df$T_15*topPortion+df$T_25*bottomPortion
    d$T_25 <- df$T_25*topPortion+df$T_35*bottomPortion
    d$T_35 <- df$T_35*topPortion+df$T_45*bottomPortion
    d$T_45 <- df$T_45*topPortion+df$T_55*bottomPortion
    d$T_55 <- df$T_55*topPortion+df$T_65*bottomPortion
    d$T_65 <- df$T_65*topPortion+df$T_75*bottomPortion
    d$T_75 <- df$T_75*topPortion+df$T_85*bottomPortion
    d$T_85 <- df$T_85*topPortion+df$T_95*bottomPortion
    d$T_95 <- df$T_95*topPortion+df$T_105*bottomPortion
    d$T_105 <- df$T_105*topPortion+df$T_115*bottomPortion
    
  }
  
  
  if(as.numeric(str_extract(excess+organic,"\\d$"))==0){
    d$T_org <- df$T_org
    d$T_05 <- df$T_05
    d$T_15 <- df$T_15
    d$T_25 <- df$T_25
    d$T_35 <- df$T_35
    d$T_45 <- df$T_45
    d$T_55 <- df$T_55
    d$T_65 <- df$T_65
    d$T_75 <- df$T_75
    d$T_85 <- df$T_85
    d$T_95 <- df$T_95
    d$T_105 <- df$T_105
    d$T_115 <- df$T_115
    
  }
  
  
  
  if (ncol(d)<14) {
    d[(ncol(d)+1):14]<-NA
  }
  
  
  
  if (as.numeric(str_extract(excess+organic,"\\d$"))!=0){
    d$M_org <- df$M_org
    d$M_05 <- df$M_05*topPortion+df$M_15*bottomPortion
    d$M_15 <- df$M_15*topPortion+df$M_25*bottomPortion
    d$M_25 <- df$M_25*topPortion+df$M_35*bottomPortion
    d$M_35 <- df$M_35*topPortion+df$M_45*bottomPortion
    d$M_45 <- df$M_45*topPortion+df$M_55*bottomPortion
    d$M_55 <- df$M_55*topPortion+df$M_65*bottomPortion
    d$M_65 <- df$M_65*topPortion+df$M_75*bottomPortion
    d$M_75 <- df$M_75*topPortion+df$M_85*bottomPortion
    d$M_85 <- df$M_85*topPortion+df$M_95*bottomPortion
    d$M_95 <- df$M_95*topPortion+df$M_105*bottomPortion
    d$M_105 <- df$M_105*topPortion+df$M_115*bottomPortion
  }
  
  
  if(as.numeric(str_extract(excess+organic,"\\d$"))==0){
    d$M_org <- df$M_org
    d$M_05 <- df$M_05
    d$M_15 <- df$M_15
    d$M_25 <- df$M_25
    d$M_35 <- df$M_35
    d$M_45 <- df$M_45
    d$M_55 <- df$M_55
    d$M_65 <- df$M_65
    d$M_75 <- df$M_75
    d$M_85 <- df$M_85
    d$M_95 <- df$M_95
    d$M_105 <- df$M_105
    d$M_115 <- df$M_115
  }
  
  
  if (ncol(d)<27) {
    d[(ncol(d)+1):27]<-NA
  }
  
  
  colnames(d) <-c("datetime","T_org","T_05","T_15","T_25","T_35","T_45","T_55","T_65","T_75","T_85","T_95","T_105","T_115",
                  "M_org","M_05","M_15","M_25","M_35","M_45","M_55","M_65","M_75","M_85","M_95","M_105","M_115")
  
  
  #plot of clean data
  ###########################################################

  graphics.off()
  
  par(fig=c(0,0.7,0.4,1), new=TRUE)
  
  
  plot(d$datetime,d$T_05,type="l",col="#FF0000",
       ylab = "Temperature [°C]",xlab=" ",xaxt='n',
       ylim=c(0,35),las=1)
  lines(d$datetime,d$T_15,col="#FF1C00")
  lines(d$datetime,d$T_25,col="#FF3800")
  lines(d$datetime,d$T_35,col="#FF5500")
  lines(d$datetime,d$T_45,col="#FF7100")
  lines(d$datetime,d$T_55,col="#FF8D00")
  lines(d$datetime,d$T_65,col="#FFAA00")
  lines(d$datetime,d$T_75,col="#FFC600")
  lines(d$datetime,d$T_85,col="#FFE200")
  lines(d$datetime,d$T_95,col="#EDF31A")
  lines(d$datetime,d$T_105,col="#DCE734")
  lines(d$datetime,d$T_115,col="#CBDB4F")
  legend("topright", legend=c("0-10 cm", "10-20 cm ", "20-30 cm ", "30-40 cm ",
                              "40-50 cm ", "50-60 cm ", "60-70 cm ","70-80 cm",
                              "80-90 cm","90-100 cm"),
         col=c("#FF0000", "#FF1C00","#FF3800","#FF5500","#FF7100","#FF8D00",
               "#FFAA00","#FFC600","#FFE200","#EDF31A"), lty=1, cex=1,)
  
  
  par(fig=c(0,0.7,0,0.6), new=TRUE)
  
  plot(d$datetime,d$M_05,type="l",col="#6495ED",
       ylab = "Soil Water Content [mm]",xlab = "Local Time",
       ylim =c(0,50),las=1)
  lines(d$datetime,d$M_15,col="#75A0D2")
  lines(d$datetime,d$M_25,col="#86ACB8")
  lines(d$datetime,d$M_35,col="#97B89E")
  lines(d$datetime,d$M_45,col="#A8C483")
  lines(d$datetime,d$M_55,col="#BACF69")
  lines(d$datetime,d$M_65,col="#CBDB4F")
  lines(d$datetime,d$M_75,col="#DCE734")
  lines(d$datetime,d$M_85,col="#EDF31A")
  lines(d$datetime,d$M_95,col="#FFE200")
  lines(d$datetime,d$M_105,col="#FFC600")
  lines(d$datetime,d$M_115,col="#FFAA00")
  legend("topright", legend=c("0-10 cm", "10-20 cm ","20-30 cm ", "30-40 cm ",
                              "40-50 cm ", "50-60 cm ", "60-70 cm ","70-80 cm",
                              "80-90 cm","90-100 cm"),
         col=c("#6495ED","#75A0D2","#86ACB8","#97B89E","#A8C483","#BACF69",
               "#CBDB4F","#DCE734","#EDF31A","#FFE200"), lty=1, cex=1)
  
  
  par(fig=c(0.7,1,0,1),new=TRUE)
  
  plot(mean(d$T_05),5,ylim=rev(range(c(-10,120)))
       ,xlim=c(0,30),ylab="Depth [cm]",xlab="Temperature [°C]"
       ,col="red",pch = 19,las=1)
  points(mean(d$T_15),15,col="red",pch = 19)
  points(mean(d$T_25),25,col="red",pch = 19)
  points(mean(d$T_35),35,col="red",pch = 19)
  points(mean(d$T_45),45,col="red",pch = 19)
  points(mean(d$T_55),55,col="red",pch = 19)
  points(mean(d$T_65),65,col="red",pch = 19)
  points(mean(d$T_75),75,col="red",pch = 19)
  points(mean(d$T_85),85,col="red",pch = 19)
  points(mean(d$T_95),95,col="red",pch = 19)
  points(mean(d$T_105),105,col="red",pch = 19)
  points(mean(d$T_115),115,col="red",pch = 19)
  points(mean(d$T_org),-5,col="red",pch=19)
  abline(h=0)
  
  par(new=TRUE)
  
  plot(mean(T$M_05),5,
       ylab = "",xlab = "",
       ylim=rev(range(c(-10,120))),xlim=c(0,50),
       axes=FALSE,col="blue",pch = 19)
  mtext("Soil Water Content [mm]",side=3,col="black",line=3) 
  axis(3, xlim=c(0,50), col="black",col.axis="black",las=1)
  points(mean(d$M_05),5,col="blue",pch = 19)
  points(mean(d$M_15),15,col="blue",pch = 19)
  points(mean(d$M_25),25,col="blue",pch = 19)
  points(mean(d$M_35),35,col="blue",pch = 19)
  points(mean(d$M_45),45,col="blue",pch = 19)
  points(mean(d$M_55),55,col="blue",pch = 19)
  points(mean(d$M_65),65,col="blue",pch = 19)
  points(mean(d$M_75),75,col="blue",pch = 19)
  points(mean(d$M_85),85,col="blue",pch = 19)
  points(mean(d$M_95),95,col="blue",pch = 19)
  points(mean(d$M_105),105,col="blue",pch = 19)
  points(mean(d$M_115),115,col="blue",pch = 19)
  points(mean(d$M_org),(-5),col="blue",pch=19)
  
  
  write.csv(d,(paste0(path_row_data,(sprintf("/%s_clean.csv",probe)))),row.names=FALSE)
  
  
}
