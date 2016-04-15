##Processing GPX files
##Input: gpx raw file
##outputs

##Work in progress

packages <- c("XML","rgdal","lubridate", "raster", "sp", "dplyr")

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

lapply(packages, require, character.only = TRUE)

#función para mover el vector un puesto (velocidad y dirección)
shift.vec <- function (vec, shift) {
  if(length(vec) <= abs(shift)) {
    rep(NA ,length(vec))
  }else{
    if (shift >= 0) {
      c(rep(NA, shift), vec[1:(length(vec)-shift)]) }
    else {
      c(vec[(abs(shift)+1):length(vec)], rep(NA, abs(shift))) } } }


#leer archivo gps
setwd("/Volumes/MacData/odrive/GDrive_UC/geo/Exp PyP/2015_12_22_Experimento 2G/P0021135423/")
gpsfile <- "15101501.gpx"
layers <- ogrListLayers(gpsfile)
gpsdata <- readOGR(gpsfile, layer=layers[5])

# #agregar tiempo en formato tiempo
# gpsdata$temp<-substr(gpsdata$time, 12, 19)
# gpsdata$tiempo<-chron(times=gpsdata$temp)

trackids <- gpsdata$track_fid
segmentids <- gpsdata$track_seg_id
pointsids <- gpsdata$track_seg_point_id
elevs <- gpsdata$ele
times <- gpsdata$time
lons <- as.numeric(gpsdata$coords.x1)
lats <- as.numeric(gpsdata$coords.x2)

gps_df <- data.frame(trackid =trackids, segmentid=segmentids, pointid=pointsids, lat = lats, lon = lons, ele = elevs, time = times)
rm(list=c("trackids","segmentids","pointsids", "elevs", "lats", "lons", "times", "gpsdata"))

# Shift vectors for lat and lon so that each row also contains the previous position.
gps_df$lat.p1 <- shift.vec(gps_df$lat, 1)
gps_df$lon.p1 <- shift.vec(gps_df$lon, 1)


# Calculate distances (in metres) using the function pointDistance from the ‘raster’ package.
# Parameter ‘lonlat’ has to be TRUE!
gps_df$dist <- apply(gps_df, 1, FUN = function (row) {
  pointDistance(c(as.numeric(row["lat.p1"]),
                  as.numeric(row["lon.p1"])),
                c(as.numeric(row["lat"]), as.numeric(row["lon"])),
                lonlat = T)
})


# Calculate the number of seconds between two positions.
gps_df$timestep <- as.numeric(
  difftime(
    strptime(gps_df$time, format = "%Y/%m/%d %H:%M:%S"), 
    strptime(lag(gps_df$time), format = "%Y/%m/%d %H:%M:%S")))


# Calculate metres per seconds, kilometres per hour and two LOWESS smoothers to get rid of some noise.
gps_df$mps <- gps_df$dist / gps_df$timestep
gps_df$kph <- gps_df$mps * 3.6
gps_df$mps_low <- lowess(gps_df$mps, f = 0.01)$y
#gps_df$kph <- ifelse(is.na(gps_df$kph), 0, gps_df$kph)

sd(gps_df$mps, na.rm=T)
mean(gps_df$mps, na.rm=T)
summary(gps_df$kph)

#distance based segmentation
##Chequear
registros <- c(1:nrow(gps_df))
gps_df$seq <- registros
segn=1

for (i in registros){
  
  if (!is.na(gps_df[i,"kph"])){
    if(gps_df[i,"kph"]>10){
      segn <- segn + 1
      seq = 1
    }
    seq <- seq + 1
    gps_df[i,"seq"] <- seq
    gps_df[i,"segmentid"] <- segn
  }
}
  



# 
# 
# gps_tr <- gps_df[gps_df$trackid==0,]
# 
# # Plot elevations and smoother
# gps_tr$lowess.ele <- lowess(gps_tr$ele, f = 0.1)$y
# plot(gps_tr$ele, type = "l", bty = "n", xaxt = "n",
#      ylab = "Elevation", xlab = "time",col = "grey40")
# lines(gps_tr$lowess.ele, col = "red", lwd = 2)
# legend(x="bottomright", legend = c("GPS elevation", "LOWESS elevation"), 
#        col = c("grey40", "red"), lwd = c(1,3), bty = "n")
# 
# 
# # Plot speed and smoother
plot(gps_df$speed.km.per.h, type = "l", bty = "n",
     xaxt = "n", ylab = "Speed", xlab = "",col = "grey40")
lines(gps_tr$lowess.speed, col = "blue", lwd = 3)
legend(x="bottomright", legend = c("GPS speed", "LOWESS speed"), 
       col = c("grey40", "blue"), lwd = c(1,3), bty = "n")
# 
# plot(gps_tr$lowess.ele,type="o")
# lines(gps_tr$lowess.speed, col="blue")
# 
# # Plot distances 
# plot(gps_tr$dist, type = "l", bty = "n",
#      xaxt = "n", ylab = "distance", xlab = "",col = "grey40")
# legend(x="bottomright", legend = c("distance"), 
#        col = c("grey40", "blue"), lwd = c(1,3), bty = "n")

write.csv(gps_df, file=paste(gpsfile,".csv", sep = ""))

coordinates(gps_df) = ~lon+lat
gps_df <-  as.data.frame(gps_df)

spplot(gps_df["segmentid"])

