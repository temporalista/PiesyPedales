source("/Volumes/MacData/odrive/GDrive_UC/Rspace/packloader.R")

ipak(c(
  "sp",
  "R2G2",
  "plyr"
))

setwd("/Volumes/MacData/odrive/GDrive_UC/geo/strava/temporales")

seq.df <- read.csv("SeqPoints.csv")
seq.df.ss <- subset(seq.df, trip_id<1000)
sectores <- read.csv("SECT_POINT.csv")
sectores.ss <- subset(sectores, COD_PARROQ=="10150", select = c("ID","COD_PARROQ","PARROQ"))

head(seq.df)
head(sectores.ss)

origin.df <- 

merged.df <- merge(seq.df,sectores.ss, by.x="poly_id", by.y="ID", all=F)

merged.df <- arrange(merged.df,trip_id,seq_n)

PolyLines2GE(coords = merged.df[, c("Lon","Lat")], 
             nesting = merged.df[, "trip_id"], 
             colors = "auto",
             goo = "Lines2GE_V1c.kml", 
             maxAlt = merged.df[, "seq_n"]*300,
             fill = FALSE,
             closepoly = FALSE,
             lwd = 0.1,
             extrude = 0)
