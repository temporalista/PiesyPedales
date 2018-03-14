################################################################
# This script creates a dataframe with polygon contiguity data
# BY: Daniel Orellana - LlactaLAB Universidad de Cuenca
# March 2018
################################################################


###Source packloader for easely load packs
#install.packages("devtools")
library(devtools)
source("https://raw.githubusercontent.com/temporalista/r-essentials/master/packloader.R")

##Load required packs
ipak(c("rgdal", "rgeos", "reshape"))

setwd("~/GDrive_UC/geo/strava")

#read some polygon data
sectores.sp<-readOGR(dsn = "Procesados/sectores_ecuador.geojson")

#use variable ID as rownames instead of original shape id's
sectores.sp <- spChFIDs(poly.sp, as.character(poly.sp$ID))

#optionally select only one province
#poly.sp <- subset(sectores.sp,COD_PROV=="01")


#get contiguity matrix
cm<-gTouches(poly.sp, byid = TRUE)

#melt the data to long form
cm.melt <- melt(cm)
colnames(cm.t) <- c("poly1","poly2","touches")

#keep only rows with contiguity
cm.t <- subset(cm.melt,value=="TRUE")

#save contiguity data
write.csv(cm.t, "Procesados/contiguity_ecuador.csv",row.names = FALSE)

