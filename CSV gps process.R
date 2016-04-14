## PROCESSING CSV TRACK FILES AND SPLIT TRACKS BY TIME

library(sp)
#library(trajectory)
##LOAD MULTIPLE CSV FILES FROM WORKING DIRECTORY
multiloader <- function(path) { 
  files <- dir(path, pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, read.csv, header=F, stringsAsFactors=FALSE)
  do.call(rbind, tables)
}

setwd("/Volumes/Mac Data/Geo/piesypedales/EXP2_data/originales")

##create df with data and rearrange & rename columns
mydata <- multiloader(getwd())
##subset rows with data
mydata <- mydata[which(as.numeric(mydata$V5)>0),1:5]


colnames(mydata) <- c("lat","lon","alt","speed","time_posix")
cols = c("lat","lon","alt","speed", "time_posix")    
mydata[,cols]  <-  apply(mydata[,cols], 2, function(x) as.numeric(x))

#convert POSIX time
mydata$time_posix_sec <- as.numeric(mydata$time_posix) / 1000
mydata$timestamp <- as.POSIXct(mydata$time_posix_sec, origin="1970-01-01", tz="GMT+5")

#new columns
mydata["timestep"]=""
mydata["trajnum"]=""
mydata["seq"]=""

mydata <- mydata[order(mydata$time_posix_sec),]

str(mydata)

#promote to spatial dataframe
#coordinates(mydata)= ~lon + lat
#plot(mydata)

#compute timstep and trajnum
registros <- c(2:nrow(mydata))
#registros <- c(1000:2000)
trajnum=1
seq=1
mydata[1,"trajnum"] <- trajnum
mydata[1,"seq"] <- seq
for (i in registros){
    timestep <- mydata[i,"time_posix_sec"] - mydata[i-1,"time_posix_sec"]
    if(timestep>300){
      trajnum <- trajnum + 1
      seq = 1
    }
      mydata[i,"timestep"] <- timestep
      mydata[i,"trajnum"] <- trajnum
      mydata[i,"seq"] <- seq
      seq <- seq+1
      
      
  }

View(mydata[1000:2000,])
#save resulting csv
write.csv(mydata,file="processed/moves1.csv")
