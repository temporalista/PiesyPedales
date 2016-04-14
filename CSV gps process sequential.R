
##TODO: 
#Chequear segmentación por tiempo: varias trayectorias diferentes tienen el mismo trajnum

##get all csv in working directory
files <- dir(getwd(), pattern = '\\.csv', full.names = TRUE)

##csv template for trackpoints
edf <- cbind("id","lat","lon","alt","speed","time_posix","time_posix_sec","timestamp","trajnum","seq","timestep")
write.table(edf,file="processed/moves.csv", sep = ",", col.names = F, na = "", row.names = F)

#initialize trajectory
trajnum=0

for (f in seq_along(files))  {
  trajnum <- trajnum + 1
  mydata <- read.csv (files[f],header = F,stringsAsFactors=FALSE)
  ##subset rows with data
  mydata <- mydata[which(as.numeric(mydata$V5)>0),1:5]
  colnames(mydata) <- c("lat","lon","alt","speed","time_posix")
  cols = c("lat","lon","alt","speed", "time_posix")    
  mydata[,cols]  <-  apply(mydata[,cols], 2, function(x) as.numeric(as.character(x)))
  
  #convert POSIX time
  mydata$time_posix_sec <- as.numeric(mydata$time_posix) / 1000
  mydata$timestamp <- as.POSIXct(mydata$time_posix_sec, origin="1970-01-01")
  mydata <- mydata[order(mydata$time_posix_sec),]
  
  ##start computation at the second row (first row has nothing behind to compare with)
  registros <- c(1:nrow(mydata))
  #registros <- c(1:1000)
  seq=1
  mydata[1,"trajnum"] <- trajnum
  mydata[1,"seq"] <- seq
  for (i in registros){

    if (i>1){
      timestep <- mydata[i,"time_posix_sec"] - mydata[i-1,"time_posix_sec"]
      if(timestep>300){
        trajnum <- trajnum + 1
        seq = 0
      }
      seq <- seq + 1
      mydata[i,"timestep"] <- timestep
      mydata[i,"trajnum"] <- trajnum
      mydata[i,"seq"] <- seq
      
    }

  }
  write.table(mydata,file="processed/moves.csv", append = T,sep = ",",col.names = F, na = "")
  print(files[f])
}

