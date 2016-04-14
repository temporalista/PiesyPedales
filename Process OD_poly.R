##This script takes the STRAVA's "edges_ride_od_polygons_data" and parses it into a sequenced list
##Resulting file has 3 columns: seq_n (number of sequence inside a trip), trip_id, Poly_id.
##Daniel Orellana, 2015-09-11

  
install.packages("dplyr")
library('dplyr')
library('stringr')

##"/Volumes/Mac Data/GEO/STRAVA/Originales/StravaMetroEcuador2015CyclingDev/ecuador_edges_ride_od_polygons_data.csv"
##"OD_sequences.csv"
##nrows(polys)
##setwd("~/Google Drive/GEO LLACTALAB/STRAVA/temporales")
CreateTripSequence <- function(
                            StravaODfile="/Volumes/MacData/GEO/STRAVA/Originales/StravaMetroEcuador2015CyclingDev/ecuador_edges_ride_od_polygons_data.csv",
                            NumTrips=10,
                            SeqFile="Trip_Sequences"
                            )
  {

    ##Create a Dataframe with details of each trip
    Trip_Data <- read.csv(
    StravaODfile, 
    stringsAsFactors=F)
  
    ##Number of trips to process, if 0 then all.
    NumTrips <- ifelse (NumTrips==0, nrow(Trip_Data), NumTrips)
    
    ##keep only NumTrips
    Trip_Data <- Trip_Data[1:NumTrips,]
    
    #Create a new column and populate with autonumeric trip_id.
    Trip_Data <- mutate(Trip_Data, trip_id=row.names(Trip_Data))
    
    #create column with fulls sequence of polygons.
    Trip_Data$sequence <- paste(Trip_Data$polygon_id,gsub("{*}*","",Trip_Data$intersected_polygons,perl = T),Trip_Data$dest_polygon_id,sep = ",")
    Trip_Data$NumPolys <- str_count(Trip_Data$sequence,",")+1
    Trip_Data$Date <- as.Date(Trip_Data$day - 1, origin = paste(Trip_Data$year,"-01-01", sep=""))
#     Trip_Data$
#       DateTime <- strptime(paste(Trip_Data$Date, Trip_Data$hour,Trip_Data$minute, sep=":"),"%Y-%m-%D:%H:%M")
#     
    #Reorder columns and keep selected trips
    Trip_Data <- Trip_Data[1:NumTrips,c(9,1,2,3,4,5,6,7,11,8,10)]
    
    #Create csv with trip data
    write.table(Trip_Data,paste("Trip_Data","_",NumTrips,".csv"), sep=",",row.names = F)

    
    #template csv for sequences
    template <- data.frame("seq_n"=integer(), "trip_id"=integer(), "poly_id"=integer())
    SeqFileName <- paste(SeqFile,"_",NumTrips,".csv",sep = "")
    write.table(template, SeqFileName, sep = ",")
      
    ##take each row and extract the polyg_ids, then populate a DF with them, 
    ## adding the seq number and trip_id. Finally append them to the csv
    
    
    for (i in 1:NumTrips)
      {
      poly_id <- unlist(strsplit(as.character(Trip_Data[i,10]),","))
      seq <- cbind(tripid=as.numeric(i),poly_id=as.numeric(poly_id))
      write.table(seq, SeqFileName, quote = F, append = T,col.names = F, sep = ",")
    }
}

AddPointCoordinates <- function(){
  points <- read.csv("SECT_POINT.csv")
  points <- subset(points, select = c(ID,Lon,Lat))
  OD_seq <- read.csv("OD_sequences.csv")
  total <- merge(OD_seq,points, by.x="poly_id", by.y = "ID")
  write.csv(total,"SeqPoints.csv", row.names = F)
}
