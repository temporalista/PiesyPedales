library(plyr)
library(dplyr)
library(data.table)

##read trackpoints data
mydata <- read.csv ("processed/moves.csv",header = T,stringsAsFactors=FALSE)

##compute traj statistics
traj_sum <- ddply(mydata,"trajnum",summarise,
                  time_start = first(timestamp),
                  time_end = last(timestamp),
                  dur_min= round(difftime(time_end, time_start, units = "mins"),2),
                  mean_speed= mean(speed)
                  )
#convert to data.table and compute origin and destination coordinates.
mydata = as.data.table(mydata)
traj_sum[,c("lat_ori","lon_ori", "lat_des","lon_des")]  <- c(
  mydata[, .SD[c(1)],by=trajnum]$lat,
  mydata[, .SD[c(1)],by=trajnum]$lon,
  mydata[, .SD[c(.N)],by=trajnum]$lat,
  mydata[, .SD[c(.N)],by=trajnum]$lon)

#save results in csv
write.csv(traj_sum,file="processed/trajectories.csv")

##2line-per-OD format
traj_lines <- mydata[, .SD[c(1,.N)], .SDcols=c("lat","lon","seq","timestamp"), by=trajnum]
write.csv(traj_lines,file="processed/OD_lines.csv")

