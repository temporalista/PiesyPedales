library(plyr)
library(dplyr)
library(data.table)


mydata <- read.csv ("processed/moves.csv",header = T,stringsAsFactors=FALSE)

traj_sum <- ddply(mydata,"trajnum",summarise,
                  time_start = first(timestamp),
                  time_end = last(timestamp),
                  dur_min= round(difftime(time_end, time_start, units = "mins"),2),
                  mean_speed= mean(speed)
                  )

traj_sum1 <- ddply(mydata,"trajnum",summarise,
                  lat_ori=first(mydata,order_by = "timestamp")
                  )

mydata = as.data.table(mydata)
traj_sum[,c("lat_ori","lon_ori", "lat_des","lon_des")]  <- c(
  mydata[, .SD[c(1)],by=trajnum]$lat,
  mydata[, .SD[c(1)],by=trajnum]$lon,
  mydata[, .SD[c(.N)],by=trajnum]$lat,
  mydata[, .SD[c(.N)],by=trajnum]$lon)

write.csv(traj_sum,file="processed/trajectories.csv")

traj_lines <- mydata[, .SD[c(1,.N)], .SDcols=c("lat","lon","seq","timestamp"), by=trajnum]
write.csv(traj_lines,file="processed/traj_lines.csv")

