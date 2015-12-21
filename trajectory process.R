##csv template for trajectories
etf <- cbind("trajnum","lat_ori", "lon_ori", "lat_des", "lon_des","start_time", "end_time", "duration")
write.table(etf,file="processed/trajectories.csv", sep = ",", col.names = F, na = "", row.names = F)

moves <- read.csv ("moves.csv",header = T,stringsAsFactors=FALSE)
for 
