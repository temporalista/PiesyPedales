#cargar libreria de plotKML,rgdal,sp, permite leer archivos gpx y chron para trabajar con tiempos
library(plotKML)
library(chron)
library(rgdal)
library(sp)


#funcion para convertir el tiempo a segundos
segundos <- function(x)
{
  hhmmss <- strsplit (x, ":", T)
  hh <- as.numeric (hhmmss[[1]][1])
  mm <- as.numeric (hhmmss[[1]][2])
  ss <- as.numeric (hhmmss[[1]][3])
  return (hh * 60 * 60 + mm * 60 + ss)
}

#funcion crear tiempo
crear_tiempo<-function(x){
  return(paste(substr(x,1,10),substr(x,12,19), sep=" "))
  
}


#leer todos los archivos de un directorio especifico
leer<-function(directorio){
  setwd(directorio)
  files<-list.files()
  id<-as.integer(length(files))
  id<-c(1:id)
  print(id)
  name<-0
  for (i in id){
    name<-name+1
    print(name)
    #leer archivo gpx
    raw<-readGPX(files[i])
    #limpiar datos
    data<-raw[4][[1]][[1]][[1]]
    #agregar tiempo en formato tiempo
    data["temp"]<-substr(data$time, 12, 19)
    data["tiempo"]<-chron(times=data$temp)
    #grabar minimo del valor tiempo
    mintime<-min(data$tiempo)
    #agregar tiempo reescalado segun minimo
    data["tiempo_E"]<-as.character(data$tiempo-mintime)
    #agregar tiempo en segundos
    data["seg"]<-sapply(data$tiempo_E, segundos)
    #cambiar formato tiempo
    data["time"]<-sapply(data$time, crear_tiempo)
    #crear elevacion a partir de segundos // factor de conversion puede cambiar
    data["ele"]<-data$seg
    #elimnar columnas innecesarias
    data$temp<-NULL
    data$tiempo<-NULL
    data$tiempo_E<-NULL
    data$seg<-NULL
    
    #escribir archivos
    nombre1<-paste(i,"_elev",".gpx",sep="")
    gpx_new<-SpatialPointsDataFrame(data=data,  coords=data[,1:2], proj4string=CRS("+proj=longlat +datum=WGS84"))
    writeOGR(gpx_new, dsn=nombre1, layer="trackpoints", driver="GPX", dataset_options="GPX_USE_EXTENSIONS=yes")
    
    
    
  }
  print("acabado!")
}
