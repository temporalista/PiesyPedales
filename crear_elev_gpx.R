##Este script toma un directorio de archivos gpx para crear trayectorias espacio-temporales
##utilizando el eje (z) para representar el tiempo. El resultado es que cada trayerctoria (archivo gpx)
##tiene coordenadas x,y,z, donde z (en metros) representa el tiempo transcurrido (en segundos) desde el
##inicio de la trayectoria.
##El input es un directorio con archivos gpx (sin otro tipo de archivos)
##Script realizado por Mateo Neira (LlactaLAB - Ciudades Sustentables, Universidad de Cuenca)


#cargar libreria de rgdal para leer archivos gpx y chron para trabajar con datos de tiempo.
library(chron)
library(rgdal)

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


#leer todos los archivos de un directorio de entrada
leer<-function(directorio){
  #leer todos los archivos de un directorio de entrada
  setwd(directorio)
  files<-list.files()
  id<-as.integer(length(files))
  id<-c(1:id)
  print(id)
  for (i in id){
    print(i)
    #leer archivo gpx
    (layers <- ogrListLayers(files[i]))
    data <- readOGR(files[i], layer=layers[5])
    
    #agregar tiempo en formato tiempo
    data$temp<-substr(data$time, 12, 19)
    data$tiempo<-chron(times=data$temp)
    
    #grabar minimo del valor tiempo
    data$id<-paste(data$track_fid,data$track_seg_id, sep="")
    data$mintime<-with(data,ave(data$tiempo,data$id,FUN=min))
    
    #agregar tiempo reescalado segun minimo
    data$tiempo_E<-as.character(data$tiempo-data$mintime)
    
    #agregar tiempo en segundos
    data$seg<-sapply(data$tiempo_E, segundos)
    
    #cambiar formato tiempo, tiempo debe tener una fecha
    data$time<-paste("2015/02/02", data$tiempo_E, sep=" ")
    
    #crear elevacion a partir de segundos // factor de conversion puede cambiar
    data$ele<-data$seg
    
    #elimnar columnas innecesarias
    data$temp<-NULL
    data$tiempo<-NULL
    data$tiempo_E<-NULL
    data$seg<-NULL
    data$mintime<-NULL
    data$id<-NULL
    
    
    #escribir archivos
    nombre1<-paste(i,"_elev",".gpx",sep="")
    print(nombre1)
    writeOGR(data, dsn=nombre1, layer="track_points", driver="GPX", dataset_options="GPX_USE_EXTENSIONS=yes")
  }
  print("acabado!")
}

