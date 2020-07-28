# Instalamos (si se requiere) y cargamos las librerias necesarias
if(!require(httr)) install.packages("httr")
if(!require(stringr)) install.packages("stringr")
if(!require(plyr)) install.packages("plyr")
library(httr)
library(stringr)
library(plyr)

# Definimos la url base de descarga
url<-"https://raw.githubusercontent.com/JuanferMG/TFM/master"

# *****************************

# Descargamos el arhivo del INE que hemos depurado, renombrado como 
# 0.1.csv y que se encuentra en mi cuenta de GitHub
url01<-paste0(url,"/0.1.csv")
GET(url01, write_disk(datos01<- tempfile(fileext = ".csv")))
datos_31250<-read.csv(file = datos01, sep = ";", 
                      encoding = "UTF-8", header = F, 
                      colClasses = "character")
# Ajustamos las cabeceras
for(i in 1:1){
  for(j in 2:ncol(datos_31250)){
    if(datos_31250[i,j]==""){
      datos_31250[i,j]<-datos_31250[i,j-1]
    }
  }
}
# Filtramos los valores de la ciudad 
datos_31250<-datos_31250[c(1:2,grep(pattern = "Val\u00E8ncia", x = datos_31250[,1])),]
# Normalizamos los datos
df<-c()
for(j in 2:ncol(datos_31250)){
  df_inter<-data.frame(paste0(datos_31250[c(3:nrow(datos_31250)),1],". ", datos_31250[1,j],"."), as.numeric(datos_31250[2,j]), as.numeric(gsub(pattern = ",", replacement = "\\.", x = gsub(pattern = "\\.", replacement = "", x = datos_31250[c(3:nrow(datos_31250)),j]))))
  colnames(df_inter)<-c("Nombre", "Anyo", "Valor")
  df<-rbind(df,df_inter)
}
datos_31250_val2<-df
# Quitamos el codigo numerico del territorio
datos_31250_val2[,1]<-sub("^\\S+\\s+", '', datos_31250_val2[,1])


# *****************************

# Repetimos estos pasos con el resto de archivos del INE

url02<-paste0(url,"/0.2.csv")
GET(url02, write_disk(datos02<- tempfile(fileext = ".csv")))
datos_31251<-read.csv(file = datos02, sep = ";", 
                      encoding = "UTF-8", header = F, 
                      colClasses = "character")
for(i in 1:1){
  for(j in 2:ncol(datos_31251)){
    if(datos_31251[i,j]==""){
      datos_31251[i,j]<-datos_31251[i,j-1]
    }
  }
}
datos_31251<-datos_31251[c(1:2,grep(pattern = "Val\u00E8ncia", x = datos_31251[,1])),]
df<-c()
for(j in 2:ncol(datos_31251)){
  df_inter<-data.frame(paste0(datos_31251[c(3:nrow(datos_31251)),1],". ", datos_31251[1,j],"."), as.numeric(datos_31251[2,j]), as.numeric(gsub(pattern = ",", replacement = "\\.", x = gsub(pattern = "\\.", replacement = "", x = datos_31251[c(3:nrow(datos_31251)),j]))))
  colnames(df_inter)<-c("Nombre", "Anyo", "Valor")
  df<-rbind(df,df_inter)
}
datos_31251_val2<-df
datos_31251_val2[,1]<-sub("^\\S+\\s+", '', datos_31251_val2[,1])

# *************************

url03<-paste0(url,"/0.3.csv")
GET(url03, write_disk(datos03<- tempfile(fileext = ".csv")))
datos_31252<-read.csv(file = datos03, sep = ";", 
                      encoding = "UTF-8", header = F, 
                      colClasses = "character")
for(i in 1:2){
  for(j in 2:ncol(datos_31252)){
    if(datos_31252[i,j]==""){
      datos_31252[i,j]<-datos_31252[i,j-1]
    }
  }
}
datos_31252<-datos_31252[c(1:3,grep(pattern = "Val\u00E8ncia", x = datos_31252[,1])),]
df<-c()
for(j in 2:ncol(datos_31252)){
  df_inter<-data.frame(paste0(datos_31252[c(4:nrow(datos_31252)),1],". ", datos_31252[1,j], ". ", datos_31252[2,j],"."), as.numeric(datos_31252[3,j]), as.numeric(gsub(pattern = ",", replacement = "\\.", x = gsub(pattern = "\\.", replacement = "", x = datos_31252[c(4:nrow(datos_31252)),j]))))
  colnames(df_inter)<-c("Nombre", "Anyo", "Valor")
  df<-rbind(df,df_inter)
}
datos_31252_val2<-df
datos_31252_val2[,1]<-sub("^\\S+\\s+", '', datos_31252_val2[,1])

# *************************

url04<-paste0(url,"/0.4.csv")
GET(url04, write_disk(datos04<- tempfile(fileext = ".csv")))
datos_31253<-read.csv(file = datos04, sep = ";", 
                      encoding = "UTF-8", header = F, 
                      colClasses = "character")
for(i in 1:3){
  for(j in 2:ncol(datos_31253)){
    if(datos_31253[i,j]==""){
      datos_31253[i,j]<-datos_31253[i,j-1]
    }
  }
}
datos_31253<-datos_31253[c(1:4,grep(pattern = "Val\u00E8ncia", x = datos_31253[,1])),]
df<-c()
for(j in 2:ncol(datos_31253)){
  df_inter<-data.frame(paste0(datos_31253[c(5:nrow(datos_31253)),1],". ", datos_31253[1,j], ". ", datos_31253[2,j], ". ", datos_31253[3,j],"."), as.numeric(datos_31253[4,j]), as.numeric(gsub(pattern = ",", replacement = "\\.", x = datos_31253[c(5:nrow(datos_31253)),j])))
  colnames(df_inter)<-c("Nombre", "Anyo", "Valor")
  df<-rbind(df,df_inter)
}
datos_31253_val2<-df
datos_31253_val2[,1]<-sub("^\\S+\\s+", '', datos_31253_val2[,1])

# *************************

url05<-paste0(url,"/0.5.csv")
GET(url05, write_disk(datos05<- tempfile(fileext = ".csv")))
datos_31254<-read.csv(file = datos05, sep = ";", 
                      encoding = "UTF-8", header = F, 
                      colClasses = "character")
for(i in 1:3){
  for(j in 2:ncol(datos_31254)){
    if(datos_31254[i,j]==""){
      datos_31254[i,j]<-datos_31254[i,j-1]
    }
  }
}
datos_31254<-datos_31254[c(1:4,grep(pattern = "Val\u00E8ncia", x = datos_31254[,1])),]
df<-c()
for(j in 2:ncol(datos_31254)){
  df_inter<-data.frame(paste0(datos_31254[c(5:nrow(datos_31254)),1],". ", datos_31254[1,j], ". ", datos_31254[2,j], ". ", datos_31254[3,j],"."), as.numeric(datos_31254[4,j]), as.numeric(gsub(pattern = ",", replacement = "\\.", x = datos_31254[c(5:nrow(datos_31254)),j])))
  colnames(df_inter)<-c("Nombre", "Anyo", "Valor")
  df<-rbind(df,df_inter)
}
datos_31254_val2<-df
datos_31254_val2[,1]<-sub("^\\S+\\s+", '', datos_31254_val2[,1])

# *************************

url06<-paste0(url,"/0.6.csv")
GET(url06, write_disk(datos06<- tempfile(fileext = ".csv")))
datos_31255<-read.csv(file = datos06, sep = ";", 
                      encoding = "UTF-8", header = F, 
                      colClasses = "character")
for(i in 1:2){
  for(j in 2:ncol(datos_31255)){
    if(datos_31255[i,j]==""){
      datos_31255[i,j]<-datos_31255[i,j-1]
    }
  }
}
datos_31255<-datos_31255[c(1:3,grep(pattern = "Val\u00E8ncia", x = datos_31255[,1])),]
df<-c()
for(j in 2:ncol(datos_31255)){
  df_inter<-data.frame(paste0(datos_31255[c(4:nrow(datos_31255)),1],". ", datos_31255[1,j],". ", datos_31255[2,j],"."), as.numeric(datos_31255[3,j]), as.numeric(gsub(pattern = ",", replacement = "\\.", x = gsub(pattern = "\\.", replacement = "", x = datos_31255[c(4:nrow(datos_31255)),j]))))
  colnames(df_inter)<-c("Nombre", "Anyo", "Valor")
  df<-rbind(df,df_inter)
}
datos_31255_val2<-df
datos_31255_val2[,1]<-sub("^\\S+\\s+", '', datos_31255_val2[,1])

# *************************

url07<-paste0(url,"/0.7.csv")
GET(url07, write_disk(datos07<- tempfile(fileext = ".csv")))
datos_31256<-read.csv(file = datos07, sep = ";", 
                      encoding = "UTF-8", header = F, 
                      colClasses = "character")
for(i in 1:3){
  for(j in 2:ncol(datos_31256)){
    if(datos_31256[i,j]==""){
      datos_31256[i,j]<-datos_31256[i,j-1]
    }
  }
}
datos_31256<-datos_31256[c(1:4,grep(pattern = "Val\u00E8ncia", x = datos_31256[,1])),]
df<-c()
for(j in 2:ncol(datos_31256)){
  df_inter<-data.frame(paste0(datos_31256[c(5:nrow(datos_31256)),1],". ", datos_31256[1,j], ". ", datos_31256[2,j], ". ", datos_31256[3,j],"."), as.numeric(datos_31256[4,j]), as.numeric(gsub(pattern = ",", replacement = "\\.", x = datos_31256[c(5:nrow(datos_31256)),j])))
  colnames(df_inter)<-c("Nombre", "Anyo", "Valor")
  df<-rbind(df,df_inter)
}
datos_31256_val2<-df
datos_31256_val2[,1]<-sub("^\\S+\\s+", '', datos_31256_val2[,1])

# *************************

url08<-paste0(url,"/0.8.csv")
GET(url08, write_disk(datos08<- tempfile(fileext = ".csv")))
datos_31257<-read.csv(file = datos08, sep = ";", 
                      encoding = "UTF-8", header = F, 
                      colClasses = "character")
for(i in 1:3){
  for(j in 2:ncol(datos_31257)){
    if(datos_31257[i,j]==""){
      datos_31257[i,j]<-datos_31257[i,j-1]
    }
  }
}
datos_31257<-datos_31257[c(1:4,grep(pattern = "Val\u00E8ncia", x = datos_31257[,1])),]
df<-c()
for(j in 2:ncol(datos_31257)){
  df_inter<-data.frame(paste0(datos_31257[c(5:nrow(datos_31257)),1],". ", datos_31257[1,j], ". ", datos_31257[2,j], ". ", datos_31257[3,j],"."), as.numeric(datos_31257[4,j]), as.numeric(gsub(pattern = ",", replacement = "\\.", x = datos_31257[c(5:nrow(datos_31257)),j])))
  colnames(df_inter)<-c("Nombre", "Anyo", "Valor")
  df<-rbind(df,df_inter)
}
datos_31257_val2<-df
datos_31257_val2[,1]<-sub("^\\S+\\s+", '', datos_31257_val2[,1])

# *************************

url09<-paste0(url,"/0.9.csv")
GET(url09, write_disk(datos09<- tempfile(fileext = ".csv")))
datos_31258<-read.csv(file = datos09, sep = ";", 
                      encoding = "UTF-8", header = F, 
                      colClasses = "character")
for(i in 1:1){
  for(j in 2:ncol(datos_31258)){
    if(datos_31258[i,j]==""){
      datos_31258[i,j]<-datos_31258[i,j-1]
    }
  }
}
datos_31258<-datos_31258[c(1:2,grep(pattern = "Val\u00E8ncia", x = datos_31258[,1])),]
df<-c()
for(j in 2:ncol(datos_31258)){
  df_inter<-data.frame(paste0(datos_31258[c(3:nrow(datos_31258)),1],". ", datos_31258[1,j]), as.numeric(datos_31258[2,j]), as.numeric(gsub(pattern = ",", replacement = "\\.", x = gsub(pattern = "\\.", replacement = "", x = datos_31258[c(3:nrow(datos_31258)),j]))))
  colnames(df_inter)<-c("Nombre", "Anyo", "Valor")
  df<-rbind(df,df_inter)
}
datos_31258_val2<-df
datos_31258_val2[,1]<-sub("^\\S+\\s+", '', datos_31258_val2[,1])

# *************************

# Combinamos todos los datos
datos_ciudad<-rbind(datos_31250_val2, datos_31251_val2, datos_31252_val2, datos_31253_val2, datos_31254_val2, datos_31255_val2, datos_31256_val2, datos_31257_val2, datos_31258_val2)

# ***************************************************

# Creamos la relacion de codigos a partir de los indicadores para el total de la ciudad
datosC<-datos_ciudad[grep(pattern = "Val\u00E8ncia\\.", x = datos_ciudad$Nombre),]
# Quitamos la primera palabra (Valencia) con sub("^\\S+\\s+", '') para crear los codigos
CodigosRenta<-data.frame(sub("^\\S+\\s+", '', unique(datosC$Nombre)),1:length(sub("^\\S+\\s+", '', unique(datosC$Nombre))))
colnames(CodigosRenta)<-c("Nombre", "Codigo")
CodigosRenta$Nombre<-as.character(CodigosRenta$Nombre)

# Filtramos los valores de las secciones censales
datosSC<-datos_ciudad[grep(pattern = "secci\u00F3n", x = datos_ciudad$Nombre),]
# Anyadimos dos columnas que codifiquen el indicador y la seccion
datosSC$Indicador<-NA
datosSC$SC<-NA
for(i in 1:nrow(CodigosRenta)){
  # Quitamos las 3 primeras palabras del la primera columna y buscamos
  # el codigo asociado a dicho indicador para la asignacion
  datosSC$Indicador[which(sub("^\\S+\\s+", '',sub("^\\S+\\s+", '',sub("^\\S+\\s+", '',datosSC$Nombre)))==CodigosRenta$Nombre[i])]<-CodigosRenta$Codigo[i]
}
# Extraemos la primera palabra que queda tras eliminar las dos primeras
# de Valencia y seccion para obtener los codigos de la seccion
codigosSC<-data.frame(paste0("secci\u00F3n ", unique(as.character(str_extract(sub("^\\S+\\s+", '',sub("^\\S+\\s+", '',datosSC$Nombre)), '(\\w+)')))),unique(as.numeric(str_extract(sub("^\\S+\\s+", '',sub("^\\S+\\s+", '',datosSC$Nombre)), '(\\w+)'))))
colnames(codigosSC)<-c("Nombre", "Codigo")
codigosSC$Nombre<-as.character(codigosSC$Nombre)
# Asignamos los codigos de seccion
for(i in 1:nrow(codigosSC)){
  datosSC$SC[grep(pattern = codigosSC$Nombre[i], x = datosSC$Nombre)]<-codigosSC$Codigo[i]
}


# Creamos la matriz que relaciona las secciones con los barrios
# Nota: no aparecen la 3028, la 6005, la 12005
codigosSC$BA<-c(rep(1,3), rep(2,4), rep(3,7), rep(4,3), rep(5,3), rep(6,5),
                rep(7,23), rep(8,7), rep(9,12),
                rep(10,5), rep(11,4), rep(12,12), rep(13,17),
                rep(14,6), rep(15,5), rep(16,4), rep(17,3), rep(14,3), rep(17,6),
                rep(18,9), rep(19,9), rep(20,6), rep(21,5), rep(22,6), rep(21,1),
                rep(23,5), rep(24,8), rep(25,7), rep(26,2), rep(24,2),
                rep(27,19), rep(28,4), rep(29,7), rep(30,2),rep(31,2),rep(27,1),rep(31,1),
                rep(32,17), rep(33,3), rep(34,5), rep(35,4), rep(36,2), rep(32,1), rep(33,2), rep(34,3), rep(33,1), rep(32,1), rep(35,2),
                rep(37,10), rep(38,11), rep(39,4), rep(40,8), rep(41,2), rep(40,1), rep(31,1),rep(38,1),
                rep(42,12), rep(43,8), rep(44,8), rep(45,2),rep(46,6), rep(44,1), rep(48,1), rep(47,1), rep(44,1), rep(42,2), rep(43,1), rep(44,5), rep(48,2), rep(47,1),
                rep(49,5), rep(50,19), rep(51,8), rep(52,3), rep(53,5), rep(51,1), rep(52,2), rep(51,1), rep(49,2),
                rep(54,13), rep(55,6), rep(56,9), rep(57,3), rep(58,1), rep(54,2), rep(56,1), rep(58,2), rep(54,1), rep(58,3), rep(57,1),
                rep(59,5), rep(60,8), rep(61,6), rep(62,3), rep(63,1), rep(62,1), rep(60,1), rep(63,1), rep(60,1), rep(59,1),
                rep(64,15), rep(65,3), rep(64,4),
                rep(66,12), rep(67,15), rep(68,1),rep(67,1), rep(66,1), rep(67,1), rep(68,3), rep(66,1), rep(68,2), rep(67,1), rep(68,1),
                rep(69,18), rep(70,4), rep(69,9),
                rep(71,1), rep(72,1), rep(73,1), rep(75,1), rep(76,1),
                rep(77,3), rep(78,1), rep(77,6),
                rep(79,1), rep(80,4), rep(81,2), rep(82,1), rep(83,1), rep(85,4), rep(80,1), rep(84,1))

# Asignamos los codigos de los barrios
datosSC$BA<-NA
for(i in 1:nrow(codigosSC)){
  datosSC$BA[datosSC$SC==codigosSC$Codigo[i]]<-codigosSC$BA[i]
}

# Agregaremos ponderando la media por las poblaciones de las secciones que
# usa el INE, guardadas como el indicador 175. Definimos primero los pesos
pesos2015<-datosSC$Valor[which(datosSC$Anyo==2015 & datosSC$Indicador==175)]
pesos2016<-datosSC$Valor[which(datosSC$Anyo==2016 & datosSC$Indicador==175)]
pesos2017<-datosSC$Valor[which(datosSC$Anyo==2017 & datosSC$Indicador==175)]
# Nos quedamos con los indicadores que tengan datos
datosSC_validos<-datosSC[which(!is.na(datosSC$Valor)),]
indicadores2015<-CodigosRenta[CodigosRenta$Codigo %in% unique(datosSC_validos$Indicador[datosSC_validos$Anyo==2015]),]
indicadores2016<-CodigosRenta[CodigosRenta$Codigo %in% unique(datosSC_validos$Indicador[datosSC_validos$Anyo==2016]),]
indicadores2017<-CodigosRenta[CodigosRenta$Codigo %in% unique(datosSC_validos$Indicador[datosSC_validos$Anyo==2017]),]

# Creamos la base de datos para 2015
any=2015
# Las dos primeras variables identifican el barrio y el anyo
datosBA_renta2015<-cbind(c(1:85),any)
for(i in 1:nrow(indicadores2015)){
  # Filtramos los valores de cada indicador, incluyendo los barrios y los pesos
  ind=indicadores2015$Codigo[i]
  datosSC_filtro<-data.frame(BA=codigosSC$BA, Indicador=datosSC$Valor[which(datosSC$Anyo==any & datosSC$Indicador==ind)], Pesos=paste0("pesos",any))
  # Con la funcion ddply calculamos la media ponderada del indicador para cada barrio
  datosBA<-ddply(datosSC_filtro, .(BA), function(x) data.frame(Indicador=weighted.mean(x$Indicador, x$Pesos)))
  # Los barrios 17.4 y 17.5 (BA=74) tendran el mismo valor que el barrio 17.1 (BA=71)
  datosBA<-rbind(datosBA[1:73,],datosBA[71,],datosBA[74:nrow(datosBA),])
  datosBA[74,1]<-74
  # Guardamos en la base de datos el indicador calculado para los barrios
  datosBA_renta2015<-cbind(datosBA_renta2015,datosBA$Indicador)
}
# Renombramos las columnas
colnames(datosBA_renta2015)<-c("BA", "Anyo", paste0("Indicador", indicadores2015$Codigo))
# Convertimos el objeto de tipo matrix a data.frame
datosBA_renta2015<-as.data.frame(datosBA_renta2015)
# Eliminamos las columnas sin dato
datosBA_renta2015 <- datosBA_renta2015[,colSums(is.na(datosBA_renta2015))<nrow(datosBA_renta2015)]

# Repetimos estos pasos con los otros dos anyos disponibles

# Creamos la base de datos para 2016
any=2016
datosBA_renta2016<-cbind(c(1:85),any)
for(i in 1:nrow(indicadores2016)){
  ind=indicadores2016$Codigo[i]
  datosSC_filtro<-data.frame(BA=codigosSC$BA, Indicador=datosSC$Valor[which(datosSC$Anyo==any & datosSC$Indicador==ind)], Pesos=paste0("pesos",any))
  datosBA<-ddply(datosSC_filtro, .(BA), function(x) data.frame(Indicador=weighted.mean(x$Indicador, x$Pesos)))
  datosBA<-rbind(datosBA[1:73,],datosBA[71,],datosBA[74:nrow(datosBA),])
  datosBA[74,1]<-74
  datosBA_renta2016<-cbind(datosBA_renta2016,datosBA$Indicador)
}
colnames(datosBA_renta2016)<-c("BA", "Anyo", paste0("Indicador", indicadores2016$Codigo))
datosBA_renta2016<-as.data.frame(datosBA_renta2016)
datosBA_renta2016 <- datosBA_renta2016[,colSums(is.na(datosBA_renta2016))<nrow(datosBA_renta2016)]

# Creamos la base de datos para 2017
any=2017
datosBA_renta2017<-cbind(c(1:85),any)
for(i in 1:nrow(indicadores2017)){
  ind=indicadores2017$Codigo[i]
  datosSC_filtro<-data.frame(BA=codigosSC$BA, Indicador=datosSC$Valor[which(datosSC$Anyo==any & datosSC$Indicador==ind)], Pesos=paste0("pesos",any))
  datosBA<-ddply(datosSC_filtro, .(BA), function(x) data.frame(Indicador=weighted.mean(x$Indicador, x$Pesos)))
  datosBA<-rbind(datosBA[1:73,],datosBA[71,],datosBA[74:nrow(datosBA),])
  datosBA[74,1]<-74
  datosBA_renta2017<-cbind(datosBA_renta2017,datosBA$Indicador)
}
colnames(datosBA_renta2017)<-c("BA", "Anyo", paste0("Indicador", indicadores2017$Codigo))
datosBA_renta2017<-as.data.frame(datosBA_renta2017)
datosBA_renta2017 <- datosBA_renta2017[,colSums(is.na(datosBA_renta2017))<nrow(datosBA_renta2017)]

# Comprobamos que hay los mismos indicadores para los 3 anyos
all(colnames(datosBA_renta2016)==colnames(datosBA_renta2015))
all(colnames(datosBA_renta2016)==colnames(datosBA_renta2017))

# Los combinamos
DatosBA_RENTA<-rbind(datosBA_renta2015,datosBA_renta2016,datosBA_renta2017)

# Guardamos la base de datos y la matriz de codigos
save(DatosBA_RENTA, CodigosRenta, file="DatosBarrioRenta.rda")
