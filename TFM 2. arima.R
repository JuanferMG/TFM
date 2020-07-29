# ***** DATOS *****

# Descargamos los datos de los barrios de mi cuenta de GitHub
url<-"https://raw.githubusercontent.com/JuanferMG/TFM/master"
# Los datos de demografia de 2004 a 2019
url1<-paste0(url,"/DatosBarrioDemografia.xlsx")
GET(url1, write_disk(datos <- tempfile(fileext = ".xlsx")))
DatosBA <- as.data.frame(read_excel(datos))
# Los datos de renta de 2015 a 2017
url2<-paste0(url,"/DatosBarrioRenta.rda")
GET(url2, write_disk(datos2 <- tempfile(fileext = ".rda")))
load(datos2)
# El shape con los poligonos para dibujar los mapas
url3<-paste0(url,"/barrios.rda")
GET(url3, write_disk(barrios <- tempfile(fileext = ".rda")))
load(barrios)


# ***** PROYECCIONES MEDIANTE MODELOS ARIMA *****

# Definimos una funcion que usa modelos ARIMA para proyectar cada indicador a 2021
sibila<-function(variable){
  # Los valores de 2019 del indicador serviran para medir la bondad del ajuste
  valor.observado<-DatosBA[which(DatosBA$Anyo==2019),variable]
  # Los valores de 2004 a 2018 del indicador serviran como predictores
  predictores<-c()
  for(i in 2004:2018){
    predictores<-rbind(predictores,DatosBA[which(DatosBA$Anyo==i),variable])
  }
  barrio=unique(DatosBA$BA)
  colnames(predictores)<-paste0("BA",barrio)
  # Inicializamos las variables usadas en la prediccion 
  evolucion.st<-c()
  auto<-c()
  modelo<-c()
  modelo.res<-c()
  shapiro<-c()
  pvalor<-c()
  dependencia.res<-c()
  nivel.acf<-c()
  modelo.prediccion<-c()
  valor.estimado<-c()
  # Para cada barrio se usara el modelo ARIMA que mejor se ajuste a la serie
  for(k in 1:length(barrio)){
    # Obtenemos la serie temporal del barrio
    evolucion<-data.frame(fecha=2004:2018, valor=predictores[,paste0("BA",barrio[k])])
    evolucion.st[[k]] <- ts(evolucion$valor, start = 2004, deltat = 1)
    # Calibramos las componentes p,d,q que mejor se ajustan a la serie
    auto[[k]]<-auto.arima(y = evolucion.st[[k]])
    # Construimos el modelo ARIMA asociado a la serie
    modelo[[k]] <- arima(evolucion.st[[k]], order = arimaorder(auto[[k]]))
    # Calculamos los residuos
    modelo.res[[k]] <- residuals(modelo[[k]]) 
    # Comprobamos si los residuos estan normalmente distribuidos
    shapiro[[k]]<-shapiro.test(modelo.res[[k]]) 
    pvalor[[k]]<-shapiro[[k]]$p.value
    # Calculamos la funcion de autocorrelacion sobre los residuos
    dependencia.res[[k]]<-acf(modelo.res[[k]], plot = F, na.action = na.pass)$acf 
    # Predecimos con el modelo los valores a corto plazo de los proximos 3 anyos
    modelo.prediccion[[k]] <- forecast(object = modelo[[k]], h = 3)
    valor.estimado<-cbind(valor.estimado,modelo.prediccion[[k]]$mean)
  }
  colnames(valor.estimado)<-NULL
  # Comprobamos que los coeficientes acf no son significativos y
  # que por tanto los residuos son independientes entre si
  nivel.acf<- qnorm((1 + 0.95)/2)/sqrt(15)
  signficativos.acf<-lapply(dependencia.res, 
                            function(x) sum(abs(x) >= nivel.acf) - 1 )
  residuos.acf<-table(unlist(signficativos.acf))
  # Calculamos la raiz del error cuadratico medio
  RMSE<-rmse(valor.observado,valor.estimado[1,])
  # La funcion devuelve el valor de 2019, los valores proyectados, el error,
  # los p-valores del test de shapiro y el numero de coeficientes acf significativos
  lista<-list(Valor2019=valor.observado, 
              Prediccion=valor.estimado,
              Error2019=RMSE, 
              P.Valores=pvalor, 
              ACF=residuos.acf)
  return(lista)
}

# Proyectamos las series temporales de los 30 indicadores calculados
Ind01<-sibila("Crecimiento_Vegetativo")
Ind02<-sibila("Saldo_Migratorio")
Ind03<-sibila("Saldo_Movimientos_Intraurbanos")
Ind04<-sibila("Tasa_Natalidad")
Ind05<-sibila("Tasa_General_Fecundidad")
Ind06<-sibila("Tasa_MortalidadT")
Ind07<-sibila("Tasa_Inmigracion")
Ind08<-sibila("Tasa_Emigracion")
Ind09<-sibila("Tasa_Llegadas_Cambio_Domicilio")
Ind10<-sibila("Tasa_Salidas_Cambio_Domicilio")
Ind11<-sibila("Relacion_Maculinidad_Nacimiento")
Ind12<-sibila("Indice_Sundbarg")
Ind13<-sibila("Indice_Friz")
Ind14<-sibila("Indice_Burgdofer")
Ind15<-sibila("Indice_Generacional_Ancianos")
Ind16<-sibila("Indice_Envejecimiento")
Ind17<-sibila("Indice_Sobreenvejecimiento")
Ind18<-sibila("Indice_Demografico_Dependencia")
Ind19<-sibila("Indice_Estructura_Poblacion_Activa")
Ind20<-sibila("Indice_Reemplazamiento_Poblacion_Activa")
Ind21<-sibila("Indice_Carga_Preescolar")
Ind22<-sibila("Razon_Progresividad_Demografica")
Ind23<-sibila("Relacion_Maculinidad")
Ind24<-sibila("Porcentaje_PobTExtr")
Ind25<-sibila("Porcentaje_PobT65_mas")
Ind26<-sibila("Porcentaje_PobT0_15")
Ind27<-sibila("Porcentaje_PobT_Nacidos_Valencia")
Ind28<-sibila("Porcentaje_Hojas_Fam_Solo80_mas")
Ind29<-sibila("Porcentaje_Hojas_Fam_Menores0")
Ind30<-sibila("Media_Personas_Hojas_Fam")


# Guardamos los valores en un archivo RDA
save(Ind01, Ind02, Ind03, Ind04, Ind05, Ind06, Ind07, Ind08, Ind09, Ind10,
     Ind11, Ind12, Ind13, Ind14, Ind15, Ind16, Ind17, Ind18, Ind19, Ind20,
     Ind21, Ind22, Ind23, Ind24, Ind25, Ind26, Ind27, Ind28, Ind29, Ind30,
     file = "Predicciones.RDA")

load("Predicciones.RDA")

# Creamos el data.frame con los indicadores con los que vamos a trabajar
f=3 #La tercera fila contiene las estimaciones para el anyo 2021
DataSet<-data.frame(Ind01$Prediccion[f,], Ind02$Prediccion[f,], Ind03$Prediccion[f,], 
                    Ind04$Prediccion[f,], Ind05$Prediccion[f,], Ind06$Prediccion[f,],
                    Ind07$Prediccion[f,], Ind08$Prediccion[f,], Ind09$Prediccion[f,],
                    Ind10$Prediccion[f,], Ind11$Prediccion[f,], Ind12$Prediccion[f,],
                    Ind13$Prediccion[f,], Ind14$Prediccion[f,], Ind15$Prediccion[f,],
                    Ind16$Prediccion[f,], Ind17$Prediccion[f,], Ind18$Prediccion[f,],
                    Ind19$Prediccion[f,], Ind20$Prediccion[f,], Ind21$Prediccion[f,],
                    Ind22$Prediccion[f,], Ind23$Prediccion[f,], Ind24$Prediccion[f,],
                    Ind25$Prediccion[f,], Ind26$Prediccion[f,], Ind27$Prediccion[f,],
                    Ind28$Prediccion[f,], Ind29$Prediccion[f,], Ind30$Prediccion[f,])
# Definimos las variables de renta elegidas
# Obtendremos la media ponderada de cada variable
Pond1<-(DatosBA$PobT[which(DatosBA$Anyo==2016)])-
  (DatosBA$PobT0_15[which(DatosBA$Anyo==2016)])
Pond2<-(DatosBA$PobT[which(DatosBA$Anyo==2017)])-(
  DatosBA$PobT0_15[which(DatosBA$Anyo==2017)])
Pond3<-(DatosBA$PobT[which(DatosBA$Anyo==2018)])-
  (DatosBA$PobT0_15[which(DatosBA$Anyo==2018)])
# Indicador 31: Renta media por persona
Ind31<-(Pond1*DatosBA_RENTA$Indicador1[which(DatosBA_RENTA$Anyo==2015)]+
        Pond2*DatosBA_RENTA$Indicador1[which(DatosBA_RENTA$Anyo==2016)]+
        Pond3*DatosBA_RENTA$Indicador1[which(DatosBA_RENTA$Anyo==2017)])
Ind31<-Ind31/(Pond1+Pond2+Pond3)
# Indicador 32: Renta media por hogar
Ind32<-(Pond1*DatosBA_RENTA$Indicador2[which(DatosBA_RENTA$Anyo==2015)]+
        Pond2*DatosBA_RENTA$Indicador2[which(DatosBA_RENTA$Anyo==2016)]+
        Pond3*DatosBA_RENTA$Indicador2[which(DatosBA_RENTA$Anyo==2017)])
Ind32<-Ind32/(Pond1+Pond2+Pond3)
# Indicador 33: Poblacion con ingresos por unidad de consumo por  
# debajo del 60 % de la mediana
Ind33<-(Pond1*DatosBA_RENTA$Indicador64[which(DatosBA_RENTA$Anyo==2015)]+
        Pond2*DatosBA_RENTA$Indicador64[which(DatosBA_RENTA$Anyo==2016)]+
        Pond3*DatosBA_RENTA$Indicador64[which(DatosBA_RENTA$Anyo==2017)])
Ind33<-Ind33/(Pond1+Pond2+Pond3)
# Incorporamos al conjunto de indicadores demograficos las variables de renta
DataSet<-cbind(DataSet, Ind31, Ind32, Ind33)
# Normalizamos el conjunto de datos tipifcando con la funcion scale()
DataSet<-as.data.frame(apply(DataSet, 2, scale)) 
colnames(DataSet)<-c(paste0("Ind0",1:9),paste0("Ind",10:33))