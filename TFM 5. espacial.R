# ***** ANALISIS ESPACIAL *****

# ***** Existencia *****

# Quitamos el poligono del puerto
barrios<-barrios[barrios$NOMBRE!="EL PORT",]
# Definimos la estructura espacial de los barrios
vecinos<-poly2nb(barrios, queen=TRUE)
W<-nb2listw(vecinos, style="W",zero.policy = T)
colnames(PUNTUACIONES)<-paste0("TC",1:ncol(PUNTUACIONES))
DataSetCut<-rbind(DataSetCut[1:74,], DataSetCut[74,],
                  DataSetCut[74:nrow(DataSetCut),], 
                  DataSetCut[nrow(DataSetCut),])
PUNTUACIONES<-rbind(PUNTUACIONES[1:74,], PUNTUACIONES[74,],
                    PUNTUACIONES[74:nrow(PUNTUACIONES),], 
                    PUNTUACIONES[nrow(PUNTUACIONES),])
# Usamos el paquete spdep para ver si existe correlacion espacial en los factores
# Global Moran's I
Global_Moran_I<-apply(PUNTUACIONES, 2, 
                      function(x) moran(x,W, n=length(W$neighbours), 
                                        S0=Szero(W), zero.policy = T, NAOK = T)$I)
set.seed(1234) 
MonteCarlo_Moran<-apply(PUNTUACIONES, 2, 
                        function(x) moran.mc(x = x,listw = W, nsim = 9999, 
                                             zero.policy = T, 
                                             na.action = na.exclude)$p.value)

# Geary C
Global_Geary_C<-apply(PUNTUACIONES, 2, 
                      function(x) geary(x = x,listw = W, n = length(W$neighbours), 
                                        n1 = length(W$neighbours)-1, 
                                        S0 = Szero(W), zero.policy = T)$C)
set.seed(1234)
MonteCarlo_Geary<-apply(PUNTUACIONES, 2, 
                        function(x) geary.mc(x = x,listw = W, nsim = 9999, 
                                             zero.policy = T)$p.value)

# ***** Localizacion *****

# Local Moran's I:
# Programamos un proceso de Monte-Carlo para el calculo de los pseudo p-valores
Pseudo.P.Valores_simulaciones<-matrix(data = NA, nrow = nrow(PUNTUACIONES), 
                                      ncol = ncol(PUNTUACIONES))
for(j in 1:ncol(Pseudo.P.Valores_simulaciones)){
  DataSet_mc<-PUNTUACIONES[,j]
  nsim=9999
  simulaciones<-matrix(0, ncol=nsim, nrow=length(W$neighbours))
  Local_Moran_I<-localmoran(x = DataSet_mc,listw = W, 
                            zero.policy = T, na.action = na.exclude)[,"Ii"]
  set.seed(1234) 
  for(i in 1:nsim){
    permutaciones<-sample(1:length(DataSet_mc))
    #Cambiamos aletoriamente los valores de las componentes principales de sitio
    DataSet_mc<-DataSet_mc[permutaciones] 
    # Calculamos el nuevo coeficiente de Moran
    simulaciones[,i]<-localmoran(x = DataSet_mc,listw = W, 
                                 zero.policy = T, na.action = na.exclude)[,"Ii"] 
  }
  for(i in 1:length(DataSet_mc)){
    # Computamos el pseudo p-valor como el % de superaciones 
    # del valor del estadistico original
    Pseudo.P.Valores_simulaciones[i,j]<- 
      (1+sum(simulaciones[,i]>=Local_Moran_I[i], na.rm = T))/(nsim+1) 
  }
}
# Aseveramos que el valor original del coeficiente de Moran no es fruto del azar
sum(Pseudo.P.Valores_simulaciones<0.01)/(ncol(PUNTUACIONES)*nrow(PUNTUACIONES))*100 
# La funcion localmoran devuelve un p-valor
P.Local_Moran_I<-apply(PUNTUACIONES, 2, 
                       function(x) localmoran(x = x,listw = W, 
                                              zero.policy = T, na.action = na.exclude)[,"Pr(z > 0)"])
rownames(P.Local_Moran_I)<-NULL
P.Local_Moran_I[is.na(P.Local_Moran_I)]<-1
# Pero utilizamos el pseudo p-valor resultante del metodo Monte Carlo empleado
P.Local_Moran_I<-Pseudo.P.Valores_simulaciones
rownames(P.Local_Moran_I)<-NULL
colnames(P.Local_Moran_I)<-paste0("TC",1:ncol(PUNTUACIONES))

#*************************

# LISA Local Moran's I 
PUNTUACIONES_LISA<-PUNTUACIONES
PUNTUACIONES_LISA$sTC1<-scale(PUNTUACIONES_LISA$TC1)
PUNTUACIONES_LISA$lag_sTC1 <- lag.listw(W, PUNTUACIONES_LISA$sTC1, 
                                        zero.policy = T, NAOK = T)
# Identificamos el cuadrante en el grafico de Moran de cada observacion
PUNTUACIONES_LISA$quad_sig <- NA
PUNTUACIONES_LISA[(PUNTUACIONES_LISA$sTC1 > 0 & PUNTUACIONES_LISA$lag_sTC1 > 0) & 
                    (P.Local_Moran_I[,"TC1"] <= 0.01), "quad_sig"] <- 1
PUNTUACIONES_LISA[(PUNTUACIONES_LISA$sTC1 <= 0 & PUNTUACIONES_LISA$lag_sTC1 <= 0) & 
                    (P.Local_Moran_I[,"TC1"] <= 0.01), "quad_sig"] <- 2
PUNTUACIONES_LISA[(PUNTUACIONES_LISA$sTC1 > 0 & PUNTUACIONES_LISA$lag_sTC1 <= 0) & 
                    (P.Local_Moran_I[,"TC1"] <= 0.01), "quad_sig"] <- 3
PUNTUACIONES_LISA[(PUNTUACIONES_LISA$sTC1 <= 0 & PUNTUACIONES_LISA$lag_sTC1 > 0) & 
                    (P.Local_Moran_I[,"TC1"] <= 0.01), "quad_sig"] <- 4
PUNTUACIONES_LISA[is.na(PUNTUACIONES_LISA$quad_sig), "quad_sig"] <- 5
clases<-c("Alto-Alto", "Bajo-Bajo", "Alto-Bajo", "Bajo-Alto", "No signif.")
colors <- c("red", "blue", "lightpink", "skyblue2", "white")
table(PUNTUACIONES_LISA$quad_sig)
sum(P.Local_Moran_I[,"TC1"]<0.01)
barrios$quad_sig<-clases[PUNTUACIONES_LISA$quad_sig]
# Dibujamos el mapa LISA para el primer factor
qtm1<-qtm(shp = barrios, fill = "quad_sig", fill.title="LISA", 
          fill.palette=c('Alto-Alto'='red', 'Alto-Bajo'='lightpink', 
                         'Bajo-Alto'='skyblue2', 'Bajo-Bajo'='blue', 
                         'No signif.'='white'))+
  tm_layout(legend.width = 0.7, main.title = "TC1", main.title.size = 0.8)
png("LISA TC1.png", width = 9, height = 5, units = 'in', res = 300)
qtm1
dev.off()

PUNTUACIONES_LISA$sTC2<-scale(PUNTUACIONES_LISA$TC2)
PUNTUACIONES_LISA$lag_sTC2 <- lag.listw(W, PUNTUACIONES_LISA$sTC2, 
                                        zero.policy = T, NAOK = T)
# Identificamos el cuadrante en el grafico de Moran de cada observacion
PUNTUACIONES_LISA$quad_sig <- NA
PUNTUACIONES_LISA[(PUNTUACIONES_LISA$sTC2 > 0 & PUNTUACIONES_LISA$lag_sTC2 > 0) & 
                    (P.Local_Moran_I[,"TC2"] <= 0.01), "quad_sig"] <- 1
PUNTUACIONES_LISA[(PUNTUACIONES_LISA$sTC2 <= 0 & PUNTUACIONES_LISA$lag_sTC2 <= 0) & 
                    (P.Local_Moran_I[,"TC2"] <= 0.01), "quad_sig"] <- 2
PUNTUACIONES_LISA[(PUNTUACIONES_LISA$sTC2 > 0 & PUNTUACIONES_LISA$lag_sTC2 <= 0) & 
                    (P.Local_Moran_I[,"TC2"] <= 0.01), "quad_sig"] <- 3
PUNTUACIONES_LISA[(PUNTUACIONES_LISA$sTC2 <= 0 & PUNTUACIONES_LISA$lag_sTC2 > 0) & 
                    (P.Local_Moran_I[,"TC2"] <= 0.01), "quad_sig"] <- 4
PUNTUACIONES_LISA[is.na(PUNTUACIONES_LISA$quad_sig), "quad_sig"] <- 5
clases<-c("Alto-Alto", "Bajo-Bajo", "Alto-Bajo", "Bajo-Alto", "No signif.")
colors <- c("red", "blue", "lightpink", "skyblue2", "white")
table(PUNTUACIONES_LISA$quad_sig)
sum(P.Local_Moran_I[,"TC2"]<0.01)
barrios$quad_sig<-clases[PUNTUACIONES_LISA$quad_sig]
# Dibujamos el mapa LISA para el segundo factor
qtm2<-qtm(shp = barrios, fill = "quad_sig", fill.title="LISA", 
          fill.palette=c('Alto-Alto'='red', 'Alto-Bajo'='lightpink', 
                         'Bajo-Alto'='skyblue2', 'Bajo-Bajo'='blue', 
                         'No signif.'='white'))+
  tm_layout(legend.width = 0.7, main.title = "TC2", main.title.size = 0.8)
png("LISA TC2.png", width = 9, height = 5, units = 'in', res = 300)
qtm2
dev.off()

PUNTUACIONES_LISA$sTC3<-scale(PUNTUACIONES_LISA$TC3)
PUNTUACIONES_LISA$lag_sTC3 <- lag.listw(W, PUNTUACIONES_LISA$sTC3, 
                                        zero.policy = T, NAOK = T)
# Identificamos el cuadrante en el grafico de Moran de cada observacion
PUNTUACIONES_LISA$quad_sig <- NA
PUNTUACIONES_LISA[(PUNTUACIONES_LISA$sTC3 > 0 & PUNTUACIONES_LISA$lag_sTC3 > 0) & 
                    (P.Local_Moran_I[,"TC3"] <= 0.01), "quad_sig"] <- 1
PUNTUACIONES_LISA[(PUNTUACIONES_LISA$sTC3 <= 0 & PUNTUACIONES_LISA$lag_sTC3 <= 0) & 
                    (P.Local_Moran_I[,"TC3"] <= 0.01), "quad_sig"] <- 2
PUNTUACIONES_LISA[(PUNTUACIONES_LISA$sTC3 > 0 & PUNTUACIONES_LISA$lag_sTC3 <= 0) & 
                    (P.Local_Moran_I[,"TC3"] <= 0.01), "quad_sig"] <- 3
PUNTUACIONES_LISA[(PUNTUACIONES_LISA$sTC3 <= 0 & PUNTUACIONES_LISA$lag_sTC3 > 0) & 
                    (P.Local_Moran_I[,"TC3"] <= 0.01), "quad_sig"] <- 4
PUNTUACIONES_LISA[is.na(PUNTUACIONES_LISA$quad_sig), "quad_sig"] <- 5
clases<-c("Alto-Alto", "Bajo-Bajo", "Alto-Bajo", "Bajo-Alto", "No signif.")
colors <- c("red", "blue", "lightpink", "skyblue2", "white")
table(PUNTUACIONES_LISA$quad_sig)
sum(P.Local_Moran_I[,"TC3"]<0.01)
barrios$quad_sig<-clases[PUNTUACIONES_LISA$quad_sig]
# Dibujamos el mapa LISA para el tercer factor
qtm3<-qtm(shp = barrios, fill = "quad_sig", fill.title="LISA", 
          fill.palette=c('Alto-Alto'='red', 'Alto-Bajo'='lightpink', 
                         'Bajo-Alto'='skyblue2', 'Bajo-Bajo'='blue', 
                         'No signif.'='white'))+
  tm_layout(legend.width = 0.7, main.title = "TC3", main.title.size = 0.8)
png("LISA TC3.png", width = 9, height = 5, units = 'in', res = 300)
qtm3
dev.off()
# Juntamos los tres mapas en uno 
png("LISA Local Moran's I.png", width = 9, height = 5, units = 'in', res = 300)
tmap_arrange(qtm1, qtm2, qtm3)
dev.off()

#*************************

# Local Gi (Getis-Ord Gi statistic)
# Calculamos el estadistico usando el paquete spdep
Local_G<-apply(PUNTUACIONES, 2, function(x) localG(x,W, zero.policy = T))
barrios$G_Vejez<-Local_G[,"TC1"]
barrios$G_Migracion<-Local_G[,"TC2"]
barrios$G_Renta<-Local_G[,"TC3"]

# LISA Getis-Ord Gi* statistic
# Dibujamos los mapas usando el paquete tm
G_mapa_vejez<-tm_shape(barrios) + 
  tm_fill("G_Vejez",title = "Local Gi*",
          palette = "RdBu", style="pretty") +
  tm_borders(alpha=.4)+tm_layout(legend.width = 0.7, main.title = "TC1", 
                                 main.title.size = 0.8)

G_mapa_migracion<-tm_shape(barrios) + 
  tm_fill("G_Migracion",title = paste0("Local Gi*"),
          palette = "RdBu", style="pretty") +
  tm_borders(alpha=.4)+tm_layout(legend.width = 0.7, main.title = "TC2", 
                                 main.title.size = 0.8)

G_mapa_renta<-tm_shape(barrios) + 
  tm_fill("G_Renta",title = paste0("Local Gi*"),
          palette = "RdBu", style="pretty") +
  tm_borders(alpha=.4)+tm_layout(legend.width = 0.7, main.title = "TC3", 
                                 main.title.size = 0.8)

png("LISA Vejez.png", width = 9, height = 5, units = 'in', res = 300)
G_mapa_vejez
dev.off()

png("LISA Migracion.png", width = 9, height = 5, units = 'in', res = 300)
G_mapa_migracion
dev.off()

png("LISA Renta.png", width = 9, height = 5, units = 'in', res = 300)
G_mapa_renta
dev.off()

# Juntamos los tres mapas en uno
png("LISA Getis-Ord Gi.png", width = 9, height = 5, units = 'in', res = 300)
tmap_arrange(G_mapa_vejez, G_mapa_migracion, G_mapa_renta)
dev.off()
