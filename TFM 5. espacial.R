# ***** ANALISIS ESPACIAL *****

# ***** Existencia *****

# Quitamos el poligono del puerto
barrios_new<-barrios[barrios$NOMBRE!="EL PORT",]
# Definimos la estructura espacial de los barrios
vecinos<-poly2nb(barrios_new, queen=TRUE)
W<-nb2listw(vecinos, style="W",zero.policy = T)
colnames(PUNTUACIONES)<-paste0("TC",1:ncol(PUNTUACIONES))
DataSetCut<-rbind(DataSetCut[1:74,], DataSetCut[74,],
                  DataSetCut[74:nrow(DataSetCut),], 
                  DataSetCut[nrow(DataSetCut),])
PUNTUACIONES_new<-rbind(PUNTUACIONES[1:74,], PUNTUACIONES[74,],
                    PUNTUACIONES[74:nrow(PUNTUACIONES),], 
                    PUNTUACIONES[nrow(PUNTUACIONES),])
# Usamos el paquete spdep para ver si existe correlacion espacial en los factores
# Global Moran's I
Global_Moran_I<-apply(PUNTUACIONES_new, 2, 
                      function(x) moran(x,W, n=length(W$neighbours), 
                                        S0=Szero(W), zero.policy = T, NAOK = T)$I)
set.seed(1234) 
MonteCarlo_Moran<-apply(PUNTUACIONES_new, 2, 
                        function(x) moran.mc(x = x,listw = W, nsim = 9999, 
                                             zero.policy = T, 
                                             na.action = na.exclude)$p.value)

# Geary C
Global_Geary_C<-apply(PUNTUACIONES_new, 2, 
                      function(x) geary(x = x,listw = W, n = length(W$neighbours), 
                                        n1 = length(W$neighbours)-1, 
                                        S0 = Szero(W), zero.policy = T)$C)
set.seed(1234)
MonteCarlo_Geary<-apply(PUNTUACIONES_new, 2, 
                        function(x) geary.mc(x = x,listw = W, nsim = 9999, 
                                             zero.policy = T)$p.value)

# ***** Localizacion *****

# Local Moran's I:
# La funcion localmoran calcula el estadistico local y ademas devuelve
# unos p-valores con los que evaluar la significacion estadistica
P.Local_Moran_I<-apply(PUNTUACIONES_new, 2, function(x) 
  localmoran(x = x,listw = W, zero.policy = T, na.action = na.exclude)[,"Pr(z > 0)"])
rownames(P.Local_Moran_I)<-NULL
# Asignamos un p-valor = 1 a los barrios del distrito 17 no fronterizos
P.Local_Moran_I[is.na(P.Local_Moran_I)]<-1

# Consideramos tres umbrales

# ***********************

# P-value Treshold 0.01

pv_base<-0.01

# ***********************

# False Discovery Rate

FDR<-(0.01/85)*1:85
pv_FDR1<-FDR[max(which(sort(P.Local_Moran_I[,1])[1:85]<FDR))]
pv_FDR2<-FDR[max(which(sort(P.Local_Moran_I[,2])[1:85]<FDR))]
pv_FDR3<-FDR[max(which(sort(P.Local_Moran_I[,3])[1:85]<FDR))]

#*************************

# Bonferroni Bound

pv_bonferroni<-0.01/85

#*************************

umbralTC1<-c(pv_base,pv_FDR1,pv_bonferroni)
umbralTC2<-c(pv_base,pv_FDR2,pv_bonferroni)
umbralTC3<-c(pv_base,pv_FDR3,pv_bonferroni)
nombres<-c("0,01", "FDR", "Bonferroni")

# Dibujamos los mapas LISA Local Moran's I 
for(u in 1:3){
  
  PUNTUACIONES_LISA<-PUNTUACIONES_new
  PUNTUACIONES_LISA$sTC1<-scale(PUNTUACIONES_LISA$TC1)
  PUNTUACIONES_LISA$lag_sTC1 <- lag.listw(W, PUNTUACIONES_LISA$sTC1, 
                                          zero.policy = T, NAOK = T)
  # Identificamos el cuadrante en el grafico de Moran de cada observacion
  clases<-c("Alto-Alto", "Bajo-Bajo", "Alto-Bajo", "Bajo-Alto", "No signif.")
  colors <- c("red", "blue", "lightpink", "skyblue2", "white")
  paleta_lisa<-c('Alto-Alto'='red', 'Bajo-Bajo'='blue', 
                 'Alto-Bajo'='lightpink', 'Bajo-Alto'='skyblue2', 
                 'No signif.'='white')
  PUNTUACIONES_LISA$quad_sig <- NA
  PUNTUACIONES_LISA[(PUNTUACIONES_LISA$sTC1 > 0 & PUNTUACIONES_LISA$lag_sTC1 > 0) & 
                      (P.Local_Moran_I[,"TC1"] <= umbralTC1[u]), "quad_sig"] <- 1
  PUNTUACIONES_LISA[(PUNTUACIONES_LISA$sTC1 <= 0 & PUNTUACIONES_LISA$lag_sTC1 <= 0) & 
                      (P.Local_Moran_I[,"TC1"] <= umbralTC1[u]), "quad_sig"] <- 2
  PUNTUACIONES_LISA[(PUNTUACIONES_LISA$sTC1 > 0 & PUNTUACIONES_LISA$lag_sTC1 <= 0) & 
                      (P.Local_Moran_I[,"TC1"] <= umbralTC1[u]), "quad_sig"] <- 3
  PUNTUACIONES_LISA[(PUNTUACIONES_LISA$sTC1 <= 0 & PUNTUACIONES_LISA$lag_sTC1 > 0) & 
                      (P.Local_Moran_I[,"TC1"] <= umbralTC1[u]), "quad_sig"] <- 4
  PUNTUACIONES_LISA[is.na(PUNTUACIONES_LISA$quad_sig), "quad_sig"] <- 5
  as.numeric(names(table(PUNTUACIONES_LISA$quad_sig)))
  sum(P.Local_Moran_I[,"TC1"]<0.01)
  barrios_new$quad_sig<-clases[PUNTUACIONES_LISA$quad_sig]
  # Definimos los colores
  colores_lisa<-paleta_lisa[as.numeric(names(table(PUNTUACIONES_LISA$quad_sig)))]
  
  # Dibujamos el mapa LISA para el primer factor
  qtm1<-qtm(shp = barrios_new, fill = "quad_sig", fill.title="LISA",
            fill.palette=colores_lisa)+
    tm_layout(legend.width = 0.7, main.title = "TC1", main.title.size = 0.8)
  png(paste0("LISA TC1 ",nombres[u],".png"), width = 9, height = 5, 
      units = 'in', res = 300)
  qtm1
  dev.off()
  
  PUNTUACIONES_LISA$sTC2<-scale(PUNTUACIONES_LISA$TC2)
  PUNTUACIONES_LISA$lag_sTC2 <- lag.listw(W, PUNTUACIONES_LISA$sTC2, 
                                          zero.policy = T, NAOK = T)
  # Identificamos el cuadrante en el grafico de Moran de cada observacion
  PUNTUACIONES_LISA$quad_sig <- NA
  PUNTUACIONES_LISA[(PUNTUACIONES_LISA$sTC2 > 0 & PUNTUACIONES_LISA$lag_sTC2 > 0) & 
                      (P.Local_Moran_I[,"TC2"] <= umbralTC2[u]), "quad_sig"] <- 1
  PUNTUACIONES_LISA[(PUNTUACIONES_LISA$sTC2 <= 0 & PUNTUACIONES_LISA$lag_sTC2 <= 0) & 
                      (P.Local_Moran_I[,"TC2"] <= umbralTC2[u]), "quad_sig"] <- 2
  PUNTUACIONES_LISA[(PUNTUACIONES_LISA$sTC2 > 0 & PUNTUACIONES_LISA$lag_sTC2 <= 0) & 
                      (P.Local_Moran_I[,"TC2"] <= umbralTC2[u]), "quad_sig"] <- 3
  PUNTUACIONES_LISA[(PUNTUACIONES_LISA$sTC2 <= 0 & PUNTUACIONES_LISA$lag_sTC2 > 0) & 
                      (P.Local_Moran_I[,"TC2"] <= umbralTC2[u]), "quad_sig"] <- 4
  PUNTUACIONES_LISA[is.na(PUNTUACIONES_LISA$quad_sig), "quad_sig"] <- 5
  table(PUNTUACIONES_LISA$quad_sig)
  sum(P.Local_Moran_I[,"TC2"]<0.01)
  barrios_new$quad_sig<-clases[PUNTUACIONES_LISA$quad_sig]
  # Definimos los colores
  colores_lisa<-paleta_lisa[as.numeric(names(table(PUNTUACIONES_LISA$quad_sig)))]
  
  # Dibujamos el mapa LISA para el segundo factor
  qtm2<-qtm(shp = barrios_new, fill = "quad_sig", fill.title="LISA",
            fill.palette=colores_lisa)+
    tm_layout(legend.width = 0.7, main.title = "TC2", main.title.size = 0.8)
  png(paste0("LISA TC2 ",nombres[u],".png"), width = 9, height = 5, 
      units = 'in', res = 300)
  qtm2
  dev.off()
  
  PUNTUACIONES_LISA$sTC3<-scale(PUNTUACIONES_LISA$TC3)
  PUNTUACIONES_LISA$lag_sTC3 <- lag.listw(W, PUNTUACIONES_LISA$sTC3, 
                                          zero.policy = T, NAOK = T)
  # Identificamos el cuadrante en el grafico de Moran de cada observacion
  PUNTUACIONES_LISA$quad_sig <- NA
  PUNTUACIONES_LISA[(PUNTUACIONES_LISA$sTC3 > 0 & PUNTUACIONES_LISA$lag_sTC3 > 0) & 
                      (P.Local_Moran_I[,"TC3"] <= umbralTC3[u]), "quad_sig"] <- 1
  PUNTUACIONES_LISA[(PUNTUACIONES_LISA$sTC3 <= 0 & PUNTUACIONES_LISA$lag_sTC3 <= 0) & 
                      (P.Local_Moran_I[,"TC3"] <= umbralTC3[u]), "quad_sig"] <- 2
  PUNTUACIONES_LISA[(PUNTUACIONES_LISA$sTC3 > 0 & PUNTUACIONES_LISA$lag_sTC3 <= 0) & 
                      (P.Local_Moran_I[,"TC3"] <= umbralTC3[u]), "quad_sig"] <- 3
  PUNTUACIONES_LISA[(PUNTUACIONES_LISA$sTC3 <= 0 & PUNTUACIONES_LISA$lag_sTC3 > 0) & 
                      (P.Local_Moran_I[,"TC3"] <= umbralTC3[u]), "quad_sig"] <- 4
  PUNTUACIONES_LISA[is.na(PUNTUACIONES_LISA$quad_sig), "quad_sig"] <- 5
  table(PUNTUACIONES_LISA$quad_sig)
  barrios_new$quad_sig<-clases[PUNTUACIONES_LISA$quad_sig]
  # Definimos los colores
  colores_lisa<-paleta_lisa[as.numeric(names(table(PUNTUACIONES_LISA$quad_sig)))]
  
  # Dibujamos el mapa LISA para el tercer factor
  qtm3<-qtm(shp = barrios_new, fill = "quad_sig", fill.title="LISA",
            fill.palette=colores_lisa)+
    tm_layout(legend.width = 0.7, main.title = "TC3", main.title.size = 0.8)
  png(paste("LISA TC3 ",nombres[u],".png"), width = 9, height = 5, 
      units = 'in', res = 300)
  qtm3
  dev.off()
  
  # Juntamos los tres mapas en uno 
  png(paste0("LISA Local Moran's I ",nombres[u],".png"), width = 9, height = 5, 
      units = 'in', res = 300)
  tmap_arrange(qtm1, qtm2, qtm3)
  dev.off()
  
}


#*************************

# Local Gi (Getis-Ord Gi statistic)
# Calculamos el estadistico usando el paquete spdep
Local_G<-apply(PUNTUACIONES_new, 2, function(x) localG(x,W, zero.policy = T))
barrios_new$G_Vejez<-Local_G[,"TC1"]
barrios_new$G_Migracion<-Local_G[,"TC2"]
barrios_new$G_Renta<-Local_G[,"TC3"]

# LISA Getis-Ord Gi* statistic
# Dibujamos los mapas usando el paquete tm
G_mapa_vejez<-tm_shape(barrios_new) + 
  tm_fill("G_Vejez",title = "Local Gi*",
          palette = rev(brewer.pal(n = 11, name = "RdBu")), style="pretty") +
  tm_borders(alpha=.4)+tm_layout(legend.width = 0.7, main.title = "TC1", 
                                 main.title.size = 0.8)

G_mapa_migracion<-tm_shape(barrios_new) + 
  tm_fill("G_Migracion",title = paste0("Local Gi*"),
          palette = rev(brewer.pal(n = 11, name = "RdBu")), style="pretty") +
  tm_borders(alpha=.4)+tm_layout(legend.width = 0.7, main.title = "TC2", 
                                 main.title.size = 0.8)

G_mapa_renta<-tm_shape(barrios_new) + 
  tm_fill("G_Renta",title = paste0("Local Gi*"),
          palette = rev(brewer.pal(n = 11, name = "RdBu")), style="pretty") +
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
