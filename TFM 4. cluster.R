# ***** ANALISIS CLUSTER *****

# Cluster tipo particional con K medias
# Con el paquete NbClust elegimos el numero optimo de clusters para K-Means
res.nbclust <- NbClust(PUNTUACIONES, distance = "euclidean",
                       min.nc = 2, max.nc = 9, 
                       method = "kmeans", index ="all")
num.factors.optimo.kmeans<-factoextra::fviz_nbclust(res.nbclust) + 
  theme_minimal() + 
  ggtitle("N\u00FAmero \u00F3ptimo de clusters para K-Means") + 
  labs(x = "N\u00FAmero de clusters k", y = "Frecuencia")
png("NbClust K-Means.png", width = 12, height = 6, units = 'in', res = 300)
num.factors.optimo.kmeans
dev.off()
# Fijamos el numero de clusters en 5
num.clusters=5
# Ejecutamos el algoritmo de agrupacion
set.seed(421669)
KMclus <-  kmeans(PUNTUACIONES, centers = num.clusters,
                  iter.max = 25 )
names(KMclus$size)<-c(1:num.clusters)
KMclus$size
ClusterKmeans<-as.factor(KMclus$cluster)
# Visualizamos los clusters sobre las puntuaciones factoriales
rownames(PUNTUACIONES)<-etiquetas
# TC1xTC2
png(paste0("Diagrama de dispersi\u00F3n K-Means TC1xTC2.png"), 
    width = 12, height = 6, units = 'in', res = 300)
fviz_cluster(object = KMclus, data = PUNTUACIONES, 
             choose.vars = c("TC1","TC2")) + theme_minimal()  +
  labs(title="Diagrama de dispersi\u00F3n TC1xTC2 con los barrios 
       de Val\u00E8ncia agrupados en 5 clusters",
       x="TC1: Vejez", y="TC2: Migraci\u00F3n",
       caption="Clasificaci\u00F3n por el algoritmo de las k-medias") + 
  theme(legend.position = "none")+
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2")
dev.off()
# TC1xTC3
png(paste0("Diagrama de dispersi\u00F3n K-Means TC1xTC3.png"), 
    width = 12, height = 6, units = 'in', res = 300)
fviz_cluster(object = KMclus, data = PUNTUACIONES, 
             choose.vars = c("TC1","TC3")) + theme_minimal()  +
  labs(title="Diagrama de dispersi\u00F3n TC1xTC3 con los barrios 
       de Val\u00E8ncia agrupados en 5 clusters",
       x="TC1: Vejez", y="TC3: Riqueza",
       caption="Clasificaci\u00F3n por el algoritmo de las k-medias") + 
  theme(legend.position = "none")+
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2")
dev.off()
# TC2xTC3
png(paste0("Diagrama de dispersi\u00F3n K-Means TC2xTC3.png"), 
    width = 12, height = 6, units = 'in', res = 300)
fviz_cluster(object = KMclus, data = PUNTUACIONES, 
             choose.vars = c("TC2","TC3")) + theme_minimal()  +
  labs(title="Diagrama de dispersi\u00F3n TC2xTC3 con los barrios 
       de Val\u00E8ncia agrupados en 5 clusters",
       x="TC2: Migraci\u00F3n", y="TC3: Riqueza",
       caption="Clasificaci\u00F3n por el algoritmo de las k-medias") + 
  theme(legend.position = "none")+
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2")
dev.off()
paleta=brewer.pal(5,"Dark2")
metodo="K-Means"
num.clusters=5
valor_ok<-ClusterKmeans 
# Dibujamos la clasificacion en el mapa de Valencia

# ********** CODIGO MAPA **********
etiquetasBA<-c(etiquetas[1:48],"",etiquetas[49:73],"17.4","17.5","17.5",
               etiquetas[75:84],"19.7","19.8")
leyenda<<-c(paste0("Grupo ", 1:num.clusters))
colores=c()
for (i in c(1:length(valor_ok))){
  if (valor_ok[i]==1){colores=c(colores,paleta[1])}
  if (valor_ok[i]==2){colores=c(colores,paleta[2])}
  if (valor_ok[i]==3){colores=c(colores,paleta[3])}
  if (valor_ok[i]==4){colores=c(colores,paleta[4])}
  if (valor_ok[i]==5){colores=c(colores,paleta[5])}
  if (valor_ok[i]==6){colores=c(colores,paleta[6])}
  if (valor_ok[i]==7){colores=c(colores,paleta[7])}
  if (valor_ok[i]==8){colores=c(colores,paleta[8])}
  if (valor_ok[i]==9){colores=c(colores,paleta[9])}
  if (valor_ok[i]==10){colores=c(colores,paleta[10])}
  if (valor_ok[i]==11){colores=c(colores,paleta[11])}
  if (valor_ok[i]==12){colores=c(colores,paleta[12])}
  if (valor_ok[i]==13){colores=c(colores,paleta[13])}
  if (valor_ok[i]==14){colores=c(colores,paleta[14])}
  if (valor_ok[i]==15){colores=c(colores,paleta[15])}
}
# (Arreglo para ampliar a los 89 poligonos los 85 barrios) 
colores=c(colores[1:48],"transparent",colores[49:length(colores)])
colores=c(colores[1:75],colores[75],colores[75:length(colores)],
          colores[length(colores)])
valor_ok=c(valor_ok[1:48],NA,valor_ok[49:length(valor_ok)])
valor_ok=c(valor_ok[1:75],valor_ok[75],valor_ok[75:length(valor_ok)], 
           valor_ok[length(valor_ok)])
barrios@data$colores=colores
barrios@data$valores=valor_ok
# Definimos el formato del titulo del mapa
tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 22px;
  }
"))
# Titulo del mapa
title <- tags$div(
  tag.map.title, HTML(paste0("Clusterizaci\u00F3n obtenida por ",metodo))
) 
# Con la funcion leaflet() creamos el mapa
mapa<-leaflet(barrios, options = leafletOptions(zoomControl = FALSE)) %>%
  addTiles() %>%
  setView(-0.37629, 39.46991, zoom = 12.5) %>%
  addPolygons(weight = 1,color="black", opacity=1,  fillColor = ~colores, 
              fillOpacity = 0.8, label = etiquetasBA, 
              labelOptions = labelOptions(noHide = T, textOnly = TRUE, 
                                          direction="center",style = list(
                                            "color" = "black",
                                            "font-weight" = "bold",
                                            "font-family" = "serif",
                                            "font-style" = "italic",
                                            "font-size" = "12px"
                                          )))%>%
  addLegend("bottomleft", colors=paleta, 
            labels = leyenda,
            title = "Clusters",
            labFormat = labelFormat(prefix = "$"),
            opacity = 0.75
  ) %>%
  addLegend("topright", 
            colors=paste0(c(colores[72], "transparent", colores[75], 
                            "transparent", colores[76], "transparent", 
                            colores[78], "transparent", colores[79],"transparent", 
                            colores[84], "transparent",colores[85], "transparent", 
                            colores[86], "transparent", colores[87]),
                          c(rep(c("; border:3px solid black",""),8),
                            "; border:3px solid black"),"; border-radius:50%"), 
            labels = c("17.1 ", "","17.4 ", "","17.5 ", "","17.6 ", "","17.7 ", "",
                       "19.3", "","19.4 ", "","19.5 ", "","19.6 "),
            title = "",
            labFormat = labelFormat(prefix = "$"),
            opacity = 0.75
  ) %>%
  addControl(title, position = "topleft", className="map-title")
mapa
# Guardamos el mapa html en formato .png
saveWidget(mapa, "temp.html", selfcontained = FALSE)
webshot("temp.html", 
        file = paste0("Clusterizaci\u00F3n ", metodo, " ",num.clusters, ".png"),
        cliprect = "viewport")

# *********************************

# Elegimos un metodo de clustering jerarquico aglomerativo 
# para una segmentacion mas detallada
# De los disponibles en la funcion hclust(), 
# nos quedaremos con el de mayor correlacion cofenetica
metodos<-c("ward.D", "ward.D2","single", "complete", "average", 
           "mcquitty", "median", "centroid")
distancias<-c("euclidean","maximum", "manhattan", "canberra", "minkowski")
coeficientes<-matrix(data = NA, nrow = length(metodos), ncol = length(distancias))
colnames(coeficientes)<-distancias
rownames(coeficientes)<-metodos
for(met in metodos){
  for(dist in distancias){
    clus <- hclust(dist(PUNTUACIONES, method = dist), method= met) 
    # Coeficiente de correlacion cofenetica
    i=grep(met,metodos)
    j=grep(dist,distancias)
    coeficientes[i,j]<-cor(dist(PUNTUACIONES, method = dist), cophenetic(clus))
  }
}
# Elegimos el metodo de Average-Linkage con distancia maximum
# Con el paquete NbClust elegimos el numero optimo de clusters para Average-Linkage
res.nbclust <- NbClust(PUNTUACIONES, distance = "maximum",
                       min.nc = 9, max.nc =  15, 
                       method = "average", index ="all")
num.factors.optimo.ward<-factoextra::fviz_nbclust(res.nbclust) + 
  theme_minimal() + 
  ggtitle("N\u00FAmero \u00F3ptimo de clusters para Average-Linkage") + 
  labs(x = "N\u00FAmero de clusters k", y = "Frecuencia")
png("NbClust Average-Linkage.png", width = 12, height = 6, units = 'in', res = 300)
num.factors.optimo.ward
dev.off()
# Fijamos el numero de clusters
num.clusters=11
# Realizamos la clasificacion 
clus <- hclust(dist(PUNTUACIONES, method = "maximum"), method= "average") 
clus_cut11= cutree(clus, 11) 
colors11<-c(brewer.pal(8,"Dark2"),brewer.pal(3,"Accent"))
paleta=colors11
metodo="Average-Linkage"
valor_ok<-clus_cut11
# Obtenemos el dendograma
clus$labels<-etiquetas
png("Dendograma.png", width = 12, height = 12, units = 'in', res = 300)
plot(as.phylo(clus), type = "fan", tip.color = colors11[clus_cut11], 
     label.offset = 0, cex = 1.2,cex.main=2,
     main=paste0("Barrios de Val\u00E8ncia agrupados en 11 clusters"))
dev.off()
# Dibujariamos el mapa con los clusters de forma analoga al anterior
