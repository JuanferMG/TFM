# ***** ANALISIS FACTORIAL EXPLORATORIO *****

# Calculamos la matriz de correlaciones del conjunto de datos
correlaciones<-cor(DataSet, use = "pairwise.complete.obs")
# Se precisan correlaciones altas para sacar factores
# Graficamos la matriz para verlas mejor
png("Correlaciones.png", width = 10, height = 10, units = 'in', res = 300)
corrplot(cor(correlaciones), order = "hclust", tl.col='black', tl.cex=1) 
dev.off()
# Una forma de comprobar que podemos factorizar las variables de forma eficiente 
# es observando que el determinante de la matriz es aproximadamente 0
det(correlaciones) 
# La forma mas habitual para comprobar que podemos factorizar las variables 
# de forma eficiente es estudiando si el indice de Kaiser-Meyer-Olkin es > 0.7
KMO<-KMO(DataSet) 
print(KMO$MSA, digits = 2)

# Dibujamos el grafico de sedimentacion para decidir el numero de factores
png("Scree Plot.png", width = 12, height = 7, units = 'in', res = 300)
scree(DataSet, factors = F, pc = T, main = "Gr\u00E1fico de sedimentaci\u00F3n")
dev.off()
# Fijamos el numero de factores
num.factores=3
# Para factorizar usaremos el metodo cuyos factores resultantes expliquen
# mayor porcentaje de la variablidad del total del conjunto de datos  
# Calculamos soluciones factoriales por el metodo de Maxima Verosimilitud:
mle_none<-factanal(x=DataSet, factors=num.factores, rotation = "none", 
                   method="mle", lower = 0.05)
loadings(mle_none)
mle_varimax<-factanal(x=DataSet, factors=num.factores, method="mle", lower = 0.05)
loadings(mle_varimax)
mle_promax<-factanal(x=DataSet, factors=num.factores, rotation="promax", 
                     method="mle", lower = 0.05)
loadings(mle_promax) 
# Calculamos soluciones factoriales por el metodo de las Componentes Principales:
pc_none<-principal(cor(DataSet), nfactors=num.factores, rotate="none")
pc_none$loadings
pc_varimax<-principal(cor(DataSet), nfactors=num.factores, rotate="varimax")
pc_varimax$loadings
pc_promax<-principal(cor(DataSet), nfactors=num.factores, rotate="promax")
pc_promax$loadings 
pc_quartimax<-principal(cor(DataSet), nfactors=num.factores, rotate="quartimax")
pc_quartimax$loadings
pc_oblimin<-principal(cor(DataSet), nfactors=num.factores, rotate="oblimin")
pc_oblimin$loadings
pc_simplimax<-principal(cor(DataSet), nfactors=num.factores, rotate="simplimax")
pc_simplimax$loadings 
pc_cluster<-principal(cor(DataSet), nfactors=num.factores, rotate="cluster")
pc_cluster$loadings
# Calculamos soluciones por el metodo de factorizacion de Ejes Principales:
ejes<-principalAxis(cor(DataSet),nFactors=num.factores)
ejes$scores<-factor.scores(DataSet,ejes)$scores
ejes$cumVarExplained
# Elegimos la solucion devuelta por el metodo de las Componentes Principales con
# rotacion simplimax, la cual explica un 64 % de la varianza
COMPONENTES<-pc_simplimax
print(COMPONENTES, cut = 0.5, digits = 3)

# Comenzamos a eliminar variables que aportan poca informacion
# 1. Quitamos el indicador 11, el que menos comunalidad tiene: 0.0542 (<0.25)
DataSetCut<-DataSet[-c(11)]
COMPONENTES<-principal(cor(DataSetCut), nfactors=num.factores, rotate="simplimax")
print(COMPONENTES, cut = 0.5, digits = 3)
# 2. Quitamos el indicador 02, el que menos comunalidad tiene: 0.189 (<0.25)
DataSetCut<-DataSet[-c(2,11)]
COMPONENTES<-principal(cor(DataSetCut), nfactors=num.factores, rotate="simplimax")
print(COMPONENTES, cut = 0.5, digits = 3)
# 3. Quitamos el indicador 03, el que menos comunalidad tiene: 0.187 (<0.25)
DataSetCut<-DataSet[-c(2,3,11)]
COMPONENTES<-principal(cor(DataSetCut), nfactors=num.factores, rotate="simplimax")
print(COMPONENTES, cut = 0.5, digits = 3)
# 4. Quitamos el indicador 22, cargas factoriales inferiores a 0.5 en todos 
# los factores siendo el que menos comunalidad tiene: 0.289 (<0.25)
DataSetCut<-DataSet[-c(2,3,11,22)]
COMPONENTES<-principal(cor(DataSetCut), nfactors=num.factores, rotate="simplimax")
print(COMPONENTES, cut = 0.5, digits = 3)
# 5. Quitamos el indicador 19, cargas factoriales inferiores a 0.5 en todos 
# los factores siendo el que menos comunalidad tiene: 0.299 (<0.25)
DataSetCut<-DataSet[-c(2,3,11,19,22)]
COMPONENTES<-principal(cor(DataSetCut), nfactors=num.factores, rotate="simplimax")
print(COMPONENTES, cut = 0.5, digits = 3)
# 6. Quitamos el indicador 04, cargas factoriales inferiores a 0.5 en todos 
# los factores siendo el que menos comunalidad tiene: 0.443 (<0.25)
DataSetCut<-DataSet[-c(2,3,4,11,19,22)]
COMPONENTES<-principal(cor(DataSetCut), nfactors=num.factores, rotate="simplimax")
print(COMPONENTES, cut = 0.5, digits = 3)
# 7. Quitamos el indicador 05, el que menos comunalidad tiene: 0.179 (<0.25)
DataSetCut<-DataSet[-c(2,3,4,5,11,19,22)]
COMPONENTES<-principal(cor(DataSetCut), nfactors=num.factores, rotate="simplimax")
print(COMPONENTES, cut = 0.5, digits = 3)
# 8. Quitamos el indicador 30, cargas inferiores a 0.5 en todos los factores 
DataSetCut<-DataSet[-c(2,3,4,5,11,19,22,30)]
COMPONENTES<-principal(cor(DataSetCut), nfactors=num.factores, rotate="simplimax")
print(COMPONENTES, cut = 0.5, digits = 3)
# 9. Quitamos el indicador 23, al no ser especifico de un factor y tener 
# mayor complejidad: 2.70 (cross-loadings)
DataSetCut<-DataSet[-c(2,3,4,5,11,19,22,23,30)]
COMPONENTES<-principal(cor(DataSetCut), nfactors=num.factores, rotate="simplimax")
print(COMPONENTES, cut = 0.5, digits = 3)
# 10. Quitamos el indicador 13, al tener mayor complejidad: 2.98 (cross-loadings)
DataSetCut<-DataSet[-c(2,3,4,5,11,13,19,22,23,30)]
COMPONENTES<-principal(cor(DataSetCut), nfactors=num.factores, rotate="simplimax")
print(COMPONENTES, cut = 0.5, digits = 3)
# 11. Quitamos el indicador 18, al tener mayor complejidad: 2.64 (cross-loadings)
DataSetCut<-DataSet[-c(2,3,4,5,11,13,18,19,22,23,30)]
COMPONENTES<-principal(cor(DataSetCut), nfactors=num.factores, rotate="simplimax")
print(COMPONENTES, cut = 0.5, digits = 3)
# 12. Quitamos el indicador 20, al tener mayor complejidad: 2.25 (cross-loadings)
DataSetCut<-DataSet[-c(2,3,4,5,11,13,18,19,20,22,23,30)]
COMPONENTES<-principal(cor(DataSetCut), nfactors=num.factores, rotate="simplimax")
print(COMPONENTES, cut = 0.5, digits = 3)
# 13. Quitamos el indicador 21, al tener mayor complejidad: 1.97 (cross-loadings)
DataSetCut<-DataSet[-c(2,3,4,5,11,13,18,19,20,21,22,23,30)]
COMPONENTES<-principal(cor(DataSetCut), nfactors=num.factores, rotate="simplimax")
print(COMPONENTES, cut = 0.5, digits = 3)

# Obtenemos las puntuaciones factoriales
PUNTUACIONES<-factor.scores(DataSetCut, COMPONENTES)$scores
PUNTUACIONES<-as.data.frame(PUNTUACIONES)
colnames(PUNTUACIONES)<-paste0("TC",1:ncol(PUNTUACIONES))
rownames(PUNTUACIONES)<-unique(DatosBA$Barrio)
# Nombramos: 
# TC1=VEJEZ (Indicadores 1, 6, 12, 14, 15, 16, 17, 25, 26, 28, 29)
# TC2=MIGRACION (Indicadores 7, 8, 9, 10, 24, 27, 31, 32, 33)
# TC3=RIQUEZA (Indicadores 31, 32, 33)

# Estudiamos la fiabilidad de la consistencia interna de los factores 
# calculando el alpha de Cronbach
print(COMPONENTES$loadings, cut = 0.5, digits = 3)
TC1<-c("Ind01", "Ind06", "Ind12", "Ind14", "Ind15", "Ind16",
       "Ind17", "Ind25", "Ind26", "Ind28", "Ind29")
TC2<-c("Ind07", "Ind08", "Ind09", "Ind10", "Ind24", "Ind27",
       "Ind31", "Ind32", "Ind33")
TC3<-c("Ind31", "Ind32", "Ind33")
alpha.TC1 = alpha(DataSetCut[TC1], keys = c(-1,1,-1,-1,-1,1,1,1,-1,1,1),
                  check.keys = TRUE)
print(alpha.TC1, digits = 2)
alpha.TC2 = alpha(DataSetCut[TC2], keys = c(1,1,1,1,1,-1,-1,-1,1), 
                  check.keys = TRUE)
print(alpha.TC2, digits = 2)
alpha.TC3 = alpha(DataSetCut[TC3], keys = c(1,1-1), check.keys = TRUE)
print(alpha.TC3, digits = 2)
print(alpha.TC1$total$raw_alpha, digits = 3)
print(alpha.TC2$total$raw_alpha, digits = 3)
print(alpha.TC3$total$raw_alpha, digits = 3)

# Dibujamos los diagramas de dispersion de las puntuaciones factoriales
etiquetas<-unique(DatosBA$DTBA)/10
etiquetas[74]<-"17.4 y 17.5"
etiquetas[85]<-"19.7 y 19.8"
PUNTUACIONES_DT<-cbind(PUNTUACIONES,DT=DatosBA$DT[1:85])
diagrama1<-ggplot(PUNTUACIONES_DT, 
                  aes(TC1, TC2, color = as.factor(DT), label=etiquetas)) + 
  geom_point() + 
  geom_text(aes(label=etiquetas),hjust=0, vjust=-0.5) +
  labs(x = "TC1: Vejez",
       y = "TC2: Migrac\u00F3n",
       title="Diagrama de dispersi\u00F3n de las puntuaciones factoriales 
       obtenidas de TC1 y TC2",
       caption="Los barrios han sido coloreados seg\u00FAn el distrito 
       al que pertenecen", colour="Distrito")
diagrama1
ggsave(filename = paste0("Diagrama de dispersi\u00F3n TC1xTC2.png"), diagrama1,
       width = 12, height = 6, dpi = 300, units = "in", device='png')
diagrama2<-ggplot(PUNTUACIONES_DT, 
                  aes(TC1, TC3, color = as.factor(DT), label=etiquetas)) + 
  geom_point() + 
  geom_text(aes(label=etiquetas),hjust=0, vjust=-0.5) +
  labs(x = "TC1: Vejez",
       y = "TC3: Riqueza",
       title="Diagrama de dispersi\u00F3n de las puntuaciones factoriales 
       obtenidas de TC1 y TC3",
       caption="Los barrios han sido coloreados seg\u00FAn el distrito 
       al que pertenecen", colour="Distrito")
diagrama2
ggsave(filename = paste0("Diagrama de dispersi\u00F3n TC1xTC3.png"), diagrama2,
       width = 12, height = 6, dpi = 300, units = "in", device='png')
diagrama3<-ggplot(PUNTUACIONES_DT, 
                  aes(TC2, TC3, color = as.factor(DT), label=etiquetas)) + 
  geom_point() + 
  geom_text(aes(label=etiquetas),hjust=0, vjust=-0.5) +
  labs(x = "TC2: Migraci\u00F3n",
       y = "TC3: Riqueza",
       title="Diagrama de dispersi\u00F3n de las puntuaciones factoriales 
       obtenidas de TC2 y TC3",
       caption="Los barrios han sido coloreados seg\u00FAn el distrito 
       al que pertenecen", colour="Distrito")
diagrama3
ggsave(filename = paste0("Diagrama de dispersi\u00F3n TC2xTC3.png"), diagrama3,
       width = 12, height = 6, dpi = 300, units = "in", device='png')
