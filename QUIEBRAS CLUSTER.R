###EL ANÁLISIS CLUSTER - QUIEBRAS BANCARIAS###

quiebras<-read.csv("quiebras.csv", header=TRUE,sep=";",dec=".", row.names = 1) #pongo row.names=1 para coger la columna 1 como nombre de las filas en lugar de su número.
quiebras
View(quiebras)


dim(quiebras)
str(quiebras)

quiebras$quiebra

quiebras<-na.omit(quiebras)
head(quiebras)


quiebras.escalado<-data.frame(scale(quiebras[,-1])) #Escalamos y quitamos la 1º columna para hacerlo numérico
quiebras.escalado
library(factoextra)
library(cluster)

summary(quiebras.escalado)

q.dist<-get_dist(quiebras.escalado, stand=FALSE, method= "pearson") #sólo admite valores numéricos. Coef. correlacion PEARSON yA ESTÁ ESCALADO así que ponemos stand=FALSE
58*57
str(q.dist)
head(q.dist)
fviz_dist(q.dist, lab_size = 5)  #Graficamos para ver distancias entre bancos

dist.eucli<-dist(quiebras.escalado)
round(as.matrix(dist.eucli)[1:6, 1:6], 1)
 
quiebras.cor<-cor(t(quiebras.escalado), method = "pearson")
ratios.cor<-cor(quiebras.escalado)
ratios.cor


######################CLÚSTERS NO JERÁRQUICOS Y EL K-MEANS###########################

#Quitamos la primera variable, pues la información sobre quiebra/ no quiebra no aporta ahora nada al proceso. 
quiebras=quiebras[,-1]

set.seed(123) # para poder reproducir el algoritmo desde un punto aleatorio
km.quiebras = kmeans(quiebras, 3, nstart = 25)
km.quiebras

#Y observamos entonces diversos elementos: # Grupo de pertenencia 
km.quiebras$cluster

# Tamaño de cada grupo 
km.quiebras$size

#Hacemos una tabla para conocer a qué grupo pertenece cada uno: 
tab = table(rownames(quiebras), km.quiebras$cluster) # para 
head(tab,20) 

#Obtenemos entonces los centroides de cada grupo, 
km.quiebras$centers

km.quiebras$centers[2,] # Centroides del grupo 2


#Y representamos los grupos por pares de variables: 
plot(quiebras, col = km.quiebras$cluster, pch = 19, frame = FALSE, main = "k medias con k = 3")

#Observamos dos variables: 
plot(quiebras[,c(1,8)],col=km.quiebras$cluster, pch = 19, frame = FALSE, 
     main = "k medias con k = 3 para AC s/AT vs CteVtas s/Vtas") 
#Agregamos los centroides 
points(km.quiebras$centers[,c(1,8)], col = 1:3, pch = 16, cex = 2)

#Procedemos a determinar el número ‘óptimo’ de grupos. 
#Aplicamos ahora el método del hombro (ﬁjando la vertical sobre 3 grupos tras haber visto el resultado, no antes):

library(factoextra)             #Aplicamos el corte en 3 clusters o número óptimo de grupos.
set.seed(123)
fviz_nbclust(quiebras, kmeans, method = "wss") + 
  geom_vline(xintercept = 3, linetype = 2)


set.seed(123) # K medias con nstart = 1 
km.q = kmeans(quiebras, 3, nstart = 1) 
km.q$tot.withinss  #En este caso, debido a la enorme heterogeneidad que provocan las tres observaciones del grupo 2, no existen diferencias observables.



###El algoritmo PAM, Partitioning Around Medoids###
require(fpc) #si no lo tenemos cargado, para ejecutar pam()
require(ggrepel)

fviz_cluster(km.q, data=quiebras, labelsize=3, repel=TRUE)

#Lo llevamos a cabo con k=3 grupos, sin tipiﬁcar variables. 
pam.q = pam(quiebras, 3)

#Los medioides de los grupos vienen dados por:
pam.q$medoids

head(pam.q$cluster)

#Podemos entonces efectuar una primera representación: 
clusplot(pam.q, main = "PAM de k = 3", color = TRUE)

#These two components explain 78.15% of the point variability

#Esta representación se mejora fácilmente: 
fviz_cluster(pam.q, data=quiebras, labelsize=3, repel=TRUE)    # Si usa el quiebras.escalado obtenemos la gráfica de los apuntes ZAFRA

#Podemos también representar el perﬁl de cada cluster. 
plot(silhouette(pam.q), col = 2:4)
#Este gráﬁco ofrece el número de elementos de cada grupo, 
#con cada línea representando una observación y su longitud la anchura del perﬁl

#Alternativamente
fviz_silhouette(silhouette(pam.q))


####CLARA: Clustering LArge Applications ####
# Algoritmo para efectuar análisis cluster sobre grandes conjuntos de varios miles de observaciones.

#Cargamos el archivo churn.arff 
install.packages("RWeka") # para leer archivos con formato arff 
library(RWeka) 
churn=read.arff("churn.arff") 


churn<-read.csv("churn.csv", header=TRUE,sep=",",dec=".", row.names = 1)
head(churn) 


#Trabajamos con las 7 variables métricas, tras comprobar que se trata de 20.000 observaciones completas:   
churn_compl = na.omit(churn) 
churn_compl=churn_compl[,2:8] 
summary(churn_compl)

#Tomamos una muestra de 1.000 observaciones, 
set.seed(123)
churn.mas = churn_compl[sample(1:nrow(churn_compl), 1000, replace=FALSE),] 
churn.mas.tip=scale(churn.mas) 
summary(churn.mas.tip)

#Aplicamos ahora la función NbClust()1. 
require(NbClust)
Nb.churn=NbClust(churn.mas.tip, distance = "euclidean", min.nc = 2, 
                 max.nc = 10, method = "complete", index ="all") 

fviz_nbclust(Nb.churn) + theme_minimal() + 
  labs(x="Número k de clusters", y="Frecuencia")

#Como vemos, la opción mayoritaria señala 4 grupos, que escogeremos. 
require(cluster) 
churn.clara=clara(churn_compl, 4, samples=200) 
require(factoextra) 
fviz_cluster(churn.clara, stand = FALSE, geom = "point", pointsize = 1)   #Solo tengo un 50% explicado, lo cual indica que las variables no se parecen tanto como anteriormente. Le puedo poner TRUE O FALSE

plot(silhouette(churn.clara), col = 2:5, main = "Gráfico de perfil")

# Medioides 
churn.clara$medoids 

require(cluster) 
require(e1071)
require(factoextra)

#Introducción: concepto de segmentación borrosa (Probabilidad de pertenencia a un grupo u otro)

#Fuzzy Analysis Clustering
library(cluster) 
set.seed(123)
# Llevamos a cabo la segmentación borrosa con 3 grupos 
fanny.q = fanny(quiebras, 3)
# Representamos 
clusplot(fanny.q)

# Con factoextra 
# library(factoextra) 
fviz_cluster(fanny.q, ellipse.type = "norm",ellipse.level = 0.68)

# Gráfico de perfil 
fviz_silhouette(fanny.q, label = TRUE)+ 
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size=6,color="chocolate4"))

###Algoritmo C medias



