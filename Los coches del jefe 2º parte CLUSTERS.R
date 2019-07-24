###LOS COCHES DEL JEFE 2º PARTE-CLUSTERS###

############ Carga de librerias ##############
library(memisc)
library(haven)
library(foreign)
library(dplyr)
library(factoextra)
library(cluster)
library(factoextra)
require(clustertend)
library("NbClust")
library(FactoMineR)


ruta <- 'tterreno.sav'

coches <- read.spss(ruta, to.data.frame = T)
coches <- data.frame(coches[,-1], row.names = make.names(coches[,1], unique = T))

summary(coches)


View(coches)

coches[116, 11] <- mean(coches[c(119, 120, 118, 121, 117), 11])
coches[116, 11]
coches[c(75:79), 11] <- mean(coches[c(63:74), 11])
coches[19, 11] <- mean(coches[c(13:18, 19:22), 11])
coches[c(105, 106), 12] <- 144
coches[114, 12] <- 135


perfomScaling <-  T
if(perfomScaling){
  for(i in names(coches)){
    if(class(coches[,i ]) == 'integer' | class(coches[,i ]) == 'numeric'){
      coches[,i ] = scale(coches[,i ])
    }
  }
}

#Creamos un nuevo dataframe, con las columnas buenas.
columnasnum <- c('potencia','rpm','peso','consurb','velocida')
cochesescalados <- subset(coches, select = columnasnum)

#Peso
cochesescalados[c(7, 8), 3] <- mean(cochesescalados[c(6, 9:12), 3])
cochesescalados[c(7, 8), 3]
cochesescalados[19, 4] <- mean(cochesescalados[c(13:18, 20:22), 4])

anyNA(cochesescalados)


#Obtenemos las distancias del anterior DF a través de Pearson
qdist <- get_dist(cochesescalados, stand = T, method = 'pearson')
qdist.manhattan <- get_dist(cochesescalados, stand = T, method = 'manhattan')
qdist.mink <- get_dist(cochesescalados, stand = T, method = 'minkowski')
str(qdist)

dist.cor <- as.dist(1 - cochescorr)
round(as.matrix(dist.cor),  2)
dist.cor <- as.dist(1 - cochescorr)
round(as.matrix(dist.cor),  2)

#Realizamos la representación gráfica.
fviz_dist(qdist, lab_size = 5)
as.matrix(as.dist(qdist))

#Cambiamos la representación 
fviz_dist(qdist,gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"), lab_size = 5)

d <- dist(cochesescalados, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D2")
plot(fit)

plot(fit, cex = 0.6, hang = -1, main="Dendrograma - hclust")
rect.hclust(fit, k=5, border = 2:4)



groups <- cutree(fit, k=5)
rect.hclust(fit, k=5, border="red")

#Ahora analizamos la correlación entre las variables
cochescorr <- cor(cochesescalados, method = 'pearson')
round(cochescorr,3)

#Y ahora la convertimos en la matriz de distancias
dist.cor <- as.dist(1 - cochescorr)
round(as.matrix(dist.cor),  2)

#Otra forma de obtención de las distancias
daisy(cochescorr, metric = c('manhattan'), stand = F)##Stand lo que hace es standarizar las variables.

#Agrupación
coches.eclust <- eclust(cochesescalados, FUNcluster = 'kmeans', stand = T, hc_metric = 'euclidean',  nstart = 25)

coches.eclust.j = eclust(cochesescalados[,-1], "hclust", k = 4)
fviz_cluster(coches.eclust.j)
fviz_silhouette(coches.eclust)                         #Este es el gráfico que mejor representa los clusters


fviz_nbclust(cochesescalados, kmeans, method = "silhouette") +
  ggtitle("Número óptimo de clusters - k medias") +
  labs(x="Número k de clusters",y="Anchura del perfil promedio")

k2 <- kmeans(cochesescalados, centers = 2, nstart = 25)
k3 <- kmeans(cochesescalados, centers = 3, nstart = 25)
k4 <- kmeans(cochesescalados, centers = 4, nstart = 25)
k5 <- kmeans(cochesescalados, centers = 5, nstart = 25)
k6 <- kmeans(cochesescalados, centers = 6, nstart = 25)
k7 <- kmeans(cochesescalados, centers = 7, nstart = 25)
k8 <- kmeans(cochesescalados, centers = 8, nstart = 25)
k9 <- kmeans(cochesescalados, centers = 9, nstart = 25)
k10 <- kmeans(cochesescalados, centers = 10, nstart = 25)


p1 <- fviz_cluster(k2, geom = 'point', data = cochesescalados) + ggtitle('K = 2')
p2 <- fviz_cluster(k3, geom = 'point', data = cochesescalados) + ggtitle('K = 3')
p3 <- fviz_cluster(k4, geom = 'point', data = cochesescalados) + ggtitle('K = 4')
p4 <- fviz_cluster(k5, geom = 'point', data = cochesescalados) + ggtitle('K = 5')
p5 <- fviz_cluster(k6, geom = 'point', data = cochesescalados) + ggtitle('K = 6')
p6 <- fviz_cluster(k7, geom = 'point', data = cochesescalados) + ggtitle('K = 7')
p7 <- fviz_cluster(k8, geom = 'point', data = cochesescalados) + ggtitle('K = 8')
p8 <- fviz_cluster(k9, geom = 'point', data = cochesescalados) + ggtitle('K = 9')
p9 <- fviz_cluster(k10, geom = 'point', data = cochesescalados) + ggtitle('K = 10')

library(gridExtra)
grid.arrange(p1, p2, p3, p4, p5, nrow = 2)

grid.arrange(p6, p7, p8, p9, nrow = 2)

##Realizamos este plot, para observar cual es el número óptimo de clusteres
fviz_nbclust(x = cochesescalados, FUNcluster = kmeans, method = "wss", k.max = 15, 
             diss = get_dist(cochesescalados, method = "euclidean"), nstart = 50)

set.seed(123)
km_clusters <- kmeans(x = cochesescalados, centers = 4, nstart = 25)
fviz_cluster(object = km_clusters, data = cochesescalados, show.clust.cent = TRUE,
             ellipse.type = "convex", star.plot = TRUE, repel = TRUE) +
  labs(title = "Resultados clustering K-means") +
  theme_bw() +
  theme(legend.position = "none")

fviz_dend(hclust(dist(cochesescalados)), k = 4, cex = 0.5, main = "Dendrograma")


coches.eclust = eclust(cochesescalados, FUNcluster = "kmeans", stand = TRUE,
                       hc_metric = "euclidean", nstart = 25, k = 4)

coches.eclust$nbclust


fviz_nbclust(cochesescalados[, -1], kmeans, method = "silhouette") +
  ggtitle("Número óptimo de clusters - k medias") +
  labs(x = "Número k de clusters",y = "Anchura del perfil promedio")


fviz_nbclust(cochesescalados, hcut, method = "silhouette", hc_method = "complete") +
  ggtitle("Número óptimo de clusters - jerárquico") +
  labs(x = "Número k de clusters", y = "Anchura del perfil promedio")



# Aplicamos el estadístico sobre los datos reales
set.seed(123)
# (prueba es el objeto que contiene los datos reales de EBITDA y Book Value)
hopkins(cochesescalados, n= nrow(cochesescalados)-1)

bondad_ac <- get_clust_tendency(cochesescalados, nrow(cochesescalados)-1)
bondad_ac$hopkins_stat
bondad_ac$plot +
  scale_fill_gradient(low = "steelblue", high = "white")


set.seed(123)
k.max = 15 # Máximo número de clusters
# lo aplicamos sobre sec.def, ya tipificado
wss= sapply(1:k.max,
            function(k){kmeans(cochesescalados[, -1], k, nstart=10 )$tot.withinss})
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE,
     xlab="Número K de clusters",
     ylab="Suma total de cuadrados intra-clusters")
abline(v = 3, lty =2)
abline(v = 4, lty =3)

fviz_nbclust(cochesescalados, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2) +
  geom_vline(xintercept = 4, linetype = 3) +
  ggtitle("Número óptimo de clusters - k medias") +
  labs(x="Número k de clusters",y="Suma total de cuadrados intra grupos")


cadamarcacorrespondeauncluster <- kmeans(cochesescalados,5)
sum(cadamarcacorrespondeauncluster$cluster == 5)

set.seed(123)
clus.nb = NbClust(cochesescalados, distance = "euclidean",
                  min.nc = 2, max.nc = 10,
                  method = "complete", index ="gap")
clus.nb # resultados
clus.nb$Best.nc

