### EXAMEN 30/01/2019

library(dplyr)

datos <- read.csv('C:/Users/Knowhow/Desktop/CUNEF/TECNICAS DE AGRUPACION Y REDUCCION DE LA DIMENSION/EXAMEN/EGT2010_2017.csv')
str(datos)
datos_nuevo <- datos[, c(81:93, 3, 4, 6, 21, 72, 112, 110, 109, 80, 151, 146)]
which(is.na(datos_nuevo))

summary(datos_nuevo)

datos_nuevo_sinNA <- na.omit(datos_nuevo)
summary(datos_nuevo_sinNA)

primer_data <- datos_nuevo_sinNA %>% filter(datos_nuevo_sinNA$AÑO <= 2014)
segundo_data <- datos_nuevo_sinNA %>% filter(datos_nuevo_sinNA$AÑO > 2014)

set.seed(1234)

primer_150 <- primer_data[sample(nrow(primer_data), 150), ]
segundo_150 <- segundo_data[sample(nrow(segundo_data), 150),]


#Procedemos a eliminar los data frames que no vamos a volver a usar
rm(datos)
rm(datos_nuevo)
rm(datos_nuevo_sinNA)
rm(primer_data)
rm(segundo_data)


## Vamos a proceder a realizar la limpieza del dataframe, y para ello vamos a sumar varias variables y aplicarles la media, debido que hay un problema de multicolinealidad
#, es decir, hay una fuerte correlación entre las variables explicativas del modelo ya 
# que puede que Los coeficientes estimados serán muy sensibles ante pequeños cambios en los datos.
str(primer_150)

Alojamiento_general = (primer_150$VALORACION_ALOJ + 
                         primer_150$VALORACION_TRATO_ALOJ + 
                         primer_150$VALORACION_GASTRONO_ALOJ) /3
Entorno_general = (primer_150$VALORACION_CLIMA +
                     primer_150$VALORACION_ZONAS_BANYO + primer_150$VALORACION_PAISAJES + 
                     primer_150$VALORACION_MEDIO_AMBIENTE + primer_150$VALORACION_TRANQUILIDAD +
                     primer_150$VALORACION_LIMPIEZA) / 6
Restaurante_general = (primer_150$VALORACION_CALIDAD_RESTAUR + 
                         primer_150$VALORACION_OFERTA_GASTR_LOC + 
                         primer_150$VALORACION_TRATO_RESTAUR +
                         primer_150$VALORACION_PRECIO_RESTAUR) / 4

str(Alojamiento_general)

#Reestructuracion de ingresos
library("dummies")
ingresos = dummy(primer_150$INGRESOS)

i1=ingresos[,1]*((12000+24000)/2)
i2= ingresos[,2]*((24000 + 36000)/2)
i3=ingresos[,3]*((36001 +48000)/2)
i4=ingresos[,4]*((48001 + 60000)/2)
i5=ingresos[,5]*((60001+72000)/2)
i6=ingresos[,6]*((72001+84000)/2)
i7=ingresos[,7]*((84000+12000)/2)

ingresos=(i1+i2+i3+i4+i5+i6+i7)/10000
head(ingresos)

primer_150_unido <- data.frame(primer_150$IMPRESION*2, Alojamiento_general, Restaurante_general,Entorno_general,
                               ingresos, primer_150$EDAD)

str(primer_150_unido)

library(factoextra)
library(FactoMineR)
library(dplyr)
#Analizamos las estructuras de los dos datas
str(primer_150)
str(segundo_150)

head(primer_150)
tail(primer_150)



##Transformación de variables##

correlaciondata_pca <- cor(primer_150_unido)
correlaciondata_pca


##SE COMIENZA A REALIZAR EL ACP
#El objetivo del análisis de componentes principales (cuando se emplea como método de reducción de la dimensión del problema)
#es reducir el número de variables originales hasta un número menor de componentes de forma que cada uno de ellos forma un ´ındice de las variables originales; el número de componentes mantenidos recogerá la mayor parte posible de la varianza de los datos.

#Como podemos observar, hemos resuelto los problemas de multicolienalidad, pero en la matriz de correlaciones se observa que no 
#Existe demasiada relación entre las variables, eso hace asumir que habrá una pérdida considerable de información
correlaciondata_pca


#0,3 (baja colinealidad), 0,5 (colinealidad media), 0,7 (colinealidad alta). Supuesto de colinealidad

det(correlaciondata_pca)

#cercano a 0, indica alta multicolinealidad entre las variables. igual a 0 (matriz no singular). 
#Supuesto de multicolinealidad test de esfericidad de Bartlett busca contrastar la hipótesis nula de que la matriz de correlaciones es igual a una matriz de identidad

datos_escalados <- scale(primer_150_unido)
datos_escalados
pairs(datos_escalados)

##Esfericidad de bartlet
library(psych)
##La prueba de esfericidad de Bartlett contrasta la hipotesis nula de que la matriz de correlaciones es una matriz de identidad, 
#en cuyo caso no existirian correlaciones significativas entre las variables y el modelo factorial o el modelo ACP no seria pertinente.
## Si Sig. (p-valor) < 0.05 aceptamos H0 (hipotesis nula) > se puede aplicar el analisis.
cortest.bartlett(correlaciondata_pca, n = nrow(primer_150))
#El determinante nos sirve para analizar la relación existente entre las variables, si es pequeño es bueno por que significa que hay relación entre las variables
#Y con ello si reducimos las dimensiones no habrá a penas perdida de informacion
det(correlaciondata_pca)

##KMO es una prueba estadistica la cual es una MSA, medida de adecuacion muestral, definicion(compara los valores de las correlaciones 
#entre las variables y sus correlaciones parciales si el indice Kmo esta proximo a 1 el ACP se puede hacer si es proximo a 0, el ACP no seria 
#relevante)
## definicion de kmo, como se puede observar en el kmo todos con mayores de 0.6, por tanto 
##Analisis de componentes principales. Cuanto mas cerca de 1 tenga el valor obtenido del test KMO, implica que la relacion entres las variables es alta.
#Si KMO  0.9
KMO(correlaciondata_pca)
#0,90 > KMO Muy bueno 0,90 > KMO > 0,80 Bueno 0,80 > KMO > 0,70 Aceptable 0,70 > KMO > 0,60 Mediocre o regular 0,60 > KMO > 0,50 Malo 
#0,50 > KMO Inaceptable o muy malo Autovalores y autovectores de la matriz de covarianzas de la muestra


library(missMDA)

acp <- PCA(correlaciondata_pca, scale.unit = T) 
## esta nos arroja resultados mas precisos. Nos retorna la dv estandar de cada uno de los componentes principales , los cuales coinciden con los autovalores de los componentes principales, y nos retorna el conjunto de componentes principales
##Utilizamos imputePCA, para eliminar los valores faltantes.
Sedimentacion <- princomp(primer_150_unido, scores = TRUE, cor = TRUE)
Sedimentacion
fviz_eig(acp, addlabels = TRUE, hjust = 0) + labs(title("Representacion"), x = "Dimensiones", y = "Porcentaje de v explicada") + theme_classic()

##Cual es la proporcion de varianza explicada por cada de una de estos componente y despues de ello elegir cual es el que nos vamos a quedar

summary(acp) ##Mirar proportion of variance 
#Según la regla de Kaiser nos quedaremos con tantos componentes principales como autovalores mayoes que 1 tengamos, por tanto nos quedamos con 2
acp$eig
#Esto nos muestra la proporción de cada variable explicada en cada dimensión, la DIM1, explica mejor Restaurante_general,Entorno_general e ingresos
#Y en la 2 la que mejor se explica es primer_150.EDAD.
acp$var$cos2

## Para un cierto nivel de signiﬁcación, si se obtienen valores altos de χ2 se rechazará la hipótesis nula, lo que indicará presencia de asociación entre las variables, 
#estando en consecuencia plenamente justiﬁcado el empleo del ANFAC.

# Métodos de extraccién de factores. 
# En este apartado nos dedicaremos a desarrollar algunos métodos que permitan la obtencién de la matriz factorial. ´Esta, como ya sabemos, veriﬁca la identidad fundamental dada por la relación (2), 
# relación que no basta para determinarla pues existen inﬁnitas matrices que lo hacen

###EL ANÁLISIS FACTORIAL###
#El análisis factorial (ANFAC) es un método de análisis multivariante que intenta explicar, según un modelo lineal, un conjunto extenso de variables observables mediante un número reducido de variables hipotéticas llamadas factores. 
#Un aspecto esencial del ANFAC es que los factores no sean directamente observables, obedeciendo a conceptos de naturaleza más abstracta que las variables originales.

#Puesto que los factores tienen como principal ﬁnalidad estudiar y simpliﬁcar las asociaciones entre las variables, medidas a través de la matriz de correlaciones
#se obtiene una nueva simpliﬁcación del problema suponiendo que las variables son también reducidas.

# Se pasa de una variable xi de media µi y desviación típica σi a una nueva variable reducida, para eliminar la inﬂuencia de la unidad de medida, 
# mediante la transformación

#El ANFAC, por su parte, (como objetivo) trata de identiﬁcar la estructura interna, subyacente, que explica la interacción entre las variables.



##Análisis Cluster

#El análisis de conglomerados o clúster es una técnica que se desarrolla para combinar observaciones en grupos, conglomerados o clústers que deben reunir ciertos requisitos, 
#como son :

#1. considerar que cada grupo debe ser homogéneo respecto de alguna caracteréstica, y, en ese sentido, considerar que las observaciones de cada grupo sean similares; y 
#2. considerar que cada grupo debe ser diferente de los demás respecto de las mismas características; esto es, las observaciones de cada grupo deben ser distintas (en ese sentido) de las observaciones del resto de grupos

#Una de las principales características radicará precisamente en la deﬁnición de la medida de homogeneidad o similitud, que dependerá de los objetivos concretos de cada estudio. 


# Método del centroide. En este m´etodo cada individuo se sustituye por un individuo tipo denominado centroide del grupo. 
# Tal centroide no es sino la media aritmética de las observaciones de los individuos que forman el cluster en cada una de las variables

#Una de las representaciones gráﬁcas más habituales de este tipo de actuación es la que conforma el denominado dendograma


require(cluster)
require(fpc)
require(factoextra)
require(dplyr)
library(dendextend)
require(ggrepel)
library(RWeka)
require(NbClust)
viajeros <- primer_150_unido
summary(viajeros)
viajerosna <- na.omit(viajeros)
viajerosna <- viajerosna[4:32,]
viajerosna_mas =viajerosna[sample(1:nrow(viajerosna), 1000,replace=TRUE),]
summary(viajerosna_mas)
#Calculamos algunos estadisticos
viajerosna_mas_stats = data.frame(
  Min = apply(viajeros, 2, min), # mínimo
  Med = apply(viajeros, 2, median), # mediana
  Mean = apply(viajeros, 2, mean), # media
  SD = apply(viajeros, 2, sd), # desviación típica
  Max = apply(viajeros, 2, max) # máximo
)

viajerosna_mas_stats = round(viajerosna_mas_stats, 1)
head(viajerosna_mas_stats)
#Tipificamos
viajeros_tip=scale(viajerosna_mas_stats)
summary(viajeros_tip)
datos_escalados_data = data.frame(
  Min = apply(datos_escalados, 2, min), # mínimo
  Med = apply(datos_escalados, 2, median), # mediana
  Mean = apply(datos_escalados, 2, mean), # media
  SD = apply(datos_escalados, 2, sd), # desviación típica
  Max = apply(datos_escalados, 2, max) # máximo
)



#euclidean
Hepta_d_euclidian <- dist(primer_150_unido, method = "euclidean")

#manhattan
Hepta_d_manhattan<- dist(primer_150_unido, method = "manhattan")

heatmap(as.matrix(Hepta_d_euclidian), Rowv = NA, Colv=NA)
heatmap(as.matrix(Hepta_d_manhattan), Rowv = NA, Colv=NA)

Hepta_d<-cbind(c(as.vector(Hepta_d_euclidian)),c(as.vector(Hepta_d_manhattan)))
colnames(Hepta_d)<-c("euclidean","manhattan")

library(reshape)
library(ggplot2)
Hepta_d2<-melt(Hepta_d)

ggplot(Hepta_d2, aes(value, fill=X2)) + geom_histogram(alpha = 0.5, position = 'identity')

# Hierarchical clustering using complete Linkage method
Hepta_complete <- hclust(Hepta_d_euclidian, method = "complete" )

# Hierarchical clustering using single Linkage method
Hepta_single <- hclust(Hepta_d_euclidian, method = "single" )


# Plot the obtained dendrogram
plot(Hepta_complete, cex = 0.6, hang = -1)
#Cut dendrogram
groups_complete <- cutree(Hepta_complete, k=length(unique(Hepta_class))) 
# Draw dendogram with red borders around the  clusters 
rect.hclust(Hepta_complete, k=length(unique(Hepta_class)), border="red")
