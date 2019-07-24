###CORRECION TRABAJO PRACTICA ACP Y FACTORIAL ESTRUCTURA TEMPORAL SOBRE TIPOS DE INTERES### ###
###LO VA A COLGAR EN CANVAS###
library(readr)
library(factoextra)
library(FactoMineR)
#Cargamos las librerías pertinentes.
bonosUSA<-read.csv2("ACP_pract2.csv", header=TRUE,sep=";",dec=".")

TIUSD<-bonosUSA[2:10] #Para coger solo 9 variables en los 978 campos (He quitado la fecha y el IRS.10Y)
TIUSD.act<-bonosUSA[1:949, 2:10] #) variables en los 949 primeros campos
head(TIUSD)
tail(TIUSD) #Aquí se ve que los tipos a corto tienen intereses más altos que los a largo plazo, lo cual es paradojico en terminos macroeconómicos.

TIUSD
str(TIUSD.act)

Dates = as.Date(TIUSD.act$x, format="%d/%m/%y")

summary(TIUSD)


cor.mat=round(cor(TIUSD.act),2)
cor.mat #Aquí vemos correlaciones muy bajas en determinadas variables por lo que podrian ser independientes.
#(Dos variables son independientes y su correlación es 0 pero no por que sea 0 la cor, seran independientes)

library(Hmisc) #Instalamos paquete Hmisc que es un misceláneo
cor.mat.nds= rcorr(as.matrix(TIUSD.act))
cor.mat.nds
#Hipótesis> son independientes? Probabilidad de rechazar la Hipotesis es 0
#Como podemos observar, se generan 3 elementos de salida: la matriz de correlaciones,R;el número de observaciones; y los nds.
#En todo par de bonos hay una asociación. Por ejemplo IRS.5Y ~IRS.7Y tiene cor 1.00 y por atrás 0.99
require(corrplot)

corrplot(cor.mat, type = "lower", order = "original")

library(PerformanceAnalytics)

chart.Correlation(TIUSD.act, histogram = TRUE, pch = 19)
#Cuantas menos estrellas, menor es el p-valor

#La dispersión o var.cuadrática es la fuerza centrífuga en física

