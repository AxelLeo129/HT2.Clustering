library(cluster) 
library(e1071)
library(mclust) 
library(fpc) 
library(NbClust) 
library(factoextra) 
setwd("C:/Users/LENOVO/Desktop/Clases/Minería de datos/HT2/HT2.Clustering")
datos <- read.csv("C:/Users/LENOVO/Desktop/Clases/Minería de datos/HT2/HT2.Clustering/data/tmdb-movies.csv")
#Quitamos los datos que no sirven: 
datos=datos[datos$budget > 1000000  ,]
datos = datos[datos$popularity != 0,]
datos = datos[datos$revenue > 1000000,]
datos= datos[datos$runtime > 30,]

#Para saber cual es el mejor numero de cluste
Variables_analizar = c("popularity", "budget", "revenue", "runtime" , "vote_count","vote_average")
wss <- (nrow(datos[,Variables_analizar])-1)*sum(apply(datos[,Variables_analizar],2,var))

for (i in 2:10) 
  wss[i] <- sum(kmeans(datos[,c("popularity", "budget" , "revenue" , "runtime" , "vote_count","vote_average")], centers=i)$withinss)

plot(1:10, wss, type="b", xlab="Number of Clusters",  ylab="Within groups sum of squares")

