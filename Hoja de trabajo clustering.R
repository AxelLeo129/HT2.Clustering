library(cluster) 
library(e1071)
library(mclust) 
library(fpc) 
library(NbClust) 
library(factoextra) 

setwd("C:/Users/LENOVO/Desktop/Clases/Miner�a de datos/HT2/HT2.Clustering")
#setwd("D:/AxelFolder/University/mineria_de_datos/HT2.Clustering")
datos <- read.csv("data/tmdb-movies.csv")

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

#k-means
datos_cluster <- datos[c("popularity", "budget" , "revenue" , "runtime" , "vote_count","vote_average")]
datos_cluster <- datos_cluster[complete.cases(datos),]
datos_agrupados <- kmeans(datos_cluster, 4, iter.max =100)
datos$grupo <- datos_agrupados$cluster

grupo1 <- datos[datos$grupo==1,]
prop.table(table(grupo1$vote_average))*100

grupo2 <- datos[datos$grupo==2,]
prop.table(table(grupo2$vote_average))*100

grupo3 <- datos[datos$grupo==3,]
prop.table(table(grupo3$vote_average))*100

grupo4 <- datos[datos$grupo==4,]
prop.table(table(grupo4$vote_average))*100

plotcluster(datos_cluster,km$cluster)

#Hierarchical clustering
hc<-hclust(dist(datos_cluster))
plot(hc)
rect.hclust(hc,k=4)
grupos<-cutree(hc,k=4)
datos$gruposHC<-grupos

grupo1_HC<-datos[datos$gruposHC==1,]
grupo2_HC<-datos[datos$gruposHC==2,]
grupo3_HC<-datos[datos$gruposHC==3,]
grupo4_HC<-datos[datos$gruposHC==4,]

#Fuzzy C-Means
fcm<-cmeans(datos_cluster,4)
datos$gruposFC<-fcm$cluster
datos<-cbind(datos,fcm$membership)
