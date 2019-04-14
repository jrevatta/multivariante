library(foreign)
gorriones1<-read.spss("D:/estadistica/rprojects/gorriones/datos_gorriones.sav")
gorriones1<-data.frame(gorriones1)
print(gorriones1)
media <-colMeans(gorriones1[1:5])
media
covara <-cov(gorriones1[1:5])
covara
correla <-cor(gorriones1[1:5])
correla
mediaA <-colMeans(subset(gorriones1, sobrevi == "sobrevivió")[1:5])
mediaA
covaA <-cov(subset(gorriones1, sobrevi == "sobrevivió")[1:5])
covaA
mediaB <-colMeans(subset(gorriones1, sobrevi == "murió")[1:5])
mediaB
covaB <-cov(subset(gorriones1, sobrevi == "murió")[1:5])
covaB
estanda <-as.data.frame(scale(gorriones1[1:5]))
dista.gorriones1 <- dist(estanda)
dista.gorriones1
mahalan <- mahalanobis(gorriones1[1:5], colMeans(gorriones1[1:5]), cov(gorriones1[1:5]))
mahalan
dismahalann <-sqrt(mahalan)
order(dismahalann)
ordirmahala <- order(mahalanobis(gorriones1[1:5], colMeans(gorriones1[1:5]), cov(gorriones1[1:5])), decreasing = TRUE)
ordirmahala
covaA
tr<-sum(diag(covaA))
gene<-det(covaA)
gene
covaA
tr<-sum(diag(covaA))
tr
boxplot(gorriones1[,1], xlab="Longitud total del gorrión", ylab="Numero de gorriones")
boxplot(gorriones1[,2], xlab="Largo del ala del gorrion", ylab="Numero de gorriones")
boxplot(gorriones1[,3], xlab="Largo pico y cabeza", ylab="Numero de gorriones")
boxplot(gorriones1[,4]~gorriones1[,6],xlab="Largo del húmero", ylab="Numero de gorriones")
plot(gorriones1$x5, gorriones1$x1)
library("car")
library("carData")
scatterplotMatrix(gorriones1[1:5])
mahalazz<-mahalanobis(gorriones1[1:5], colMeans(gorriones1[1:5]), cov(gorriones1[1:5]))
boxplot(mahalazz, ylab="distancias de Mahalanobis")
dismahalann<-matrix(dismahalann)
boxplot(dismahalann~gorriones1[,6], xlab="Situación de sobrevivencia", ylab="Distancia de mahalanobis")






