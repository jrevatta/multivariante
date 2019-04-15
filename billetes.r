library(foreign)
billetes1<-read.spss("D:/estadistica/rprojects/gorriones/billetes.sav")
billetes1<-data.frame(billetes1)
media <-colMeans(billete1[1:6])
covara <-cov(billete1[1:6])
correla <-cor(billete1[1:6])
mediaA <-colMeans(subset(billete1, X7 == "verdadero")[1:6])
covaA <-cov(subset(billete1, X7 == "verdadero")[1:6])
mediaB <-colMeans(subset(billete1, X7 == "falso")[1:6])
covaB <-cov(subset(billete1, X7 == "falso")[1:6])
estanda <-as.data.frame(scale(billete1[1:6]))
dista.billete1 <-dist(estanda)
mahalan <- mahalanobis(billete1[1:6], colMeans(billete1[1:6]), cov(billete1[1:6]))
dismahalann <-sqrt(mahalan)
order(dismahalann)
ordirmahala <- order(mahalanobis(billete1[1:6], colMeans(billete1[1:6]), cov(billete1[1:6])), decreasing = TRUE)
tr<-sum(diag(covaA))
gene<-det(covaA)

boxplot(billete1[,1], xlab="X1", ylab="Numero de billetes")
boxplot(billete1[,2], xlab="X2", ylab="Numero de billetes")
boxplot(billete1[,3], xlab="X3", ylab="Numero de billetes")

boxplot(billete1[,4]~billete1[,6],xlab="X4", ylab="X6")

boxplot(billete1[,5]~billete1[,6],xlab="X5", ylab="X6")

#plot(billete1$x6, billete1$x1)
library("car")
library("carData")
scatterplotMatrix(billete1[1:6])
mahalazz<-mahalanobis(billete1[1:6], colMeans(billete1[1:6]), cov(billete1[1:6]))
boxplot(mahalazz, ylab="distancias de Mahalanobis")
dismahalann<-matrix(dismahalann)
boxplot(dismahalann~billete1[,6], xlab="Situación veracidad", ylab="Distancia de mahalanobis")

#muestra 1
n <- 15
muestra <- sample(1:nrow(billetes1), size = n, replace = FALSE)
dmuestra <-billetes1[muestra,]
qqnorm(dmuestra$X1)
qqline(dmuestra$X1, col='red')

#muestra 2
n <- 15
muestra <- sample(1:nrow(billetes1), size = n, replace = FALSE)
dmuestra <-billetes1[muestra,]
qqnorm(dmuestra$X1)
qqline(dmuestra$X1, col='blue')

#muestra 3
n <- 15
muestra <- sample(1:nrow(billetes1), size = n, replace = FALSE)
dmuestra <-billetes1[muestra,]
qqnorm(dmuestra$X1)
qqline(dmuestra$X1, col='green')

