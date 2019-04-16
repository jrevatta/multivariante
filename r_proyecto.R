library(foreign)
billetes1<-read.spss("e:/multi/billetes.sav")
billete1<-data.frame(billetes1)

# x1: Longitud del billete de banco
# x2: altura del billete medido a la izquierda
# x3: Altura del billete de banco medido a la derecha
# x4: distancia del marco interno al borde inferior
# x5: distancia del marco interno al borde superior
# x6: longitud de la diagonal
# cualitativa: situacion del billete.


media <-colMeans(billete1[1:6])
media
covara <-cov(billete1[1:6])
covara
correla <-cor(billete1[1:6])
correla

mediaA <-colMeans(subset(billete1, X7 == "verdadero")[1:6])
mediaA
covaA <-cov(subset(billete1, X7 == "verdadero")[1:6])
covaA
correlaA <-cor(subset(billete1, X7 == "verdadero")[1:6])
correlaA

mediaB <-colMeans(subset(billete1, X7 == "falso")[1:6])
mediaB
covaB <-cov(subset(billete1, X7 == "falso")[1:6])
covaB
correlaB <-cor(subset(billete1, X7 == "falso")[1:6])
correlaB


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
muestra <- sample(1:nrow(billete1), size = n, replace = FALSE)
dmuestra <-billete1[muestra,]
qqnorm(dmuestra$X1)
qqline(dmuestra$X1, col='red')

#muestra 2
n <- 15
muestra <- sample(1:nrow(billete1), size = n, replace = FALSE)
dmuestra <-billete1[muestra,]
dmuestra1 <-as.data.frame(scale(billete1[1:6]))
qqnorm(dmuestra1$X1)
qqline(dmuestra1$X1, col='blue')

#muestra 3
n <- 15
muestra <- sample(1:nrow(billete1), size = n, replace = FALSE)
dmuestra <-billete1[muestra,]
qqnorm(dmuestra$X1)
qqline(dmuestra$X1, col='green')