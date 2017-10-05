# para 80 muestras
## http://www.bearcave.com/misl/misl_tech/wavelets/matrix/index.html

# establece el directorio de trabajo
setwd("/home/jorge/Documents/TesisNoFallout")

# leer un archivo csv con todas las muestras
muestras=read.table("muestra_una_caida.csv", sep=",", header=FALSE, col.names = c("X","Y","Z"))
#muestras=read.table("muestra_un_caminando_lento.csv", sep=",", header=FALSE, col.names = c("X","Y","Z"))
#muestras=read.table("muestra_un_subir_escaleras.csv", sep=",", header=FALSE, col.names = c("X","Y","Z"))

# convierte los valores leidos a números
muestras$X=as.numeric(muestras$X)
muestras$Y=as.numeric(muestras$Y)
muestras$Z=as.numeric(muestras$Z)

# realiza un vector suma para trabajar un solo conjunto de coeficientes y no uno de cada eje
# simplifica el trabajo
vSuma=sqrt(((muestras$X)^2)+(muestras$Y)^2+(muestras$Z)^2)

# crea una funcion para obtener los coeficientes
getCoef <- function(vector) {
  nivel = length(vector)
  multiplo = (trunc(length(vector)/8))*8
  k <- c(vector[1:multiplo]) # length 80
  matrizT <- c(1,1,0,0,0,0,0,0,
               1,-1,0,0,0,0,0,0,
               0,0,1,1,0,0,0,0,
               0,0,1,-1,0,0,0,0,
               0,0,0,0,1,1,0,0,
               0,0,0,0,1,-1,0,0,
               0,0,0,0,0,0,1,1,
               0,0,0,0,0,0,1,-1)
  matrizT <- matrix(matrizT, nrow = 8, ncol = 8)
  matrizS <- matrix(k,nrow=8, ncol = length(k)/8)
  x <- (1/sqrt(2)) * matrizT %*% matrizS
  View(x)
  y <- c(x)
  ai <- y[seq(1, length(y), 2)]
  imp <- ai[10:20]
  ai
}

ai <- getCoef(vSuma)
print(ai)
plot(ai)

