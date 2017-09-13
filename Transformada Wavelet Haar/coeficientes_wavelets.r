# para 80 muestras
## http://www.bearcave.com/misl/misl_tech/wavelets/matrix/index.html

#muestras=read.table("muestra_una_caida.csv", sep=",", header=FALSE, col.names = c("X","Y","Z"))
#muestras=read.table("muestra_un_caminando_lento.csv", sep=",", header=FALSE, col.names = c("X","Y","Z"))
muestras=read.table("muestra_un_subir_escaleras.csv", sep=",", header=FALSE, col.names = c("X","Y","Z"))
muestras$X=as.numeric(muestras$X)
muestras$Y=as.numeric(muestras$Y)
muestras$Z=as.numeric(muestras$Z)
vSuma=sqrt(((muestras$X)^2)+(muestras$Y)^2+(muestras$Z)^2)

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
  y <- c(x)
  ai <- y[seq(1, length(y), 2)]
  imp <- ai[10:20]
  ai
}

ai <- getCoef(vSuma)
plot(ai)

