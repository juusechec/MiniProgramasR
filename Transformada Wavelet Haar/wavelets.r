library(wavelets)
k <- c(1,2,3,4)
w <- dwt(k, filter="haar")
w@V[[1]]
# http://www.bearcave.com/misl/misl_tech/wavelets/matrix/index.html
a <- c(1,1,0,0,
       1,-1,0,0,
       0,0,1,1,
       0,0,1,-1)
a <- matrix(a, nrow = 4, ncol = 4)
b <- matrix(k,nrow=4, ncol = 1)
x <- (1/sqrt(2)) * a %*% b
y <- c(x)
ai <- y[seq(1, 3, 2)]


#library(wavelets)
# https://stackoverflow.com/questions/19677229/r-haar-wavelet-transform
k <- c(4,6,10,12,8,6,5,5)
w <- dwt(k, filter="haar")
w@V[[1]]
# http://www.bearcave.com/misl/misl_tech/wavelets/matrix/index.html
a <- c(1,1,0,0,0,0,0,0,
       1,-1,0,0,0,0,0,0,
       0,0,1,1,0,0,0,0,
       0,0,1,-1,0,0,0,0,
       0,0,0,0,1,1,0,0,
       0,0,0,0,1,-1,0,0,
       0,0,0,0,0,0,1,1,
       0,0,0,0,0,0,1,-1)
a <- matrix(a, nrow = 8, ncol = 8)
b <- matrix(k,nrow=8, ncol = 1)
x <- (1/sqrt(2)) * a %*% b
y <- c(x)
ai <- y[seq(1, 7, 2)]


#library(wavelets)
# https://stackoverflow.com/questions/19677229/r-haar-wavelet-transform
# 40 muestras haar vs jorge
k <- rep(c(4,6,10,12,8,6,5,8),4)
w <- dwt(k, filter="haar")
bi <- c(w@V[[1]])
# http://www.bearcave.com/misl/misl_tech/wavelets/matrix/index.html
a <- c(1,1,0,0,0,0,0,0,
       1,-1,0,0,0,0,0,0,
       0,0,1,1,0,0,0,0,
       0,0,1,-1,0,0,0,0,
       0,0,0,0,1,1,0,0,
       0,0,0,0,1,-1,0,0,
       0,0,0,0,0,0,1,1,
       0,0,0,0,0,0,1,-1)
a <- matrix(a, nrow = 8, ncol = 8)
b <- matrix(k,nrow=8, ncol = length(k)/8)
x <- (1/sqrt(2)) * a %*% b
y <- c(x)
ai <- y[seq(1, length(y)-1, 2)]



# para 80 muestras
nivel = 80
k <- rep(c(4,6,10,12,8,6,5,5,23,1),8) # length 80
a <- c()
estado = FALSE
for (index in c(1:nivel)){
  estado = !estado
  arreglo = rep(0,nivel)
  myIndice = 0
  if (index %% 2 == 0){# par
    myIndice = index -1
  } else {
    myIndice = index
  }
  par = c()
  if (estado) {
    par = c(1,1)
  } else {
    par = c(1,-1)
  }
  iInf = myIndice-1
  iSup = myIndice+2
  #print(paste("Indice", iInf, iSup))
  #print(arreglo[0:iInf])
  #print(arreglo[iSup:length(arreglo)])
  if (iInf == 0){
    arreglo = c(par, arreglo[iSup:length(arreglo)])
  } else if (iSup == length(arreglo)+1){
    arreglo=c(arreglo[1:iInf], par)
  } else {
    arreglo=c(arreglo[1:iInf], par, arreglo[iSup:length(arreglo)])
  }
  #print(paste("Estado", estado))
  #print(paste("Par", par))
  #print(arreglo)
  #print(paste("Indice", index))
  a <- c(a,arreglo)
}
a <- matrix(a, nrow=nivel, ncol=nivel)
b <- matrix(k, nrow=nivel, ncol=1)
x <- (1/sqrt(2)) * a %*% b
y <- c(x)
ai <- y[seq(1, nivel, 2)]

