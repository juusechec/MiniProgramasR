require(rpart)
require(rpart.plot)
require(car)
require(caret)

# establece el directorio de trabajo
setwd("/home/jorge/Documents/TesisNoFallout")
# lee los datos del archivo csv y los guarda en wdbc, indica que se 
# separa por ',' y que no tiene header (pone como header V1, V2, V3, ..., V#)
wdbc=read.table("datos_caidas.csv", sep=",", header=TRUE)

# dimensión del vector
dim(wdbc)

# reemplaza los '?' por 'NA' como numérico
#wdbc$V7=as.numeric(recode(wdbc$V7, "'?'=NA"))

# solo para pruebas, deja solo dos valores para clasificar
wdbc$TIPO=as.numeric(recode(wdbc$TIPO, "1=1"))
wdbc$TIPO=as.numeric(recode(wdbc$TIPO, "2=1"))
wdbc$TIPO=as.numeric(recode(wdbc$TIPO, "3=1"))
wdbc$TIPO=as.numeric(recode(wdbc$TIPO, "5=1"))
wdbc$TIPO=as.numeric(recode(wdbc$TIPO, "6=1"))
wdbc$TIPO=as.numeric(recode(wdbc$TIPO, "7=1"))
wdbc$TIPO=as.numeric(recode(wdbc$TIPO, "8=1"))
wdbc$TIPO=as.numeric(recode(wdbc$TIPO, "9=1"))

# guarda en 'x' los datos que NO tienen datos incompletos. Columnas con NA
#x=wdbc[complete.cases(wdbc),]
x=wdbc # solo si todos están completos

# dimensión del vector
dim(x)

# entrenamiento (2/3), pruebas (1/3)
N=dim(x)[1]
# hace un vector con la longitud de 'x'
all=seq(1,N)
# selecciona 2/3 de las muestras randomly aleatoriamente
train=sort(sample(N,N*2/3.0))
# lo que esta en el conjunto 'a' que no esta en el 'b'
# o sea, selecciona los índices que no están en 'train'
# que son los indices de entrenamiento
test=setdiff(all,train) # indices para vector de prueba

xtrain=x[train,]
xtest=x[test,]

# crear primero el arbol usando parametros predeterminados
t0=rpart(as.factor(TIPO) ~ MAXIMO_X+MAXIMO_Y+MAXIMO_Z+MINIMO_X+MINIMO_Y+MINIMO_Z+MEDIA_X+MEDIA_Y+MEDIA_Z+MODA_X+MODA_Y+MODA_Z+MEDIANA_X+MEDIANA_Y+MEDIANA_Z+VARIANZA_X+VARIANZA_Y+VARIANZA_Z,
         data = xtrain, method = "class",
         control = rpart.control(minsplit = 0, cp = 0.0))
prp(t0, extra = 1)
y1train=predict(t0, xtrain, type = "class")
y1test=predict(t0, xtest, type = "class")
# analizar que tan bien es t0 como un clasificador
confusionMatrix(table(xtrain$TIPO,y1train))
confusionMatrix(table(xtest$TIPO,y1test))

t1=rpart(as.factor(TIPO) ~ MAXIMO_X+MAXIMO_Y+MAXIMO_Z+MINIMO_X+MINIMO_Y+MINIMO_Z+MEDIA_X+MEDIA_Y+MEDIA_Z+MODA_X+MODA_Y+MODA_Z+MEDIANA_X+MEDIANA_Y+MEDIANA_Z+VARIANZA_X+VARIANZA_Y+VARIANZA_Z,
         data = xtrain, method = "class")
# la gráfica se lee como
#(SI es menor)  [ V3 < 3.5 ] (NO es menor) # indica qué rama es SI y cuál NO en todas las comparaciones
#    [    2   ]        [    4   ]  # los que están clasificados como 2 o 4 según el criterio anterior
#    [ 285 8  ]        [ 7 138  ]  # 285 están bien clasificados como "2" y 8 son mal clasificados
prp(t1, extra = 1)
y1train=predict(t1, xtrain, type = "class")
y1test=predict(t1, xtest, type = "class")

# analizar que tan bien es t1 como un clasificador
confusionMatrix(table(xtrain$V11,y1train))
confusionMatrix(table(xtest$V11,y1test))

# calcular PCA para las muestras de entrenamiento
pca=princomp(xtrain[,2:10])
summary(pca)
pca$loadings[,1]

# a~nadir una nueva variable con el resultado del pca
xtrain$C1=pca$scores[,1]
xtest$C1=predict(pca,xtest)[,1]

# crear el arbol t2 usando esta variable
t2=rpart(as.factor(V11) ~ V2+V3+V4+V5+V6+V7+V8+V9+V10+C1,
         data = xtrain, method = "class")

prp(t2, extra = 1)
y2train=predict(t2, xtrain, type = "class")
y2test=predict(t2, xtest, type = "class")

confusionMatrix(table(xtrain$V11,y2train))
confusionMatrix(table(xtest$V11,y2test))

# NO se presenta overfitin

# forzar a completar un arbol puro t3 (sin ningun error)
t3=rpart(as.factor(V11) ~ V2+V3+V4+V5+V6+V7+V8+V9+V10+C1,
         data = xtrain, method = "class",
         control = rpart.control(minsplit = 0, cp = 0.0))
#control point CP es cero

prp(t3, extra = 1)
y3train=predict(t3, xtrain, type = "class")
y3test=predict(t3, xtest, type = "class")

confusionMatrix(table(xtrain$V11,y3train))
confusionMatrix(table(xtest$V11,y3test))
# min 18:47

