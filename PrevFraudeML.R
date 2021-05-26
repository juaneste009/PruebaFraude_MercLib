### Prueba MercadoLibre
### Prevención de fraude


### Cambiar directorio de la sesión

### Instalar paquetes
#install.packages('caret',dependencies=c('Depends','Suggests'))
#install.packages('missForest')

### Librerías requeridas
library(caret)
#library(missForest)

### Lectura de la base de datos ###
data<-read.csv('Fraud Dataset.csv')
sapply(data,class)
head(data)

# Es necesario corregir tipos de datos
# Char a num
data$Q<-as.numeric(gsub(',','',data$Q))
data$R<-as.numeric(gsub(',','',data$R))
data$Monto<-as.numeric(gsub(',','',data$Monto))
# Char a factor
data$Fraude<-as.factor(data$Fraude)
data$J<-as.factor(data$J)
head(data)
str(data)


### Validación de datos faltantes
table(is.na(data))
# % por variable
apply(is.na(data),2,mean)
# Total por variables
apply(is.na(data),2,sum)

### Tratamiento datos faltantes
# Dado que no se conoce el detalle del porque faltan estos datos
# ni la naturaleza del campo, es difícil decidir realizar una
# imputación, por tanto lo mejor es eliminar ambas variables
data<-data[,c(-3,-11)]
# En caso de imputar datos, se usaba este pedazo de código
# aplicando bosques aleatorios
#data_imp<-missForest(data,maxiter=5,ntree=30)
#summary(data$C)
#data_imp$OOBerror
#data<-data_imp$ximp
# De nuevo validamos NA
table(is.na(data))


### Análisis exploratorio de los datos
# Distribución de la variables respuesta
(table(data$Fraude)/dim(data)[1])*100

# Distribución variable cat
(table(data$J)/dim(data)[1])*100

# Resumen de los datos
summary(data)

# Histograma variables num
num1<-data[,1:8]
(n1<-dim(num1)[2])
par(mfrow=c(2,4))
for(i in 1:n1){
    hist(num1[,i],main=names(num1)[i],xlab='',ylab='Freq')
}
num2<-data[,10:18]
(n2<-dim(num2)[2])
par(mfrow=c(2,5))
for(i in 1:n2){
    hist(num2[,i],main=names(num2)[i],xlab='',ylab='Freq')
}

# Boxplot variables num
par(mfrow=c(2,4))
for(i in 1:n1){
    boxplot(num1[,i],main=names(num1)[i])
}
par(mfrow=c(2,5))
for(i in 1:n2){
    boxplot(num2[,i],main=names(num2)[i],xlab='',ylab='Freq')
}

### Análisis multivariado
# Boxplot varibales num vs respuesta
par(mfrow=c(2,4))
for(i in 1:n1){
    boxplot(num1[,i]~data$Fraude,main=names(num1)[i],outline=F)
}
par(mfrow=c(2,5))
for(i in 1:n2){
    boxplot(num2[,i]~data$Fraude,main=names(num2)[i],outline=F)
}

# Misma gráfica sin considerar outliers
par(mfrow=c(2,4))
for(i in 1:n1){
    boxplot(num1[,i]~data$Fraude,main=names(num1)[i],outline=F)
}
par(mfrow=c(2,5))
for(i in 1:n2){
    boxplot(num2[,i]~data$Fraude,main=names(num2)[i],outline=F)
}

# Gráficos de densidad
par(mfrow=c(2,4))
for(i in 1:n1){
    x<-num1[,i][data$Fraude=='1']
    y<-num1[,i][data$Fraude=='0']
    plot(density(x),main=names(num1)[i],ylab='',xlab='',col='red')
    lines(density(y),col='blue')
}
par(mfrow=c(2,5))
for(i in 1:n2){
    x<-num2[,i][data$Fraude=='1']
    y<-num2[,i][data$Fraude=='0']
    plot(density(x),main=names(num2)[i],ylab='',xlab='',col='red')
    lines(density(y),col='blue')
}

# Variable cat vs variable respuesta
addmargins(table(data$J,data$Fraude))
round(100*prop.table(table(data$J,data$Fraude),1),digits=0)
# Vale la pena recodificar esta variable para agrupar
# los niveles sin fraude
J<-as.character(data$J)
J_nom<-names(table(J)[table(J,data$Fraude)[,2]==0])
j<-length(J_nom)
for(i in 1:j){
    J[J==J_nom[i]]<-'OT'
}
data$J<-as.factor(J)
# Analisemos de nuevo la variable tratada
addmargins(table(data$J,data$Fraude))
round(100*prop.table(table(data$J,data$Fraude),1),digits=0)


### Creación base de datos construcción y prueba
set.seed(1601)
muestra<-createDataPartition(data$Fraude,p=0.8,list=F)
prueba<-data[-muestra,]
data<-data[muestra,]


### Selección de variables para el modelo
# Creación fórmula
varx<-paste(names(data)[c(-18,-19)],collapse='+')
(fm=as.formula(paste(names(data)[19],varx,sep='~')))
# Modelo para validar importancia de variables
fraude_rf<-randomForest(fm,data=data,importance=T,ntree=100)
# Gráfico
varImpPlot(fraude_rf)
# Tomando como referencia el análisis exploratorio
# y la selección de variables usando bosques aleatorios
# se escogen las variables: B,J,P,S


### Ajuste de modelos
# Definición tipo de validación
ctrl<-trainControl(method='cv',number=10)
mtr<-'Accuracy'
# Creación de la fórmula
varx<-paste(names(data)[c(2,9,14,17)],collapse='+')
(fm<-as.formula(paste(names(data)[19],varx,sep='~')))
# Arboles de regresión y clasificación
set.seed(1601)
fit.cart<-train(fm,data=data,method='rpart',metric=mtr,trControl=ctrl)
# K-nearest neighbors
set.seed(1601)
fit.knn<-train(fm,data=data,method='knn',metric=mtr,trControl=ctrl)
# Bosque aleatorio
set.seed(1601)
fit.rf<-train(fm,data=data,method='rf',metric=mtr,trControl=ctrl)


### Comparación de los modelos ajustados
res<-resamples(list(cart=fit.cart,knn=fit.knn,rf=fit.rf))
summary(res)
# Gráfica
dotplot(res)
# El bosque aleatorio es el modelo que interpreta mejor los datos
# Vamos a evaluar en detalle  el modelo elegido
pred.rf<-predict(fit.rf,prueba)
confusionMatrix(pred.rf,prueba$Fraude)


### Ajustamos una regresión logítica
fit.lg<-glm(fm,data=data,family='binomial')
# Evaluémos el modelo de regresión logística
summary(fit.lg)
pred.lg<-as.factor(ifelse(fit.lg$fitted.values>0.5,1,0))
confusionMatrix(pred.lg,fit.lg$model$Fraude)

# Realicemos la prueba con el otro conjunto de datos
pred.lg.1<-predict(fit.lg,prueba,type='response')
pred.lg.2<-as.factor(ifelse(pred.lg.1>0.5,1,0))
confusionMatrix(pred.lg.2,prueba$Fraude)
# Ambos modelos son muy buenos para detectar transacciones no
# fraudulentas (sensibilidad) y no tienen buena capacidad para
# identificar transacciona fraudulentas(especificidad),
# es importante evaluar en terminos de perdidas y ganancias


### Validación de ambos modelos en termino de perdidas
prueba$Monto.RF<-ifelse(pred.rf=='1',prueba$Monto*(-1),prueba$Monto*0.25)
prueba$Monto.LG<-ifelse(pred.lg.2=='1',prueba$Monto*(-1),prueba$Monto*0.25)
prueba$Monto.Real<-ifelse(prueba$Fraude=='1',prueba$Monto*(-1),prueba$Monto*0.25)
colSums(prueba[,20:22])
# En terminos monetarios el mejor modelo es la regresión logística