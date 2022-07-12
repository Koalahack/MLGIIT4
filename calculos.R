# 1. Paquete #####

library(MPV)
library(car)
library(MASS)
library(pROC)

library(ggplot2)
library(gridExtra)
library(plotly)

# 2. Datos ####

data =  read.csv("AbusoDrogas.txt", sep="")

data = data[,c(2,3,5,7,9)]
head(data)

## 2.1. Visualización de datos ####



# 3. Modelo logístico ####

# Enlace Logit
mlg1.logit = glm(DFREE~AGE + BECK + NDRUGTX + TREAT,family = binomial("logit"),data)
summary(mlg1.logit)

# Interpretación de los coeficientes
exp(mlg1.logit$coefficients)

## 3.1. Modelo logístico con interacciones ####

# Enlace Logit e interacciones (punto2)
mlg2.logit = glm(DFREE~AGE + BECK + NDRUGTX +AGE*TREAT + BECK*TREAT + NDRUGTX*TREAT + TREAT,family = binomial("logit"),data)
summary(mlg2.logit)

# Interpretación de los modelos
exp(mlg2.logit$coefficients)

## 3.2. Modelos con otras funciones ####

# (punto3)

# Enlace probit
mlg3.probit = glm(DFREE~AGE + BECK + NDRUGTX + TREAT,family = binomial("probit"),data)
summary(mlg3.probit)

# Enlace log complementaria
mlg4.logcom = glm(DFREE~AGE + BECK + NDRUGTX + TREAT,family = binomial("cloglog"),data)
summary(mlg4.logcom)

# Enlace cauchit
mlg5.cauchit = glm(DFREE~AGE + BECK + NDRUGTX + TREAT,family = binomial("cauchit"),data)
summary(mlg5.cauchit)

# Comparación de con curva rog

# curvas roc
ROCbw.logit = roc(data$DFREE~mlg1.logit$fitted.values)
ROCbw.probit = roc(data$DFREE~mlg3.probit$fitted.values)
ROCbw.cloglog = roc(data$DFREE~mlg4.logcom$fitted.values)
ROCbw.cauchit = roc(data$DFREE~mlg5.cauchit$fitted.values)
plot(ROCbw.logit)
lines(ROCbw.probit,col=2)
lines(ROCbw.cloglog,col=3)
lines(ROCbw.cauchit,col=4)




