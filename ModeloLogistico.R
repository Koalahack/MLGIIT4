library(faraway)


### datos de escarabajos
logdose <- c(1.6907, 1.7242, 1.7552, 1.7842, 1.8113, 1.8369, 1.8610, 1.8839)
dead <- c(6, 13, 18, 28, 52, 53, 61, 60) # numbers dead
n <- c(59, 60, 62, 56, 63, 59, 62, 60) # binomial sample sizes
plot(logdose,dead/n,xlab='log dosis',ylab='proporción de muertos',ylim=c(0,1))
# ajuste del modelo
modBin = glm(cbind(dead,n-dead)~logdose,family=binomial)
summary(modBin)

# devianza
D = deviance(modBin)
1-pchisq(D,6) # valor p
# chi-cuadrado de Pearson
X2 = sum(residuals(modBin,type='pearson')^2)
1-pchisq(X2,6) # valor p

## intervalos de confianza
# para beta
confint(modBin)
# para la media
pred.x = data.frame(logdose = seq(min(logdose),max(logdose),length.out = 50))
pred = predict(modBin,pred.x,type='response')

pred.link = predict(modBin,newdata = pred.x,type='link',se.fit = T)

lim.sup = ilogit(pred.link$fit + qnorm(0.975)*pred.link$se.fit)
lim.inf = ilogit(pred.link$fit - qnorm(0.975)*pred.link$se.fit)

plot(logdose,dead/n,xlab='log dosis',ylab='proporción de muertos',ylim=c(0,1))
lines(pred.x$logdose,pred)


lines(pred.x$logdose,lim.sup, lty=2)
lines(pred.x$logdose,lim.inf, lty=2)

# comparación de funciones de enlace
modEsc.logit = glm(cbind(dead,n-dead)~logdose,family=binomial(logit))
modEsc.probit = glm(cbind(dead,n-dead)~logdose,family=binomial(probit))
modEsc.cloglog = glm(cbind(dead,n-dead)~logdose,family=binomial(cloglog))
modEsc.cauchit = glm(cbind(dead,n-dead)~logdose,family=binomial(cauchit))

AIC(modEsc.logit)
AIC(modEsc.probit)
AIC(modEsc.cloglog)
AIC(modEsc.cauchit)

# curva ROC
library(pROC)
roc(y, modEsc.cloglog2$fitted.values, plot = TRUE, legacy.axes = TRUE,
    percent = TRUE, lwd = 2, print.auc = TRUE)

## datos de peso al nacer
library(MASS)
data(birthwt)
range(birthwt$age)
modbw.logit = glm(low~age+lwt+ptl+smoke,family=binomial(logit),data=birthwt)
summary(modbw.logit)

# ajustes con funciones de enlace diferentes
modbw.logit = glm(low~age+lwt+smoke+ptl,family=binomial(logit),data=birthwt)
modbw.probit = glm(low~age+lwt+smoke+ptl,family=binomial(probit),data=birthwt)
modbw.cloglog = glm(low~age+lwt+smoke+ptl,family=binomial(cloglog),data=birthwt)

# curvas roc
ROCbw.logit = roc(birthwt$low~modbw.logit$fitted.values)
ROCbw.probit = roc(birthwt$low~modbw.probit$fitted.values)
ROCbw.cloglog = roc(birthwt$low~modbw.cloglog$fitted.values)
plot(ROCbw.logit)
lines(ROCbw.probit,col=2)
lines(ROCbw.cloglog,col=3)

## datos en teratología
data(lirat,package = 'VGAM')
modlirat.binom = glm(cbind(R,N-R)~hb+as.factor(grp),family=binomial,data=lirat)
summary(modlirat.binom)
# estadístico chi cuadrado de Pearson
X2 = sum(residuals(modlirat.binom,type='pearson')^2)
1-pchisq(X2,53)
X2/53
# devianza
D = deviance(modlirat.binom)
1-pchisq(D,53)

# modelo beta-binomial
library(aod)
modlirat.betabinom = betabin(cbind(R,N-R)~as.factor(grp)+hb,data=lirat,random=~1)

