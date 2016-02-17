# script TP n°2, Séries Temporelles, novembre 2015

# Partie 1 : Etude d'un AR(2) puis d'un MA(6)

# Etude d'un AR(2)
# Modèle X(t) - mu = phi1*(X(t-1) - mu) + phi2*(X(t-2) - mu) + Z(t)
# Z[t] bruit blanc gaussien N(0,varZ)
# simulation d'un AR(1) par une phase initiale de stationarisation

# inverses des racines du polynome PHI(z) = 1 - phi1*z - phi2*Z^2

# cas de deux racines réelles dans ]-1, 1[ : jouer sur les paramètres
r1 <- 0.3
r2 <- 0.5
phi1 <- r1 + r2  
phi2 <- - r1*r2			# paramètres AR(2) 

# cas de deux racines complexes conjuguées de module r < 1
r <- 0.7
angle <- 130 			# en degrés dans [0, 180]
phi1 <- 2*r*cos(angle*pi/180)	
phi2 <- - r*r			# paramètres AR(2) 

mu <- 0				# moyenne du processus X[t]
sigZ <- 1				# écart-type du bruit Z[t]

ninit <- 50
n <- 200
ntot <- ninit + n

xtot <- rep(0,ntot)
xtot[1] <- 0
xtot[2] <- 0
for (t in 3:ntot) xtot[t] <- phi1*xtot[t-1] + phi2*xtot[t-2] + sigZ*rnorm(1)

xtot <- mu + xtot
xinit <- xtot[1:ninit]
xstat <- xtot[(ninit+1):ntot]

plot(xtot, type='o', xlab="Temps t", main = "AR(2) simulé", col="grey")
lines((ninit+1):ntot, xstat, type='o')
abline(mu, 0, col="red", lwd=2)


# analyse graphique (chronogramme, acf)

op <- par(mfrow=c(2,1))
plot(xstat,type='l',xlab='time',ylab='')
abline(mu,0,col='red')
ro <- acf(xstat,10,main="ACF empirique",ylim=c(-1,1))
par(op)


# acf et pacf de la série simulée : formes ? commenter !
op <- par(mfrow = c(1,2))
ro <- acf(xstat, lag=15, ylim = c(-1,1), main = "ACF empirique")
alpha <- pacf(xstat, lag=15, ylim = c(-1,1), main = "et PACF", xlim=c(0,15))
par(op)


# Etude d'un MA(6) : modifier les param?tres

mu <- 0					# moyenne du processus X[t]
theta <- c(0.1, -0.4, 0.5, 0, 0.2, 1)  	# param?tre MA(6)
sigZ <- 1					# ?cart-type du bruit Z[t]


# simulation d'un MA(6) de taille n
n <- 400

x <- rep(0,n) # initialisation de la série x[t]

z <- sigZ*rnorm(n+6)     


for (t in 7:(n+6)) {
  x[t-6] <- mu + z[t] + theta[1]*z[t-1] + theta[2]*z[t-2] + theta[3]*z[t-3] + theta[4]*z[t-4] +
    theta[5]*z[t-5] + theta[6]*z[t-6]
}

# chronogramme de la série simulée
plot(x, type='o', xlab="Temps t", main = "MA(1) simul?")
abline(mu, 0, col="red", lwd=2)

# analyse graphique (chronogramme, acf)

op <- par(mfrow=c(2,1))
plot(x,type='l',xlab='time',ylab='')
abline(mu,0,col='red')
ro <- acf(x,10,main="ACF empirique",ylim=c(-1,1))
par(op)


# acf et pacf de la série simulée : ? commenter !
op <- par(mfrow = c(1,2))
ro <- acf(x, lag=15, ylim = c(-1,1), main = "ACF empirique")
alpha <- pacf(x, lag=15, ylim = c(-1,1), main = "et PACF", xlim=c(0,15))
par(op)


# Partie 2 :
# On étudie la série de trafic aérien international "airline" (série Airpassengers de R)
# C'est l'exemple 1 du poly : voir pages 4 et 5

# ********************************************************************************************************
# Partie exploratoire : exécuter le code pas à pas situé entre deux commentaires signalés par # 
# Il n'y a aucune instruction à compléter. Par contre, vous pouvez changer certains paramètres 
# et visualiser l'effet obtenu
# ********************************************************************************************************

# Chargement des données brutes et mise sous forme de série chronologique avec ts

airlinetab <- read.table("/home/yassine/EMSE 2015-2016/Data Science/Séries Temporelles/TP2/airline.dat")
airline <- airlinetab$V1
airline <- ts(airline, start = c(1949,1), freq = 12)

# Chronogramme et légendes...

plot(airline, type='o', xlab="Année",
ylab="nombre de passagers (milliers)", main="Trafic aérien international de janv. 1949 à déc. 1960")


# Méthodologie de Box&Jenkins, première transformation simple par
# Passage au logarithme et visualisation de l'effet obtenu

logair <- log(airline)
op <- par(mfrow = c(2,1))
plot(airline,type='o', main = "Série x initiale")
plot(logair, type='o', main = "log(x)")
par(op)

# On élimine la tendance (linéaire) par différenciation simple

difflogair <- diff(logair)
plot(difflogair, type='o', main = "Série log(x) différenciée",
xlab="Année", ylab = expression(paste("(",I-B,")",log(x[t])) ))
abline(0, 0, col="red", lwd=2)

# puis différenciation saisonnière (comparer la figure obtenue avec celle du poly page 13)
# pour éliminer la composante périodique de période s = 12 mois

diff2logair <- diff(difflogair, lag=12)
op <- par(cex.lab = 0.8)
plot(diff2logair, type='o', main = "Différenciation simple et différenciation saisonnière",
xlab="Année", ylab = expression(paste("(",I-B^12,")","(",I-B,")",log(x[t]))))
par(op)
abline(0, 0, col="red", lwd=2)

# ********************************************************************************************************
# On déroule la méthodologie de Box et Jenkins en partant de la série qui vient
# d'être obtenue par deux différenciations successives (série diff2logair). On l'analyse comme une
# série stationnaire à l'aide des ACF et PACF. On vérifie alors que le modèle SARIMA du poly semble bien 
# adapté (voir pages 37 et 38 pour l'expression de ce modèle). On estime ensuite ce modèle, on vérifie 
# que les coefficients sont bien ceux du poly et on valide graphiquement. Enfin, on utilise le modèle pour 
# faire de la prévision à un an et on visualise la qualité de prévision à un an par une technique de 
# back-testing (voir poly page 45, analyse post-sample)...
# ********************************************************************************************************

# on rebaptise la série obtenue (sans tendance et désaisonnalisée) 

airdts <- diff2logair

# ACF et PACF de airdts (comparer avec le poly page 38)

op <- par(mfrow = c(1,2))
airdts <- as.vector(airdts)
ro <- acf(airdts , lag=25, ylim = c(-1,1), main = expression("ACF série stationnarisée"), xlab="Lag (en mois)",
lwd=2)
alpha <- pacf(airdts , lag=25, ylim = c(-1,1), main = expression("PACF"), xlab="Lag (en mois)", lwd=2)
par(op)

# ajustement d'un modèle SARIMA(0,1,1)(0,1,1) avec saisonnalité s = 12 et comparaison des coefficients obtenus
# avec ceux du poly (voir page 38 pour l'expression complète du modèle et les valeurs des coeff)
# fonction "arima" de R

modele <- arima(logair, order = c(0,1,1), seasonal = list(order=c(0,1,1), period = 12))
print(modele)

# validation du modèle

# fonction "tsdiag" de R

tsdiag(modele)

# extraction des "résidus" du modèle (processus de bruit sous-jacent) et "normal qqplot"

residus <- modele$residuals
qqnorm(residus, main = "Normal Q-Q Plot", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       plot.it = TRUE)


# Prévision avec horizon h = 12 mois (prévision à un an de janvier 62 à décembre 62)
# L'intêrêt ici est de voir comment utliser la fonction "predict" du logiciel R 
# Attention, on prévoit le log de la série, prendre partout l'exponentielle pour revenir
# à des prévision sur la série initiale

logairplot <- ts(c(logair, rep(NA,12)), start = c(1949,1), freq = 12)
plot(logairplot, type='o', xlab='Année', ylab='Log du nombre (en milliers) de passagers',
     main ="Prévision SARIMA du trafic aérien sur un an", ylim=c(4.5,6.8))

prevision <- predict(modele, n.ahead=12, prediction.interval=T)

lines( ts(prevision$pred, start=c(1961,1), freq=12), type='o', col='red', lwd=2 )
lines( ts(prevision$pred + 2*prevision$se, start=c(1961,1), freq=12),  col='blue', lwd=2 )
lines( ts(prevision$pred - 2*prevision$se, start=c(1961,1), freq=12),  col='blue', lwd=2 )


# Back-testing du modèle SARIMA : on enlève les 12 dernières valeurs que l'on cherche ensuite
# à prévoir. On compare alors avec les valeurs réelles de la série!

nair <- length(airline)
airfit <- airline[1:(nair - 12)]	# on enlève les 12 dernières valeurs

logairfit <- ts(log(airfit), start = c(1949,1), freq = 12)

# ajustement d'un modèle SARIMA(0,1,1)(0,1,1) avec s = 12 sur la série tronquée logairfit

modele <- arima(logairfit, order = c(0,1,1), seasonal = list(order=c(0,1,1), period = 12))
print(modele)

tsdiag(modele)

residus <- modele$residuals
qqnorm(residus, main = "Normal Q-Q Plot", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       plot.it = TRUE)

# prévision avec horizon h = 12

logair <- log(airline)

plot(logair, type='o', xlab='Année', ylab='Log du nombre (en milliers) de passagers',
     main ="Pr?vision SARIMA du trafic aérien et valeurs réelles", ylim=c(4.5,6.8))

prevision <- predict(modele, n.ahead=12, prediction.interval=T)

lines( ts(prevision$pred, start=c(1960,1), freq=12), type='o', col='red', lwd=2 )
lines( ts(prevision$pred + 2*prevision$se, start=c(1960,1), freq=12),  col='blue', lwd=2 )
lines( ts(prevision$pred - 2*prevision$se, start=c(1960,1), freq=12),  col='blue', lwd=2 )

# ********************************************************************************************************
#  Travail à faire : le dernier graphique obtenu pour back-tester la méthode de prévision porte sur le
# logarithme de la série. Obtenir la même analyse graphique mais sur la série initiale. Pour cela,
# compléter le code suivant (les parties à compléter sont de la forme ... (3 points de suspension):
# ********************************************************************************************************


plot( airline , type='o', xlab='Année', ylab='Nombre (en milliers) de passagers',
     main ="Prévision SARIMA du trafic aérien et valeurs réelles", ylim=c(100,700))

prevision <- predict(modele, n.ahead=12, prediction.interval=T)

lines( ts( exp(prevision$pred), start=c(1960,1) , freq=12), type='o', col='red', lwd=2 )
lines( ts( exp(prevision$pred + 2*prevision$se), start=c(1960,1), freq=12),  col='blue', lwd=2 )
lines( ts( exp(prevision$pred - 2*prevision$se) , start=c(1960,1), freq=12),  col='blue', lwd=2 )


# ********************************************************************************************************
# On estime de manière paramètrique la tendance et saisonnalité du log de la série, et on traite 
# la série obtenue comme une série stationnaire. On travaille sur l'historique privé des 12 dernières
# valeurs pour tester à nouveau la qualité de la prévision obtenue.
# Compléter les ...
# ********************************************************************************************************

airlinetab <- read.table("/home/yassine/EMSE 2015-2016/Data Science/Séries Temporelles/TP2/airline.dat")
airline <- airlinetab$V1
airline <- ts(airline, start = c(1949,1), freq = 12)

logair <- log(airline)
nair <- length(logair)

# données pour estimer le modèle (on enléve les 12 dernières valeurs)

logairfit <- logair[1:(nair-12)]
logairfit <- ts(logairfit, start=c(1949,1), freq=12)

plot(logairfit, type='o', xlab="Année",
ylab="Log du nombre de passagers (milliers)", main="Trafic aérien international de janv. 1949 à déc. 1959")

# on estime tendance et saisonnalité à l'aide d'un modèle linéaire

logairfit <- as.vector(logairfit)

# prédicteurs avec predic1 qui est la variable temps

predic1 <- 1:(nair-12)
predic2 <- sin(2*pi*predic1/12)
predic3 <- cos(2*pi*predic1/12)

# On utise la fonction lm de R pour estimer le modèle linéaire

mod.lm <- lm( logairfit ~ predic1 + predic2 + predic3)

# on en déduit la tendance 

tend <- mod.lm$coef[1] + mod.lm$coef[2]*predic1
tend <- ts(tend, start=c(1949,1), freq=12)
lines(tend, col='red',lwd=2)

# on calcule la saisonnalité 

sais <- mod.lm$coef[3]*predic2 + mod.lm$coef[4]*predic3
sais <- ts(sais, start=c(1949,1), freq=12)
lines(tend+sais, col='blue',lwd=2)

# série log(airline) sans tendance et saisonnalité

logairdts <- logairfit - (tend + sais)
logairdts <- ts(logairdts, start=c(1949,1), freq=12)
op <- par(cex.lab = 0.8)
plot(logairdts, type='o', main = "Résidus estimés avec le modèle linéaire", xlab="Année")
par(op)
abline(0, 0, col="red", lwd=2)

# ACF et PACF de logairdts 

op <- par(mfrow = c(1,2))
logairdts <- as.vector(logairdts)
ro <- acf(logairdts , lag=25, ylim = c(-1,1), main = expression("ACF série stationnarisée"), xlab="Lag (en mois)", lwd=2)
alpha <- pacf(logairdts , lag=25, ylim = c(-1,1), main = expression("PACF"), xlab="Lag (en mois)", lwd=2)
par(op)


# ajustement d'un modèle SARIMA 

logairdts <- ts(logairdts, start=c(1949,1), freq=12)
logairdtsdiff6 <- diff(logairdts, lag=6)

op <- par(mfrow = c(1,2))
logairdtsdiff6 <- as.vector(logairdtsdiff6)
ro <- acf(logairdtsdiff6 , lag=25, ylim = c(-1,1), main = expression("ACF série stationnarisée"), xlab="Lag (en mois)", lwd=2)
alpha <- pacf(logairdtsdiff6 , lag=25, ylim = c(-1,1), main = expression("PACF"), xlab="Lag (en mois)", lwd=2)
par(op)


modele <- arima(logairdts, order = c(0,1,1), seasonal = list(order=c(0,1,1), period = 12))
print(modele)

tsdiag(modele)

residus <- modele$residuals
qqnorm(residus, main = "Normal Q-Q Plot", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       plot.it = TRUE)

# prévision de logairdts avec horizon h = 12 

plot(logair, type='o', xlab='Année', ylab='Log du nombre (en milliers) de passagers',
     main ="Prévision Modèle linéaire + SARIMA", ylim=c(4.5,6.8))

prevision <- predict(modele, n.ahead=12, prediction.interval=T)

# attention à tenir compte de la tendance et saisonnalité

prevlm <- as.vector(prevision$pred)
predic1 <- (nair-11):nair
predic2 <- sin(2*pi*predic1/12)
predic3 <- cos(2*pi*predic1/12)
tend <- mod.lm$coef[1] + mod.lm$coef[2]*predic1
sais <- mod.lm$coef[3]*predic2 + mod.lm$coef[4]*predic3

prevlm <- prevlm + tend + sais

lines( ts(prevlm, start=c(1960,1), freq=12), type='o', col='red', lwd=2 )
lines( ts(prevlm + 2*prevision$se, start=c(1960,1), freq=12),  col='blue', lwd=2 )
lines( ts(prevlm - 2*prevision$se, start=c(1960,1), freq=12),  col='blue', lwd=2 )


# ********************************************************************************************************
# Partie 3 : on utilise le package datasets de R 
#
# LakeHuron 		        Level of lake Huron 1875-1972 
# UKDriverDeaths	      Road Casualties in Great Britean 1969-84
# Nile			            Flow of the river Nile
# EuStockMarkets	      Daily Closing Prices of Major European Stock Indices, 1991-1998
# ETC.
#
# ********************************************************************************************************

# Exemple : décès sur les routes en GB de janv. 69 ? dec. 84 (192 donnees)
# A compléter

UKDD <- UKDriverDeaths
plot(UKDD,type = 'o')

UKDDdtrend <- diff(UKDD)
plot(UKDDdtrend,type = 'o')
UKDDdtrend <- diff(UKDDdtrend, lag=12)
plot(UKDDdtrend,type = 'o')

# ACF et PACF 

op <- par(mfrow = c(2,1))
ukdd <- as.vector(UKDDdtrend)
ro <- acf(ukdd , lag=25, ylim = c(-1,1), main = expression("ACF de la série stationnarisée"), xlab="Lag (en mois)",
lwd=2)
alpha <- pacf(ukdd , lag=25, ylim = c(-1,1), main = expression("PACF"), xlab="Lag (en mois)", lwd=2)
par(op)

# ajustement d'un modèle SARIMA(1,0,0)(1,0,0) avec s = 12 sur la série UKDD
modele <- arima(UKDD, order = c(1,0,0), seasonal = list(order=c(1,1,0), period = 12 ))
print(modele)
tsdiag(modele)
residus <- modele$residuals
qqnorm(residus, main = "Normal Q-Q Plot", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       plot.it = TRUE)
# back-testing de la prévision SARIMA : on enléve les 12 dernières valeurs que l'on cherche ensuite
# à prévoir. On compare alors avec les valeurs réelles de la série!

nukdd <- length(UKDD)
ukddfit <- UKDD[1:(nukdd - 12)]

# ajustement d'un modèle SARIMA(1,0,0)(1,1,0) avec s = 12 
modele <- arima(ukddfit, order = c(1,0,0), seasonal = list(order=c(1,1,0), period = 12 ))
print(modele)

tsdiag(modele)

residus <- modele$residuals

# prévision avec horizon h = 12

plot(UKDD, type='o', xlab='Année', ylab='Nombre de décès sur les routes en GB',
     main ="Prévision SARIMA et valeurs réelles", ylim = c(700,2600))

prevision <- predict(modele, n.ahead=12, prediction.interval=T)

lines( ts(prevision$pred, start=c(1984,1), freq=12), type='o', col='red', lwd=2 )
lines( ts(prevision$pred + 2*prevision$se, start=c(1984,1), freq=12),  col='blue', lwd=2 )
lines( ts(prevision$pred - 2*prevision$se, start=c(1984,1), freq=12),  col='blue', lwd=2 )

#An other model
x <- co2
plot(x, xlab='Année', main=' Carbon Dioxide Uptake in Grass Plants',)

#on remarque qu'il y a une tendance, on l'enlève
co2diff <- diff(x)
plot(co2diff)

# ACF et PACF 
op <- par(mfrow = c(3,1))
co2diff <- as.vector(co2diff)
ro <- acf(co2diff , lag=25, ylim = c(-1,1), main = expression("ACF de la série stationnarisée"), xlab="Lag (en mois)",
          lwd=2)
alpha <- pacf(co2diff , lag=25, ylim = c(-1,1), main = expression("PACF"), xlab="Lag (en mois)", lwd=2)
spectrum(co2diff)
par(op)

#on remarque qu'il y a une saisonalité, on différencie avec un lag égale à 12
co2diff <- diff(co2diff, lag = 12)

# ACF et PACF 
op <- par(mfrow = c(2,1))
co2diff <- as.vector(co2diff)
plot(co2diff, type = "l")
ro <- acf(co2diff , lag=25, ylim = c(-1,1), main = expression("ACF de la série stationnarisée"), xlab="Lag (en mois)",
          lwd=2)
alpha <- pacf(co2diff , lag=25, ylim = c(-1,1), main = expression("PACF"), xlab="Lag (en mois)", lwd=2)
spectrum(co2diff)
par(op)

#la différenciation pour enlever la tendance et la saisonnalité aurait pu être faite
#directement en utilisant diff(x, lag="12") dès le début

#back testing sur les 12 dernières années

nco2 <- length(x)
co2fit <- x[1:(nco2 - 12)]

#SARIMA(0,1,1)(0,1,1)
m1 <- arima(co2fit, order = c(0, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12))
#SARIMA(1,1,1)(2,1,1)
m2 <- arima(co2fit, order = c(1, 1, 1), seasonal = list(order = c(2, 1, 1), period = 12))
#SARIMA(1,1,2)(2,1,1)
m3 <- arima(co2fit, order = c(1, 1, 2), seasonal = list(order = c(2, 1, 1), period = 12))
#SARIMA(2,0,0)(1,1,0)
m4 <- arima(co2fit, order = c(2, 0, 0), seasonal = list(order = c(1, 1, 0), period = 12))

print(c(m1$aic,m2$aic,m3$aic,m4$aic))

tsdiag(m1)
residus <- m1$residuals
plot(residus)

# prévision avec horizon h = 12
p <- predict(modele, n.ahead=12)
plot(co2fit,type='l', xlab='Année', ylab=' Carbon Dioxide Uptake in Grass Plants',
     main ="Prévision SARIMA", ylim=range(c(co2,p$pred)))
lines(p$pred, col='red')
lines(p$pred+qnorm(.025)*p$se, col='blue', lty=2)
lines(p$pred+qnorm(.975)*p$se, col='blue', lty=2)

lines( ts(p$pred, start=c(1998,1), freq=12), type='l', col='red', lwd=2 )
lines( ts(p$pred + 2*p$se, start=c(1998,1), freq=12),  col='blue', lwd=2 )
lines( ts(p$pred - 2*p$se, start=c(1998,1), freq=12),  col='blue', lwd=2 )


