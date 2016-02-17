#script du TD n?1, cours sde S?ries Temporelles 2015-16

# Lecture du fichier de donn?es "texte" unemp.dat avec read.table
# Donn?es mensuelles de ch?mage aux Etats-Unis chez les jeunes femmes
# de janvier 61 ? d?cembre 85 (= 25 ann?es d'historique)

unemptab <- read.table("/home/yassine/EMSE 2015-2016/Data Science/Séries Temporelles/unemp.dat")
unemp <- unemptab$V1

# Formatage avec ts (time-series object) et chronogramme

unemp <- ts(unemp, start = c(1961,1), freq = 12)
plot(unemp, type='l', xlab="Année", ylab="nombre en milliers",
     main="Chômage des femmes de 16 à 19 ans aux USA")


# Stationnarisation par différenciation simple et visualisation

diffunemp <- diff(unemp)
mu <- mean(diffunemp)
plot(diffunemp, type='o', main = expression(paste("Série différenciée ", Y[t])),
     xlab="Année", ylab = expression(paste("(",I-B,")",X[t])))
abline(0, 0, col="red", lwd=2)

# Visualisation des deux séries

op <- par(mfrow = c(2,1),mex=0.9)
plot(unemp, type='l', xlab="Année", ylab="nombre en milliers", 
     main=expression(paste("Série initiale ",X[t])))
plot(diffunemp, type='o', 
     main = expression( paste("Série différenciée ", Y[t]," = (",I-B,")",X[t]) ),
     xlab="Année", ylab = expression(paste("(",I-B,")",X[t])))
abline(0, 0, col="red", lwd=2)
par(op)

# Visualisation des auto-corrélations de la série différenciée 


n <- length(diffunemp)
k <- 4
m <- matrix(nr=n+k-1, nc=k)
colnames(m) <- c("y[t]", "y[t-1]", "y[t-2]","y[t-3]")
for (i in 1:k) {
  m[,i] <- c(rep(NA,i-1), diffunemp, rep(NA, k-i))
}

op <- par(mex=0.8)
pairs(m, gap=0, cex.labels=1.2, lower.panel=NULL, main="Visulaisation des auto-corr?lations")
par(op)

# ACF de la s?rie diff?renci?e Y

ro <- acf(diffunemp, lag=20, ylim = c(-1,1), 
main = expression(paste("ACF empirique de la s?rie ", Y[t])), xlab="Lag h (en mois)")

# ACF de la s?rie initiale

rounemp <- acf(unemp, lag=20, ylim = c(-1,1), 
main = expression(paste("ACF empirique de la s?rie initiale ", X[t])), xlab="Lag h (en mois)")

# bilan final

op <- par(mfrow = c(2,2))
plot(unemp, type='l', xlab="Ann?e", ylab="nombre en milliers", 
     main=expression(paste("S?rie initiale ",X[t])))
diffunemp <- diff(unemp)
plot(diffunemp, type='o', main = expression( paste(Y[t]," = (",I-B,")",X[t]) ),
     xlab="Ann?e", ylab = expression(paste("(",I-B,")",X[t])))
abline(0, 0, col="red", lwd=2)
ro <- acf(diffunemp, lag=20, ylim = c(-1,1), main = expression(paste("ACF de la s?rie ",Y[t])),
          xlab="Lag (en mois)")
alpha <- pacf(diffunemp, lag=20, ylim = c(-1,1), main = expression("et sa PACF"), 
              xlab="Lag (en mois)")
par(op)

# estimation du coeff AR d'ordre 1

rho = ro$acf[2]

# identification du MA(1)

theta <- (1 - sqrt(1 - 4*rho*rho) )/(2*rho)

# simulation d'un bruit de m?me variance que la s?rie diff?renci?e diffunemp et comparaison avec unemp

n <- length(diffunemp)
sigeps <- sd(diffunemp)

bruit <- sigeps*rnorm(n)
bruit <- ts(bruit, start = c(1961,1), freq = 12)

op <- par(mfrow = c(2,1))
plot(diffunemp, type='o', xlab="Ann?e", main = "S?rie diff?renci?e")
abline(0, 0, col="red", lwd=2)
plot(bruit, type='o', xlab="Ann?e", main = "Simulation d'un bruit de m?me variance")
abline(0, 0, col="red", lwd=2)
par(op)

bruit <- as.vector(bruit)
ro <- acf(bruit, lag=20, ylim = c(-1,1), main = "ACF d'un bruit", xlab="Lag (en mois)")

# simulation d'un MA(1) 

n <- length(diffunemp)
sigmaY <- sd(diffunemp)
sigma <- sigmaY/sqrt(1+theta^2)

bruit <- sigma*rnorm(n+1)
Ysim <- rep(0,n)
for (t in 1:n) Ysim[t] <- bruit[t+1] + theta*bruit[t]
Ysim <- ts(Ysim, start = c(1961,2), freq = 12)

op <- par(mfrow = c(2,1))
plot(diffunemp, type='o', xlab="Ann?e", main = "S?rie diff?renci?e")
abline(0, 0, col="red", lwd=2)
plot(Ysim, type='o', xlab="Ann?e", main = "MA(1) simul?")
abline(0, 0, col="red", lwd=2)
par(op)

roYsim <- acf(Ysim, lag=20, ylim = c(-1,1), main = "ACF du MA(1) simul?",
 xlab="Lag (en mois)")


# s?rie initiale en rouge et simulation avec un MA(1) en noir

plot(unemp, type='l', col='red', xlab="Ann?e", main = "S?rie initiale et MA(1) simul?",ylim=c(0,1000))
n <- length(unemp)

for (k in 1:1) {
  Xsim <- rep(0,n)
  Xsim[1] <- unemp[1]
  bruit <- sigma*rnorm(n)
  for (t in 2:n) Xsim[t] <-  Xsim[t-1] + mu + bruit[t] + theta*bruit[t-1] 
  Xsim <- ts(Xsim, start = c(1961,2), freq = 12)
  lines(Xsim)
}


# pr?diction du mod?le : on enl?ve les deux derni?res observations (nov. et d?c. 1985)
# que l'on cherche ensuite ? pr?voir ? partir de l'historique 01/1961 ? 10/1985, 
# soit au total nfit = 298 valeurs


nfit <- 298
unempfit <- unemp[1:nfit]
modele <- arima(unempfit,order = c(0,1,1))
print(modele)
tsdiag(modele)
residus.modele <- modele$residuals

op <- par(mfrow = c(1,2))
unempfit <- ts(unemp[1:298], start = c(1961,1), freq = 12)
plot(unempfit, type='l', xlab="Ann?e", ylab="nombre en milliers", main="Ch?mage jusqu'? oct. 85",
     ylim=c(280, 920), col='grey')
unempfit <- as.vector(unempfit)
unempend <- ts(unempfit[250:298], start=c(1981,10), freq=12)
lines(unempend, col='black')
unempfit <- as.vector(unempfit)
abline(unempfit[298], 0)


# pr?vision avec horizon = 2

prevision <- predict(modele, n.ahead=2, prediction.interval=T)
unempend <- ts(unemp[250:300], start=c(1981,10), freq=12)
plot(unempend,type='l', xlab='Ann?e', ylab='nombre en milliers', ylim=c(280, 920), 
     main="Pr?vision mois de nov. et d?c. 1985")
abline(unemp[298],0)
unempend <- ts(unemp[298:300], start=c(1985,10), freq=12)
lines(unempend, col='blue')
prev <- c(unempfit[298], prevision$pred)
prev <- ts(prev, start=c(1985,10), freq=12)
lines(prev, col='red')
prevsup <- c(unempfit[298], prevision$pred + 2*prevision$se)
prevsup <- ts(prevsup, start=c(1985,10), freq=12)
lines(prevsup, col='green')
previnf <- c(unempfit[298], prevision$pred - 2*prevision$se)
previnf <- ts(previnf, start=c(1985,10), freq=12)
lines(previnf, col='green')
par(op)

# pr?vision ? 4 ans

prevision <- predict(modele, n.ahead=48, prediction.interval=T)
unemptot <- ts(c(unemp[1:298], prevision$pred), start=c(1961,1), freq=12)
plot(unemptot,type='l', xlab='Ann?e', ylab='nombre en milliers', ylim=c(280, 920), 
     main="Pr?vision ? 4 ans")
prev <- c(unemp[298], prevision$pred)
prev <- ts(prev, start=c(1985,10), freq=12)
lines(prev, col='red')
prevsup <- c(unemp[298], prevision$pred + 2*prevision$se)
prevsup <- ts(prevsup, start=c(1985,10), freq=12)
lines(prevsup, col='green')
previnf <- c(unemp[298], prevision$pred - 2*prevision$se)
previnf <- ts(previnf, start=c(1985,10), freq=12)
lines(previnf, col='green')

# analyse de la variance d'erreur de pr?diction ? un pas en fontion de la longueur d'historique

varpred <- sigmaY^2
varn <- varpred
while (varn > 1.000001*sigma^2) {
  varn <- sigmaY^2 - ((rho*sigmaY^2)^2)/varn
  varpred <- c(varpred, varn) }




