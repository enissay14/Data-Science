
time(AirPassengers)
cycle(AirPassengers)

diffAir <- diff(AirPassengers) 

#differienciated series
op <- par(mfrow = c(2,1),mex=0.9)
plot(AirPassengers, type='l', xlab="Ann??e",ylab="Passengers", 
     main=expression(paste("S??rie initiale ",X[t])))
plot(diffAir, type='o', 
     main = expression( paste("S??rie diff??renci??e ", Y[t]," = (",I-B,")",X[t]) ),
     xlab="Ann??e", ylab = expression(paste("(",I-B,")",X[t])))
abline(0, 0, col="red", lwd=2)
par(op)

# Seasonal decomposition
fit <- stl(AirPassengers, s.window="period")
plot(fit)

# additional plots
monthplot(AirPassengers)
library(forecast)
seasonplot(AirPassengers)

#filtres lin??aires
plot(AirPassengers,type="l")
AirPassengers.2 <- filter(AirPassengers,filter=rep(1/5,5))
AirPassengers.12 <- filter(AirPassengers,filter=rep(1/25,25))
AirPassengers.40 <- filter(AirPassengers,filter=rep(1/81,81))
lines(AirPassengers.12,col="red")
lines(AirPassengers.2,col="purple")
lines(AirPassengers.40,col="blue")
lines(lowess(AirPassengers), col="blue", lty="dashed")

dlAirPassengers <- diff(log(AirPassengers))
plot(dlAirPassengers)
shapiro.test(dlAirPassengers)
par(mfrow=c(2,1))        
hist(dlAirPassengers, prob=TRUE, 12)   # histogram    
lines(density(dlAirPassengers))     # smooth it - ?density for details 
qqnorm(dlAirPassengers)             # normal Q-Q plot  
qqline(dlAirPassengers)             # add a line 

lag.plot(dlAirPassengers, 12)
  
par(mfrow=c(3,1))
# simple exponential - models level
fit.1 <- HoltWinters(AirPassengers, beta=FALSE, gamma=FALSE)
# double exponential - models level and trend
fit.2 <- HoltWinters(AirPassengers, gamma=FALSE)
# triple exponential - models level, trend, and seasonal components
fit.3 <- HoltWinters(AirPassengers)
  
# predictive accuracy
library(forecast)
plot(forecast(fit.1, 12),main="models level")
plot(forecast(fit.2, 12),main="models level and trend")
plot(forecast(fit.3, 12),main="models level, trend, and seasonal components")
par(op)

