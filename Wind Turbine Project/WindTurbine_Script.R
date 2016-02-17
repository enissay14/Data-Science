
#1 PREPARATION OF DATA ------------------------------------------------------
# Load data
wt <- read.table('/home/yassine/EMSE 2015-2016/Data Science/Octo TP/eolienne_public_v1.csv', sep = ';', dec = '.', 
                 header = TRUE, stringsAsFactors = FALSE)

#check for NA ---------
apply(wt, 2,  function(x) (75680/length(which( is.na(x)))) )
# There is 11.65% of missing Data for the Nacelle Angle
# -- ~ 3.23% missing of Generator Speed, Palm Orientation and the Speed of the Rotor
length(which( !apply(wt, 1,  function(x) length(which( is.na(x))) ) == 0 ))/75680
#31.05% of lines are missing data and they are ...
wt[which( !apply(wt, 1,  function(x) length(which( is.na(x))) ) == 0 ),]

#exemple of Generator Speed
plot(wt$VG)

#since we're going to work only on the first variables for now VV, DV, and AN
#we can just delete those lines for now

# Remove all lines containing NA
NAfilter <- function(mydf){
  indexNA <- c()
  for (i in 1:ncol(mydf)){
    indexNA <- union(indexNA, which(is.na(mydf[, i])))
  }
  return(indexNA)
}

indexNA <- NAfilter(wt)
wt <- wt[-indexNA, ]
# check: print(NAfilter(wt))

# extract a subsample at random (useful for exploration)
n <- nrow(wt)
set.seed(0)  # for reproductibility fix the random seed
subsample <- sample.int(n, floor(n/10))   
subwt <- wt[subsample,]   # subsample of wt of size n/10

###############################################
n <- dim(wt)[1]
p <- dim(wt)[2]
learning.Set <- setdiff(1:n,3*(1:(n/3)))
test.Set <- 3*(1:(n/3))
wt.learning <- wt[learning.Set,]
wt.test <- wt[test.Set,]
wt.learning.sc <- as.data.frame(scale(wt.learning,center = TRUE,scale = TRUE))

#2 Exploration of data ------------------------------------------------------
#scale of data
boxplot(subwt)

#ACP 
LS.pca <- prcomp(subwt, scale=TRUE)
summary(LS.pca)
plot(LS.pca, type = "l")
biplot(LS.pca,choices = c(1,2), cex=0.5)
abline(h = 0, v = 0, lty = 2, col = 8)
#first two PC's are enough to explain 99.99% of variance


#On desire connaitre la signification des axes pour chaque ACP pour cela on cree une fonction "contribution" :
#x : l'ACP sur laquelle on desire travailler
#u : l'axe sur lequel on veut des renseignements
#t : variable booleene qui nous donne la contribution positive de l'axe si vrai, la contribution negative sinon
contribution<-function(x,u,t){
  a<-c();
  b<-c();
  n<-length(x$rotation[,u]);
  for(i in 1:n){
    m<-max(x$rotation[,u]);
    if(abs(x$rotation[i,u])>75/100*m){
      
      if(x$rotation[i,u]>0){
        a<-cbind(a,rownames(x$rotation)[i]);
      }
      if(x$rotation[i,u]<0){
        b<-cbind(b,rownames(x$rotation)[i]);
      }
    }
  }
  
  if(t==TRUE) return(a)
  if(t==FALSE) return(b)
}

contribution(LS.pca,1,TRUE) # VV,VG,VR are the variables influencing positevely the first PC
contribution(LS.pca,1,FALSE) # OP1/2/3 are the variables influencing negatively the first PC
contribution(LS.pca,2,TRUE) # T is influencing positevely the first PC
contribution(LS.pca,2,FALSE) # Everything else influencing negatively
contribution(LS.pca,3,TRUE) # Alignement Direction du vent 
contribution(LS.pca,3,FALSE) #  l'angle nacelle et l'orientation des pales et VV
contribution(LS.pca,4,TRUE) # T 
contribution(LS.pca,4,FALSE) #  NULL
contribution(LS.pca,5,TRUE) # Angle Nacelle
contribution(LS.pca,5,FALSE) #  Direction Vent

#repartition of each variable
hist(LS$PR)
hist(LS$VV)
#normal?

hist(LS$DV)
hist(LS$AN)
#DV and AN have almost the same distrubution

#relation of variables between each other
pairs(LS)
plot(subwt$X2[1:100],type="l")
lines(subwt$Y[1:100],col="red")
lines(subwt$X6[1:100],col="blue")
lines(subwt$X7[1:100],col="green")

subwt.sc <- as.data.frame(scale(subwt,center = TRUE,scale = TRUE))
plot(subwt.sc$Y[1:50],type="l")
lines(subwt.sc$X3[1:50],col="red")
lines(subwt.sc$X2[1:100],col="blue")
#3 main areas: 
# - VV < 0.5        : no energy produced
# - 0.5 < VV < 1.8  : positive correlation (power? ,exponential?)
# - VV < 1.8        : stagnation of energy produced (at ~ 2.7) (1er indice? seuil de s?cu)

plot(LS$DV-LS$AN,LS$PR) 
plot(LS$AN,LS$PR)  # positevely correlated and follows the distribution of DV and AN (affine?)

#3 Regression Model ------------------------------------------------------
#after the result of the exploration of the data we confirm that a regression
#model based on VV, DV, AN is enough to predict the PR

#from the plot of VV,PR we see that we have 3 main areas...
#visually we can use a regression on "V - a" with a estimated visually to ~1.8

#3.1 Multivariate Adaptive Regression Splines-----------
install.packages("earth")
library(earth)

PR <- wt.learning$PR
VV <- wt.learning$VV
plot( PR~ VV)
######### detection du seuil VV
earth.mod <- earth(PR ~ VV, data = wt.learning)
plotmo(earth.mod)
summary(earth.mod, digits = 2, style = "pmax")

# first break : VV = 0.53
# last break : VV =  1.7
seuilmin <- 0.53
seuilmax <- 1.7
idx<- which(VV>seuilmin & VV<seuilmax)
PR <- PR[idx]
VV <- VV[idx]
data <- wt.learning[idx,]
plot( PR~ VV)

plot( subwt$PR~ subwt$OP1)
OP1 <- wt.learning$OP1
earth.mod2 <- earth(PR ~ OP1, data = wt.learning)
plotmo(earth.mod2)
summary(earth.mod2, digits = 2, style = "pmax")

seuilminOP <- 0.43
seuilmaxOP <- 1.1
idx<- which(OP1<seuilmax)
PR <- PR[idx]
OP1 <- OP1[idx]
data <- wt.learning[idx,]
plot( PR~ OP1)
#3.2 Regression models-----------------------------------


############################### Log transformation
idx1 <- which(PR>0 & VV>0)
PR <- PR[idx1]
VV <- VV[idx1]
data <- data[idx1,]

plot( PR ~ log(VV))
plot(log(PR) ~ log(VV))

model.log <- lm(log(PR) ~ log(VV), data = data)
summary(model.log)
coefficients(model.log)
# log(Y) = a + b*log(X1)
# Y = a1*X1^b with a1 = exp(a)

a =  -0.523684
b = 3.560144

#power model ------------------------------------
LS.powreg <- lm(PR ~  I(pmax(pmin(VV, seuilmax), seuilmin)^b) + I(VG^b) + I(VR^b) - 1 , data=wt.learning)
summary(LS.powreg)
plot(LS.powreg)

#3.3 Quality criteria
Q2 <- function(y, pred){
  1 - sum((y - pred)^2)/sum((y - mean(y))^2)
}
RMSE <- function(y, pred){
  sqrt(mean((y - pred)^2))
}  
sdRMSE <- function(y, pred){
  sqrt(mean((y - pred)^2))/sd(y)
}  

pred.powreg <- predict(LS.powreg, newdata = wt.test)

#Q2reg <- Q2(y = test$Y, pred = predreg)
#RMSEreg <- RMSE(y = test$Y, pred = predreg)
sdRMSEreg <- sdRMSE(y = wt.test$PR, pred = pred.powreg)
print(sdRMSEreg)

#Day2-------------------------------------------

n <- dim(wt)[1]
p <- dim(wt)[2]
wt.learning <- wt[1:(n-18),]
wt.test <- wt[(n-17):n,]
wt.learning.sc <- as.data.frame(scale(wt.learning,center = TRUE,scale = TRUE))
seuilmin <- 0.53
seuilmax <- 1.7
a =  -0.523684
b = 3.560144

# Examine the AR structure of the residuals. Following are the ACF and PACF of the residuals. 
LS.powreg <- lm(PR ~  I(pmax(pmin(VV, seuilmax), seuilmin)^b) + I(VG^b) + I(VR^b) - 1 , data=wt.learning)

plot(LS.powreg$residuals)
acf(LS.powreg$residuals,lag=100)
pacf(LS.powreg$residuals,lag=100)

arima.mod <- arima(wt.learning$PR,order=c(2,0,0),xreg=as.matrix(LS.powreg$model[,-1]))
tsdiag(arima.mod)

#forecast play------
install.packages("forecast")
library(forecast)
fc <- forecast(arima.mod , h=1000, level=95)
plot(fc)

#suite-------
wt.test$VV <-  pmax(pmin(wt.test$VV, seuilmax), seuilmin)^b
wt.test$VG <-  wt.test$VG^b
wt.test$VR <- wt.test$VR^b
wt.test <- subset(wt.test, select = c(PR,VV,VG,VR))  

arima.predict <- predict(arima.mod, n.ahead = 18, newxreg = wt.test[,-1])
plot(arima.predict$pred)
pred <- arima.predict$pred

plot(wt$PR[(n-18):n],type="l")
lines(1:18,arima.predict$pred,col="red")
lines(1:18,arima.predict$pred+2*arima.predict$se,col="red",lty=3)
lines(1:18,arima.predict$pred-2*arima.predict$se,col="red",lty=3)

#Day-3----------
B<-100
h<-18
Y<-matrix(NA,h,B)
u<-matrix(NA,1,h+2)
epsilon<-matrix(NA,1,h)

epsilon<-sample(LS.powreg$residuals,size=h,replace=TRUE)

u[1]<- LS.powreg$residuals[length(LS.powreg$residuals)-1]
u[2]<- LS.powreg$residuals[length(LS.powreg$residuals)]
for(j in 1:B){
  for(i in 3:20){
    u[i]<-arima.mod$model$phi[1]*u[i-1]+arima.mod$model$phi[2]*u[i-2]+epsilon[i-2]
    Y[i-2,j]<- LS.powreg$coefficients[1] * wt.test$VV[i-2] + LS.powreg$coefficients[2] * wt.test$VG[i-2] + LS.powreg$coefficients[3] * wt.test$VR[i-2] + u[i] 
  }
}

apply(Y,2,quantile,0,025)
apply(Y,2,quantile,0,975)

#Day-4--------------------------
n <- nrow(wt)
wt.train <- wt[1:((n/3)*2),]
wt.test <- wt[(((n/3)*2)+1):n,]
#Recursive Partitioning and Regression Trees
library(rpart)
library(party)
library(partykit)
library(rattle)

n.train <- nrow(wt.train)
n.test <- nrow(wt.test)

wt.train$PRP <- c(0,wt.train$PR[1:(n.train-1)])
wt.test$PRP <- c(0,wt.test$PR[1:(n.test-1)])

modelTree1 <- rpart(PR ~ .,data = wt.train, method="anova")
asRules(modelTree1)
plot(as.party(modelTree1))

pred <- predict(modelTree1, wt.test)
plot(pred ~ wt.test$PR)
Q2(wt.test$PR, pred)
sdRMSE(y = wt.test$PR, pred = pred)

#plus il est petit le cp plus le modèle est complexe
mod2 <- rpart(Y ~ ., data= train, method="anova",cp=0.5)


#on peut utiliser le paramètre 0 mais faut etre méfiant avec les arbres trop complexes
#penser validation croisée

#RandomForest------------------------------
library(randomForest)
#nodesize feuille terminal il faut avoir au moins 1000 elem (réduit la complexité)
#ntree nombre d'arbre'
modelRF <- randomForest(PR ~ . , data =trainnodesize=1000, ntree=80)
plot(modelRF)

library(foreach)
library(doParallel)

detectCores()

cl <- makeCluster(2)
registerDoParallel(cl)


modelRF <- foreach(ntree=rep(10,8), .combine = combine, .packages = "randomForest"){
  randomForest(PR ~ .,
               data=, nodesize,
               ntree= ntree,
               importance = TRUE)
}

stopCluster(cl)
#interessant!! qu'elles ont été le varibles dans la constitution des arbres
importance(modelRF)

#gradient boosting-----------------------
library(xgboost)

x <- as.matrix(train[,-1])
y <- as.matrix(train$PR)

modelxgboost <- xgboost(dat = x, label = y, objective="reg:linear', max.depth = 5, nrouds = 1000")
#prof de 5 et nround nombre d'apprentissage successif
predictionXGB <- predict(modelxgboost, as.matrix(train))

#cross validation
library(caret)

fitControl <- trainControl(
        #n-fold CV
        method = "repeatedcv",
        number = 3,
        repeats = 1)

cl <- makeCluster(2)
registerDoParallel(cl)
#eta shrinkage paramater
tuneGrid <- expand.grid(max_depth = 1:10, nround=150, eta = c(0.01,0.1,0.5))

#package caret
xvalxGB <- train(PR ~., ata = traindata,
                 #ou mettre method="randomForest"
                 method="xgbTree",
                 tuneGrid = tuneGrid,
                 trcontrol = fitControl)
stopCluster(cl)

plot(xvalxGB)

predict(xvalxGB ~ )

#VW------

cols2Lines <- function(df, sep) {
  
  v <- paste0(rep(names(df), each = nrow(df)) , sep, unlist(df))
  
  ma <- matrix(v , ncol = ncol(df) , nrow = nrow(df) , byrow = F)
  
  ma <- apply(ma , 1 , paste , collapse = " ")
  
  ma
  
}
df2vwNumCar <- function(df , target){
  
  lineHolders <- df[,target]
  
  dfvar <- df[, names(df) !=target]
  
  numericVar <- dfvar[,sapply(dfvar, is.numeric)]
  
  characterVar <- dfvar[,!sapply(dfvar, is.numeric)]
  
  maNum <- cols2Lines(numericVar, ':')
  
  maCar <- cols2Lines(characterVar, '_')
  
  nlines <- 1:nrow(df)
  
  DF <- paste(lineHolders, " ", nlines, "|f", " ", maCar, " ", maNum, sep = '')
  
  DF
}

dataVw <- df2vwNumCar(wt.train, 'PR')

write(dataVw, 'trainData.vw')


system("bash ")


