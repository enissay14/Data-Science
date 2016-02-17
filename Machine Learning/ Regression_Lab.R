installed.packages("ISLR")
library(ISLR)

#1Data preparation-------------------------------------------------------------------------------------
fix(Hitters)
Hitters<-na.omit(Hitters)
n <- length(Hitters[,1])

#learning set construction
LS<- Hitters[setdiff(1:n, 3*(1:(n/3))),]

#Test set construction
TS <- Hitters[3*(1:(n/3))  ,]

#2Examination of Data----------------------------------------------------------------------------------
pairs(LS[,1:10])	
boxplot(LS[,1:10])
hist(LS)
plot(LS[,1],LS[,2])

#3Regression-------------------------------------------------------------------------------------------
reg <- lm(Salary ~ ., data=LS)
summary(reg)
reg$coefficients

#plot of residuals
plot(reg)
plot(density(resid(reg)))
plot(rstudent(reg) ~ hatvalues(reg))

# which observations 'are' influential
inflm.reg <- influence.measures(reg)
which(apply(inflm.reg$is.inf, 1, any))

#4Construction données centrées réduites--------------------------------------------------------------

LS.cs <- subset(LS, select = -c(Division,League,NewLeague) )     #remove qualitative varibles from learning set
LS.cs <- scale(LS, center=TRUE, scale=TRUE)                   #center and scale the learning set 
LS.cs <- as.data.frame(LS.cs)

LS.cs$Division <-LS$Division                                  #put back the qualitative varibles
LS.cs$League <- LS$League
LS.cs$NewLeague <- LS$NewLeague

#5Regression sur données centrées réduites-----------------------------------------------------------------
reg.cs <- lm(Salary ~ ., data=LS.cs)
summary(reg.cs)

#comparaison avec le modèle sur les données non réduites
plot(reg$coefficients ~ reg.cs$coefficients)
summary(reg)
summary(reg.cs)
#les coefficients sont plus centrées (logique) mais en remarque que
#le rapport "t" de la variance sur l'erreur reste le même

plot(reg.cs)
plot(density(resid(reg.cs)))
plot(rstudent(reg.cs) ~ hatvalues(reg.cs))
plot(rstandard(reg.cs) ~ hatvalues(reg.cs))

# which observations 'are' influential
inflm.reg.cs <- influence.measures(reg.cs)
which(apply(inflm.reg.cs$is.inf, 1, any))

#6 Stepwise selection method-----------------------------------------------------------------
step(reg, direction="both")
#modèle retenu pour données CS
#Call: lm(formula = Salary ~ AtBat + Hits + Walks + CAtBat + CHits + 
#     CRuns + CRBI + CWalks + PutOuts, data = LS)
step(reg.cs, direction="both")
#modèle retenu pour données CS
#Call: lm(formula = Salary ~ AtBat + Hits + Walks + CAtBat + CHits + 
#     CRuns + CRBI + CWalks + PutOuts, data = LS.cs)

reg.best <- lm(Salary ~ AtBat + Hits + Walks + CAtBat + CHits + CRuns + CRBI + CWalks + PutOuts, data = LS.cs)
summary(reg.best)
anova(reg.cs,reg.best)
#la p-value est proche de 1 (=0.8734) donc ça confirme que notre ème modèle est meilleur! (logique)

#7 best subset Model from ISLR----------------------------------------------------------------------
install.packages("leaps")
library("leaps")

Sub <- regsubsets(Salary ~ AtBat + Hits + Walks + CAtBat + CHits + CRuns + CRBI + CWalks + PutOuts,data=LS)
summary(Sub)
plot(Sub, scale="bic")#Ctitère BIC : meilleur modèle Walts + CAtBat + CHits + PutOuts 
plot(Sub, scale="Cp") #Ctitère Cp : meilleur modèle Walts + CAtBat + CHits + CRBI + PutOuts 
plot(Sub, scale="r2") #Ctitère R2 : meilleur modèle All var sauf CWalks
plot(Sub, scale="adjr2") #Ctitère R2adj : meilleur modèle All var sauf CWalks

#8 Install pls package----------------------------------------------------------------------------------------------
install.packages("pls")
library(pls)

#9 PCR Model -----------------------------------------------------------------------------------------------------
LS.wq <- subset(LS.cs, select = -c(Division,League,NewLeague))  #Learning Set without quantitative data
PCR <- pcr(Salary ~ .,3,data = LS.wq, validation = "CV")
summary(PCR)
coef(PCR, intercept="TRUE")
coef(PCR, intercept="TRUE", ncomp=3)
R2(PCR)
plot(PCR)
plot(RMSEP(PCR), legendpos = "right")
plot(PCR, ncomp = 3, asp = 1, line = TRUE)                  #plot cross-validated predictions fct measured
plot(PCR, ncomp = 3, asp = 1, line = TRUE,which = "train")  #plot fitted values fct measured
plot(PCR , plottype = "scores", comps=1:3)                  #plot of score values
plot(PCR, "loadings", comps = 1:3, legendpos ="topleft")                          #loading plot
abline(h = 0)
corrplot(PCR, comps = 1:2,labels = "names")                 #Correlation loading plot

#10 Ridge Regression ----------
install.packages("MASS")
library(MASS)

ridge.reg <- lm.ridge(Salary ~ .,data = LS.cs,lambda = seq(0,0.1,0.001))
summary(ridge.reg)
plot(ridge.reg)
select(ridge.reg)

# Selection of constant is at endpoint.  Extend endpoint and try again
ridge.reg <- lm.ridge(Salary ~ .,data = LS.cs,lambda = seq(0,1,0.01))
summary(ridge.reg)
plot(ridge.reg)
select(ridge.reg)

# Selection of constant is at endpoint.  Extend endpoint and try again
ridge.reg <- lm.ridge(Salary ~ .,data = LS.cs,lambda = seq(0,10,0.1))
summary(ridge.reg)
plot(ridge.reg)
select(ridge.reg)

# Selection of constant is at endpoint.  Extend endpoint and try again
ridge.reg <- lm.ridge(Salary ~ .,data = LS.cs,lambda = seq(0,100,1))
summary(ridge.reg)
plot(ridge.reg)
select(ridge.reg)

ridge.reg$kLW
ridge.reg$kHKB
lamin <- which.min(ridge.reg$GCV)
ridge.reg$coef[,lamin]

#11 Lasso Regression -------------------------------------------------
install.packages("lars")
library(lars)

y <- LS.cs$Salary                                                           #prepare x and y 
x<- subset(LS.cs, select = -c(Salary,Division,League,NewLeague))
x <- as.matrix(x)

lasso.reg <-lars(x,y)                                             
cv.lars(x,y)
plot(lasso.reg)
summary(lasso.reg)
coef.lars(lasso.reg)

