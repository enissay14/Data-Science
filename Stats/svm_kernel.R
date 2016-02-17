# Séparateur à Vaste Marge, cas lin?aire
# TP Optim Classique, Majeure Data Science 2015-16

SVMk <- function(X,lab,group,p,alph0,pasgrad,precision,npas_max,Nc,COLOR1,COLOR2){
  
  # classification  supervis?e, d?termination des groupes
  group1 <- (lab == 1)
  group2 <- (lab == -1)
  
  # Visualisation 2D
  xmin <- -5;
  ymin <- -5;
  xmax <- 5;
  ymax <- 5;
  plot(X[1,],X[2,],col=group)
 # plot(X[1,group1],X[2,group1],type='p',lwd=2,asp=1,ylim=c(ymin,ymax),
  #     xlab="x1",ylab="x2",main="SVM")
  #points(X[1,group2],X[2,group2],pch=4,lwd=2)
  # assemblage matrice A de la fonction duale H(alpha)
  
  A <- matrix(0,nrow=p,ncol=p)
  for(i in 1:p) {
    for(j in 1:p) {
      A[i,j] <- lab[i]*lab[j]* kernelG(X[,i],X[,j])
    }
  }
  
  alph0 <- matrix(0,nrow=p,ncol=1)                    # initialisation ? 0 (r?glable)
  u <- matrix(1,nrow=p,ncol=1)                        # vecteur colonne form? que de 1 et de taille p
  seuil_arret <- 1                                    # pour le crit?re d'arr?t sur alpha
  npas <- 0                                           # nombre d'it?rations 
  norm2lab <- as.numeric(t(lab)%*%lab)                # norme carr?e du vecteur lab
  
  while (seuil_arret > precision && npas < npas_max) {    
    npas <- npas+1
    alph <- alph0 + pasgrad*( - A%*%alph0 + u )           # gradient pas constant
    alph <- alph - as.numeric(t(alph)%*%lab)*lab/norm2lab # projection sur hyperplan
    alph <- pmax(alph,0)                                  # contrainte alpha > 0
    seuil_arret <- norm(alph - alph0)                     # distance entre deux alpha successifs
    alph0 <- alph # on it?re
  }
  
  # Calcul de b : on le calcule pour un point support, voire plusieurs 
  # pour tester la convergence
  # imax = indice i correspondant ? alpha[i] maximum
  
  imax <- which.max(alph)
  b <- 0                                                  # calcul de b
  for(i in 1:p) {
    b <- b + (alph0 * lab)[i,] * kernelG(X[,i],X[,imax]) 
  }
  b <- lab[imax] - b              
  
  decision <- function(x){
    fx <- 0
    for(i in 1:p) {
      fx <- fx + lab[i]*alph[i]* kernelG(X[,i],x) 
    }
    
    return(fx + b)
  }
  
  axe_x <- seq(xmin, xmax, length.out=Nc)
  axe_y <- seq(xmin, xmax, length.out=Nc)
  decisionA <- matrix(nrow = length(axe_x),ncol = length(axe_y))
  for(i in 1:length(axe_x))
  {
    for (j in 1:length(axe_y))
    {
      decisionA[i,j] <- decision(c(axe_x[i], axe_y[j]))
    }
  }
  
  contour(axe_x,axe_y,decisionA,nlevels = 3, levels = 0, col=COLOR1,add=TRUE,label="droite separatrice",labcex=0.8)
  contour(axe_x,axe_y,decisionA,nlevels = 3, levels = c(-1,1), col=COLOR2,add=TRUE,label="droite support",labcex=0.8)
  
  # Affichage npas, seuil d'arr?t et alpha
  print("nombre it?rations")
  print(npas)
  print("seuil d'arr?t")
  print(seuil_arret)
  print("alpha")
  print(alph)
}

kernelG <- function(X,Y){
  return(exp(- sum((X - Y)^2) / (teta^2) ))
}

kernelP <- function(X,Y){
  return( (1 + X %*% Y)^teta)
}

# jeu de donn?es 2D : p points en colonnes dans X de taille 2xp  
# labels dans le vecteur colonne lab de taille px1
X <- matrix(c(0.5,2,1,1,2,3,-1,0,-2,0.5),nrow=2,ncol=5)
lab <- matrix(c(1,1,1,-1,-1),nrow=5,ncol=1)

# Simulate some data
n    = 100
nnew = 50
set.seed(12345)
lab = sample(c(-1,1), n, replace=T)
group <- mat <- ifelse(lab<0,2,lab)
X   = matrix(rnorm(n*2, rep(lab, each=2)), nrow=2)
p <- length(lab)  # nombre de points

pasgrad <- 1e-3                 # pas du gradient : param?tre r?glable
precision <- 1e-5 
npas_max <- 100000              # nombre max d'it?rations (garde fou)

teta <- 6                #téta kernel
Nc <- 100                       #grille

#color1 of seperator line
#color2 of support lines
SVMk(X,lab,group,p,alph0,pasgrad,precision,npas_max,Nc,"blue","green")


