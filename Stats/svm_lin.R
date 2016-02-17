# Séparateur à Vaste Marge, cas linéaire
# TP Optim Classique, Majeure Data Science 2015-16

SVM_GP <- function(X,lab,p,alph0,pasgrad,precision,npas_max,COLOR1,COLOR2){
  
  
  # Assemblage matrice A de la fonction duale H(alpha)
  A <- matrix(0,nrow=p,ncol=p)
  for(i in 1:p) {
    for(j in 1:p) {
      A[i,j] <- lab[i]*lab[j]*t(X[,i])%*%X[,j]
    }
  }
  
  
  
  # Gradient à pas constant pour le problème dual
  seuil_arret <- 1                             # pour le critère d'arrêt sur alpha
  u <- matrix(1,nrow=p,ncol=1)                # vecteur colonne formé que de 1 et de taille p
  npas <- 0                                   # nombre d'itérations 
  norm2lab <- as.numeric(t(lab)%*%lab)        # norme carrée du vecteur lab
  conv <- matrix(0,nrow = npas_max,ncol = 2)  # tableau de convergence
  
  
  while (seuil_arret > precision && npas < npas_max) {    
    npas <- npas+1
    alph <- alph0 + pasgrad*( - A%*%alph0 + u )           # gradient pas constant
    alph <- alph - as.numeric(t(alph)%*%lab)*lab/norm2lab # projection sur hyperplan
    alph <- pmax(alph,0)                                  # contrainte alpha > 0
    conv[npas,1] <- npas
    conv[npas,2] <- norm(alph - alph0)
    seuil_arret <- norm(alph - alph0)                             # distance entre deux alpha successifs
    alph0 <- alph                                                 # on itère
  }
  
  # Calcul de w optimal
  
  w <- X%*%(lab*alph)
  
  # Calcul de b : on le calcule pour un point support, voire plusieurs 
  # pour tester la convergence
  
  # imax = indice i correspondant ? alpha[i] maximum et calcul de b optimal
  
  imax <- which.max(alph)
  b <- lab[imax] - t(w)%*%X[,imax]            
  
  # classification  supervisée, détermination des groupes
  group1 <- (lab == 1)
  group2 <- (lab == -1)
  
  # Visualisation 2D
  xmin <- -15;
  ymin <- -5;
  xmax <- 15;
  ymax <- 5;
  plot(X[1,group1],X[2,group1],type='p',lwd=2,asp=1,ylim=c(ymin,ymax),
       xlab="x1",ylab="x2",main="SVM")
  points(X[1,group2],X[2,group2],pch=4,lwd=2)
  
  # Tracé de la droite séparatrice w'*x + b = 0 
  
  xp <- c(xmin,xmax);
  if(w[2] != 0) yp <- (- b - w[1]*xp)/w[2] else xp <- (- b - w[2]*yp)/w[1]
  lines(xp,yp,col=COLOR1)
  
  # Tracé des deux droites support w'*x + b = +1 et w'*x + b = -1
  
  if(w[2] != 0) yp <- (1 - b - w[1]*xp)/w[2] else xp <- (1 - b - w[2]*yp)/w[1]
  lines(xp,yp,col=COLOR2)
  if(w[2] != 0) yp <- (-1 - b - w[1]*xp)/w[2] else xp <- (-1 - b - w[2]*yp)/w[1]
  lines(xp,yp,col=COLOR2)
  
  print(conv[(npas-1):npas,])
  # Convergence
  #plot(conv[,1],conv[,2],type ="l", xlab="pas",ylab="distance alph - alph0",main="Convergence")
  
  # Affichage npas, seuil d'arrêt et alpha
  
  print("nombre itérations")
  print(npas)
  return(npas)
}

# jeu de données 2D : p points en colonnes dans X de taille 2xp  
# labels dans le vecteur colonne lab de taille px1

X <- matrix(c(0.5,2,1,1,2,3,-1,0,-2,0.5),nrow=2,ncol=5)
lab <- matrix(c(1,1,1,-1,-1),nrow=5,ncol=1)
p <- length(lab)  # nombre de points

alph0 <- matrix(4,nrow=p,ncol=1)      # initialisation à 0 (réglable)
pasgrad <- 0.1                       # pas du gradient : paramètre réglable
precision <- 1e-5                     #précision  
npas_max <- 100000                    # nombre max d'itérations (garde fou)

#color1 of seperator line
#color2 of support lines
SVM_GP(X,lab,p,alph0,pasgrad,precision,npas_max,"blue","red")

param <- 20
G <- matrix(0,ncol = 2, nrow = param)
for(i in 1:param ){
  G[i,1] <- i
  alph0 <- matrix(i,nrow=p,ncol=1)
  G[i,2] <- SVM_GP(X,lab,p,alph0,pasgrad,precision,npas_max,"blue","red")
}

plot(G[,1],G[,2],type ="l", xlab="alpha0",ylab="itération",main="Variation des itérations en fonction de alpha0")