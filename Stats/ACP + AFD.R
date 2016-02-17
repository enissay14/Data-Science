
#load data
BF <- read.csv2("/home/yassine/EMSE 2015-2016/Data Science/Analyse de données/TP3/BF23_covDS14.csv")
CA <- read.csv2("/home/yassine/EMSE 2015-2016/Data Science/Analyse de données/TP3/CA1234_covDS14.csv")

#unify colnames
colnames(CA) <- colnames(BF)

#change 0 to NA
BF[BF == 0] <- NA
CA[CA == 0] <- NA

#all set
X = rbind(BF ,CA)

apply(X, 2,  function(x) length(which( is.na(x)) ) )  #check NA

#---------------------question 1----------------------------------------------------------

#Traitement de donnees
#Pretraitement moyennes des valeurs sur points
moyenne.point <- function(X){
  for(j in 7:32){
    for(i in 1:142) {
      #Si cellule NA
      if(is.na(X[i,j])){
        
        #recuperer variable + station
        var <- colnames(X)[j]
        station <- as.character(X[i,6])
        
        #recuperer les mesures de la station sur les autres periodes
        station.mesures <- X[X[,"COMMENT"]==station,var]
        
        #Si nombre des mesures Na / nombre mesures d'une station sur toutes les priodes <= 20%
        if(length(which( is.na(station.mesures))) / length(station.mesures) <= 0.2){
          
          #Remplacer par la moyennes des mesures de la station non NA
          X[i,j] <- 0
          count <- 0
          for(k in 1:length(station.mesures)){
            if(!is.na(station.mesures[k])){
              X[i,j] <- X[i,j] + station.mesures[k]
              count <- count + 1
            }
          } 
          X[i,j] <- X[i,j] / count
        }
      }
    }
  }
  return(X)
}

#Pretraitement moyennes des valeurs sur periodes
moyenne.period<- function(X){
  for(j in 7:32){
    #recuperer les mesures de la periode
    period.mesures <- X[,j]
    #Si nombre des mesures Na / nombre mesures d'une periode <= 20%
    p <- length(which( is.na(period.mesures))) / length(period.mesures)
    
    if( p <= 0.4 && p  > 0){
      for(i in 1:length(period.mesures)) {
        
        
        #Si cellule NA
        if(is.na(X[i,j])){
          
          #Remplacer par la moyennes des mesures de periode non NA
          X[i,j] <- 0
          count <- 0
          for(k in 1:length(period.mesures)){
            if(!is.na(period.mesures[k])){
              X[i,j] <- X[i,j] + period.mesures[k]
              count <- count + 1
            }
          } 
          X[i,j] <- X[i,j] / count
        }
      }
    }
  }
  return(X)
}

fix <- function(X){
  #moyenne temporelle sur les mesures d'une station
  X <- moyenne.point(X)
  #count NA in each colomns
  apply(X, 2,  function(x) length(which( is.na(x)) ) )
  
  BF2 <- X[X[,"PERIODE"]== "BF2",]
  BF3 <- X[X[,"PERIODE"]== "BF3",]
  
  CA1 <- X[X[,"PERIODE"]== "CA1",]
  CA2 <- X[X[,"PERIODE"]== "CA2",]
  CA3 <- X[X[,"PERIODE"]== "CA3",]
  CA4 <- X[X[,"PERIODE"]== "CA4",]
  
  #count NA in each colomns
  apply(BF2, 2,  function(x) length(which( is.na(x)) ) )
  apply(BF3, 2,  function(x) length(which( is.na(x)) ) )
  apply(CA1, 2,  function(x) length(which( is.na(x)) ) )
  apply(CA2, 2,  function(x) length(which( is.na(x)) ) )
  apply(CA3, 2,  function(x) length(which( is.na(x)) ) )
  apply(CA4, 2,  function(x) length(which( is.na(x)) ) )
  
  #moyenne spaciale sur les mesures de toutes les stations d'une periode
  BF2.c <- moyenne.period(BF2)
  BF3.c <- moyenne.period(BF3)
  CA1.c <- moyenne.period(CA1)
  CA2.c <- moyenne.period(CA2)
  CA3.c <- moyenne.period(CA3)
  CA4.c <- moyenne.period(CA4)
  
  #count NA in each colomns
  apply(BF2.c, 2,  function(x) length(which( is.na(x)) ) )
  apply(BF3.c, 2,  function(x) length(which( is.na(x)) ) )
  apply(CA1.c, 2,  function(x) length(which( is.na(x)) ) )
  apply(CA2.c, 2,  function(x) length(which( is.na(x)) ) )
  apply(CA3.c, 2,  function(x) length(which( is.na(x)) ) )
  apply(CA4.c, 2,  function(x) length(which( is.na(x)) ) )
  
  #reconstruction du nouveau X
  X = rbind(BF2.c,BF3.c,CA1.c,CA2.c,CA3.c,CA4.c)
  #count NA in each colomns
  apply(X, 2,  function(x) length(which( is.na(x)) ) )
  return(X)
}

X <- fix(X)
apply(X, 2,  function(x) length(which( is.na(x)) ) )        #check NA


#Supprresion des colonnes où les données sont manquntes
X <- X[,-11]  #supprimmer X7_ane
X <- X[,-11]  #supprimmer X8_ane
X <- X[,-19]  #supprimmer AcAcBE
X <- X[,-19]  #supprimmer Carene
X <- X[,-19]  #supprimmer Pinene
X <- X[,-19]  #supprimmer Limonene
X <- X[,-19]  #supprimmer Thujone
X <- X[,-22]  #supprimmer NananoicAc
X <- X[,-22]  #supprimmer DecanoicAc
X <- X[,-22]  #supprimmer Octanal
X <- X[,-22]  #supprimmer Decanal
apply(X, 2,  function(x) length(which( is.na(x)) ) )         #check NA

#---------------------question 2----------------------------------------------------------

#Construction des Matrices
BF2 <- X[X[,"PERIODE"]== "BF2",]
BF3 <- X[X[,"PERIODE"]== "BF3",]

CA1 <- X[X[,"PERIODE"]== "CA1",]
CA2 <- X[X[,"PERIODE"]== "CA2",]
CA3 <- X[X[,"PERIODE"]== "CA3",]
CA4 <- X[X[,"PERIODE"]== "CA4",]

#matrice avant et après activité
BF <- rbind(BF2,BF3)
CA <- rbind(CA1,CA2,CA3,CA4)

#matrice hiver et ete
hiver <- rbind(BF2,BF3,CA1,CA3)
ete <- rbind(CA2,CA4)

rownames(BF) <-  paste(strtrim(BF[,3],4),BF[,6],sep="")
rownames(CA) <-  paste(strtrim(CA[,3],4),CA[,6],sep="")
rownames(ete) <-  paste(strtrim(ete[,3],4),ete[,6],sep="")
rownames(hiver) <-  paste(strtrim(hiver[,3],4),hiver[,6],sep="")

BF<-BF[,-1:-6]
CA<-CA[,-1:-6]
ete<-ete[,-1:-6]
hiver<-hiver[,-1:-6]

#Pour ne pas avoir de problèmes d'échelles on centre et on réduit nos données grâce à la fonction "centre_reduit" :
centre_reduit<-function(x){
  p<-length(x[1,]);
  n<-length(x[,1])
  for(i in 1:p){
       m<-mean(x[,i]);
      s<-sd(x[,i]);
      for(j in 1:n){
          x[j,i]<-(x[j,i]-m)/s
      }
  }
  return(x)
}

A<-centre_reduit(BF);
B<-centre_reduit(CA);
E<-centre_reduit(ete);
H<-centre_reduit(hiver);

#5
#On désire connaître la signification des axes pour chaque ACP pour cela on crée une fonction "contribution" :
#x : l'ACP sur laquelle on désire travailler
#u : l'axe sur lequel on veut des renseignements
#t : variable booléene qui nous donne la contribution positive de l'axe si vrai, la contribution négative sinon
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

#Pour voir les corrélations entre variables on crée une fonction "correlation" qui nous 
#affiche les fortes corrélations ( critère arbitraire >0.80 ) entre variables d'une table de données :
correlation<-function(x){
  p<-length(x[1,]);
  z<-matrix(NaN,nrow=p,ncol=p);
  a<-colnames(x);
  colnames(z)<-a;
  rownames(z)<-a;
  for(i in 1:p)
  {
    for(j in 1:p)
    {
      z[i,j]<-cor(x[,i],x[,j]);
    }
  }
  
  pairs(x);
  
  b<-array(99,1);
  m<-matrix(c('Variable 1','Variable 2','Correlation'),nrow=1,ncol=3);
  
  for(i in 1:p)
  {
    for(j in 1:p)
    {
      s<-0;
      for(k in 1:length(b))
      {
        if(b[k]==z[i,j]) s<-1
        
      }
      
      
      if(z[i,j]>0.80 && i!=j && s==0)
      {
        a<-matrix(c(colnames(z)[i],colnames(z)[j],z[i,j]),nrow=1,ncol=3);
        m<-rbind(m,a);
        b<-cbind(b,z[i,j]);
      }
    }
  }  
  
  return (m)  
}

#Analyse Ete/Hiver

summary(H)
summary(E)

correlation(E)
correlation(H)
#on voit qu'il y a forte corrélation entre T E et X avec BTM et X1_M_2_PA pour les périodes d'hiver, 
#en été on a la disparition de T et l'apparition de la corrélation avec X10_ane

E.acp <- prcomp(E)
summary(E.acp)
plot(E.acp, type = "l")
#besoin de 7 Composantes principales pour passer le cap de 90% d'explication de la variance
#on restera sur 3 CP qui expliquent presque 80% 

#Ete : quelles sont les variables qui contribuent à P1 positivement (TRUE) / négativement (FALSE)
contribution(E.acp,1,TRUE)
contribution(E.acp,1,FALSE)
#quelles sont les variables qui contribuent à CP2 positivement (TRUE) / négativement (FALSE)
contribution(E.acp,2,TRUE)
contribution(E.acp,2,FALSE)
#quelles sont les variables qui contribuent à CP3 positivement (TRUE) / négativement (FALSE)
contribution(E.acp,3,TRUE)
contribution(E.acp,3,FALSE)

#Projection des individus
biplot(E.acp,choices = 1:2, cex=0.5)
abline(h = 0, v = 0, lty = 2, col = 8)
biplot(E.acp,choices = 2:3, cex=0.8)
abline(h = 0, v = 0, lty = 2, col = 8)

#on remarque qu'il y a deux groupes distincts ainsi qu'une station qui se démarque 
# par sa concentration en composantes qui sont négatiement corrélées à la première composantes, qui est B10
#l'apport de la troisième composante amène plus de variance (meme si les observations ont plus tendance à ce grouper dns le centre de ce plan)
#certaines stations se démarquent (tiré par les concentration des variables corrélées négativement avec CP3 :"X1_6ol_2_E" "FormicAcid")
# et qui sont A27, A02, B05, B10 , ainsi qu"une autre station b18 ( tiré par la forte concentrtion en X16_ane cette fois)


H.acp <- prcomp(H)
summary(H.acp)
plot(H.acp, type = "l")
# de même 3 CPs qui expliquent ~74% de la varince
biplot(H.acp,choices = 1:2, cex=0.8)
abline(h = 0, v = 0, lty = 2, col = 8)
biplot(H.acp,choices = 2:3, cex=0.8)
abline(h = 0, v = 0, lty = 2, col = 8)

#Hiver : quelles sont les variables qui contribuent à P1 positivement (TRUE) / négativement (FALSE)
contribution(H.acp,1,TRUE)
contribution(H.acp,1,FALSE)
#quelles sont les variables qui contribuent à CP2 positivement (TRUE) / négativement (FALSE)
contribution(H.acp,2,TRUE)
contribution(H.acp,2,FALSE)
#quelles sont les variables qui contribuent à CP3 positivement (TRUE) / négativement (FALSE)
contribution(H.acp,3,TRUE)
contribution(H.acp,3,FALSE)

#Sans surprise la station B10 est constatée comme étant la plus poluée ( plan CP2 et CP3 on la voit nettement la distance
#entre sa valeur avant et après l'installation - ce qui nous amènerait à dire que l'axe 2 peut être assimilé à l'explication
#de la concentration en pollution)

#Analyse Avant/Après installation site
correlation(A)
correlation(B)

A.acp <- prcomp(A)
summary(A.acp)
plot(A.acp, type = "l")
biplot(A.acp,choices = 1:2, cex=0.8)
abline(h = 0, v = 0, lty = 2, col = 8)
biplot(A.acp,choices = 2:3, cex=0.8)
abline(h = 0, v = 0, lty = 2, col = 8)

#Avant : quelles sont les variables qui contribuent à P1 positivement (TRUE) / négativement (FALSE)
contribution(A.acp,1,TRUE)
contribution(A.acp,1,FALSE)
#quelles sont les variables qui contribuent à CP2 positivement (TRUE) / négativement (FALSE)
contribution(A.acp,2,TRUE)
contribution(A.acp,2,FALSE)
#quelles sont les variables qui contribuent à CP3 positivement (TRUE) / négativement (FALSE)
contribution(A.acp,3,TRUE)
contribution(A.acp,3,FALSE)

#on remarque qu'en plus la station b10 était déjàa polluée et que d'autres (B02, B17,B19,B21..) avait une forte
#concentration en X14 et X16 initialement (pour fait de proximité, est ce que ça veut dire qu'elle étaient moins polluées?)')
#la projection sur le plan PC2 PC3 nous montrent presque la meme choses (toujours les mêmes stations qui sortent du lot
#grâce à de grandes cencentrations en général de BTM et X1_M_2_PA)

B.acp <- prcomp(B)
summary(B.acp)
plot(B.acp, type = "l")
biplot(B.acp,choices = 1:2, cex=0.8)
abline(h = 0, v = 0, lty = 2, col = 8)
biplot(B.acp,choices = 3:4, cex=0.8)
abline(h = 0, v = 0, lty = 2, col = 8)

#Après : quelles sont les variables qui contribuent à P1 positivement (TRUE) / négativement (FALSE)
contribution(B.acp,1,TRUE)
contribution(B.acp,1,FALSE)
#quelles sont les variables qui contribuent à CP2 positivement (TRUE) / négativement (FALSE)
contribution(B.acp,2,TRUE)
contribution(B.acp,2,FALSE)
#quelles sont les variables qui contribuent à CP3 positivement (TRUE) / négativement (FALSE)
contribution(B.acp,4,TRUE)
contribution(B.acp,4,FALSE)

#En résultat la variance apportée par la station B10 (qui tire toujours sur la première composante principale)
#nous oblige à passer aux plans suivant pour déchiffrer les relations entre les variables pendant le temps.
#Ce qu'apporte l'ACP sur les données enregistrées après l'installation du site et le la constitution de 3 groupes 
#étalés sur l axe de la composante principales numéro 3, ainsi que la concentration qui a augmentée pour plusieurs
#stations (B17 , A27, A22 et B06, B05, B04 proches??!) en "X1_6ol_2_E" et "FormicAcid"

#ACP

ACP<-prcomp(rbind(A,B))
plot(ACP, type = "l")
summary(ACP)
#Avec les 4 premières composantes principales on peut expliquer 80% de la variance

biplot(ACP,choice=1:2,cex=0.6)
abline(h = 0, v = 0, lty = 2, col = 8)
biplot(ACP,choice=2:3,cex=0.6)
abline(h = 0, v = 0, lty = 2, col = 8)
biplot(ACP,choice=3:4,cex=0.6)
abline(h = 0, v = 0, lty = 2, col = 8)

#Après : quelles sont les variables qui contribuent à P1 positivement (TRUE) / négativement (FALSE)
contribution(ACP,1,TRUE)
contribution(ACP,1,FALSE)
#quelles sont les variables qui contribuent à CP2 positivement (TRUE) / négativement (FALSE)
contribution(ACP,2,TRUE)
contribution(ACP,2,FALSE)
#quelles sont les variables qui contribuent à CP3 positivement (TRUE) / négativement (FALSE)
contribution(ACP,3,TRUE)
contribution(ACP,3,FALSE)

#meme constats que sur les acp sur les différentes périodes (hyp selon laquelles des stations était déjà polluées
#B02,B10,B15) et d'autres qui avait déjà de forte concentration en X16_ane (B17,B18...)
#on remarque que la station B10 est la plus polluée de tous on l'enlève et on répète les ACP pour
#montrer les variances des autres stations.
#aussi l'ACP pour les valeur entière a donnée les même résultats que les ACP sur les différentes périodes 
#ce qui veut dire qu'on peut utiliser les données groupées.

#--------- ACP sans les mesures sur B10 -------------
X <- rbind(A,B)
X <- X[-6,]
X <- X[-38,]
X <- X[-55,]
X <- X[-77,]

ACP.bis<-prcomp(X)
plot(ACP.bis, type = "l")
summary(ACP.bis)
#Avec les 3 premières composantes principales on peut expliquer 67% de la variance

biplot(ACP.bis,choice=1:2,cex=0.6)
abline(h = 0, v = 0, lty = 2, col = 8)
biplot(ACP.bis,choice=2:3,cex=0.6)
abline(h = 0, v = 0, lty = 2, col = 8)

#Après : quelles sont les variables qui contribuent à P1 positivement (TRUE) / négativement (FALSE)
contribution(ACP.bis,1,TRUE)
contribution(ACP.bis,1,FALSE)
#quelles sont les variables qui contribuent à CP2 positivement (TRUE) / négativement (FALSE)
contribution(ACP.bis,2,TRUE)
contribution(ACP.bis,2,FALSE)
#quelles sont les variables qui contribuent à CP3 positivement (TRUE) / négativement (FALSE)
contribution(ACP.bis,3,TRUE)
contribution(ACP.bis,3,FALSE)

#On remarque 3 groupes distincts dès la projection sur le premier plan des 2 première composntes principales
#un groupe des mesures faites en hiver qui est plutôt proche du centre, et 2 groupes plus étirés (2 formes de pllutions?)
#constituées principalement de mesures après installation.
#un vers le haut ("X15_ane" et "aceticacid" qui contribuent positivement à PC2) et un vers la gauche où les mesures 
#des stations sont fortes en B T et X.. 
#L'analyse sur le deuxième plan nous montrent que les concentrations en FomicAcid par exemple augmentaient pour des 
#des stations comme B17 B15, mais qu'après l'installation du site elles ont rejoints les autres stations
#où les concentrations en X15_ane et aceticacid augmentent


-------------------------- AFD -------------------------------------
  
  
#Hiver / Eté
  #On reload les données et on refait le prétraitement de la partie 1
  #ainsi on obtient une matrice X
  
  load.data <- function(){
    BF <- read.csv2("/home/yassine/EMSE 2015-2016/Data Science/Analyse de données/TP3/BF23_covDS14.csv")
    CA <- read.csv2("/home/yassine/EMSE 2015-2016/Data Science/Analyse de données/TP3/CA1234_covDS14.csv")
    #unify colnames
    colnames(CA) <- colnames(BF)
    #change 0 to NA
    BF[BF == 0] <- NA
    CA[CA == 0] <- NA
    #all set
    X = rbind(BF ,CA)
    #Pretraitement moyennes des valeurs sur points
    X <- fix(X)
    
    #Supprresion des colonnes où les données sont manquntes
    X <- X[,-11]  #supprimmer X7_ane
    X <- X[,-11]  #supprimmer X8_ane
    X <- X[,-19]  #supprimmer AcAcBE
    X <- X[,-19]  #supprimmer Carene
    X <- X[,-19]  #supprimmer Pinene
    X <- X[,-19]  #supprimmer Limonene
    X <- X[,-19]  #supprimmer Thujone
    X <- X[,-22]  #supprimmer NananoicAc
    X <- X[,-22]  #supprimmer DecanoicAc
    X <- X[,-22]  #supprimmer Octanal
    X <- X[,-22]  #supprimmer Decanal
    
    return(X)
  }
  
  X <- load.data()
  colnames(X)[1] <- "period"
  period <- X[,1]
  X<-X[,-1:-6]
  X<-centre_reduit(X);
  X$period <- period
  
  #Analyse discriminante
  install.packages("FactoMineR")
  library(FactoMineR)
  
  famd1 <- FAMD(X,graph = TRUE)
  
  #Hiver/Ete : quelles sont les variables qui contribuent à P1 positivement (TRUE) / négativement (FALSE)
  contribution(famd1,1,TRUE)
  contribution(famd1,1,FALSE)
  #Axe 1
  #Contribution positive -> BTM X10ane X
  #Contribution négative -> NULL
  
  #quelles sont les variables qui contribuent à CP2 positivement (TRUE) / négativement (FALSE)
  contribution(famd1,2,TRUE)
  contribution(famd1,2,FALSE)
  #Axe 2
  #Contribution positive -> period Formiacid
  #Contribution négative -> NULL
  
  #On distingue deux rassemblement selon l'axe 2 qui correspond aux deux périodes. On remarque que le rassemblement correspondant à l'été s'étire le long de l'axe 1 (BTM X10ane et X).
  #Cet étirement laisse supposer que les sites sont plus pollués l'été que l'hiver.

#Avant/Après
  #On reload les données et on refait le prétraitement de la partie 1
  #ainsi on obtient une matrice X
  
  X <- load.data()
  
  #construction colonne "periode"
  #Construction des Matrices
  BF2 <- X[X[,"PERIODE"]== "BF2",]
  BF3 <- X[X[,"PERIODE"]== "BF3",]
  
  CA1 <- X[X[,"PERIODE"]== "CA1",]
  CA2 <- X[X[,"PERIODE"]== "CA2",]
  CA3 <- X[X[,"PERIODE"]== "CA3",]
  CA4 <- X[X[,"PERIODE"]== "CA4",]
  
  #matrice avant et après activité
  BF <- rbind(BF2,BF3)
  CA <- rbind(CA1,CA2,CA3,CA4)
  
  BF$period <- "B" 
  CA$period <- "C"
  
  X <-rbind(BF,CA)
  period <- X$period
  X<-X[,-1:-6]
  X<-X[,-16]
  X<-centre_reduit(X);
  X$period <- period
  
  #Analyse discriminante
  install.packages("FactoMineR")
  library(FactoMineR)
  
  famd2 <- FAMD(X,graph = TRUE)
  
  #Hiver/Ete : quelles sont les variables qui contribuent à P1 positivement (TRUE) / négativement (FALSE)
  contribution(famd2,1,TRUE)
  contribution(famd2,1,FALSE)
  #Axe 1
  #Contribution positive -> X9ane X X10ane BTM
  #Contribution négative -> NULL
  
  #quelles sont les variables qui contribuent à CP2 positivement (TRUE) / négativement (FALSE)
  contribution(famd2,2,TRUE)
  contribution(famd2,2,FALSE)
  #Axe 2
  #Contribution positive -> period 
  #Contribution négative -> NULL
  
  #On observe maintenant trois groupes, le groupe avant installation en hiver, le groupe après installation en hiver et le groupe après installation en été.
  #Les groupes sont beaucoup mieux discriminés, les points sont moins éparpillés. On remarque toujours un étirement du groupe après installation en été le long de l'axe 1.

  #On en déduit les sites avec une concentration en X9ane X X10ane et BTM importante :
  #-CA2B18
  #-CA2B17
  #-CA2B20
  #-CA2B15
  #-CA2A32
  #-CA2A33
  
