# génération aléatoire des données à classifier
# avec une première moitié dans une classe et la seconde dans une autre classe

nn = 50
x <- rbind(matrix(rnorm(2*nn, mean = 0.5, sd = 0.3), ncol = 2),
           matrix(rnorm(2*nn, mean = 1, sd = 0.3), ncol = 2))
colnames(x) <- c("x", "y")
z <- rbind(matrix(1,nn, 1), matrix(2,nn, 1))
colnames(z) <- c("z")

donnees <- cbind(x,z)
donnees

plot(donnees[,1:2], col = z)

# on prépare un sosu-ensemble d'apprentissae et un autre de test
n <- nrow(donnees)

I <- sample(1:n,(2*n)/3)
J <- setdiff(1:n,I)

# on prépare les données : on construit le classifieur K-NN
# pour les valeurs numérique et on extrait explicitement la classe 
# à predire

cl <- donnees[I,3]

dlrn <- donnees[I,1:2]
dtest <- donnees[J,1:2]

library (class)

mknn1 <- knn(dlrn, dtest,cl, k=1)
mknn1
table(mknn1, donnees[J,3])


mknn3 <- knn(dlrn, dtest,cl, k=3)
mknn3
table(mknn3, donnees[J,3])

mknn7 <- knn(dlrn, dtest,cl, k=7)
mknn7
table(mknn7, donnees[J,3])

mknn11 <- knn(dlrn, dtest,cl, k=11)
mknn11
table(mknn11, donnees[J,3])

# validation croisée

train <- donnees[,1:2]
cl <- donnees[,3]
model <- knn.cv(train,cl,k=5)
model
table(cl,model)
