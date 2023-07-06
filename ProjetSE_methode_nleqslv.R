#=============Groupe systeme d'equation non linéaire======================
#====================résolution avec nleqslv()===============================
# Charger le fichier de données
library(haven)
test <- read_dta("test.dta")
View(test)
data=test[,1:7]

library(nleqslv)

# Créer les variables A, B, C initialisées à 0
data$A <- 0
data$B <- 0
data$C <- 0

# Début de la boucle
for (i in 1:4) {
  # Extraire les valeurs nécessaires pour l'itération i
  yf <- data$Yf[i]
  e1 <- data$e1[i]
  e2 <- data$e2[i]
  e3 <- data$e3[i]
  w1 <- data$w1[i]
  w2 <- data$w2[i]
  w3 <- data$w3[i]
  
  # Définir la fonction du solveur
  mysolver <- function(p) {
    a <- p[1]
    b <- p[2]
    c <- p[3]
    lnf<- numeric(3)
    lnf[1] <- ((a*w1/e1)/(a*w1/e1 + b*w2/e2 + c*w3/e3) - 2/5)
    lnf[2] <-((b*w2/e2)/(a*w1/e1 + b*w2/e2 + c*w3/e3) - 2/5)
    lnf[3] <- (a + b + c - yf)
    return(lnf)
  }
  
  # Appeler le solveur
  
  result <- nleqslv::nleqslv( c(1, 2, 1), mysolver, method = "Broyden",control = list( xtol= 1e-8,ftol=1e-15) )
  p <- result$x
  print(result$iter)
  
  # Assigner les résultats aux variables A, B, C
  data$A[i] <- p[1]
  data$B[i] <- p[2]
  data$C[i] <- p[3]
}
View(data)

