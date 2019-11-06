taille = c(147, 132, 156, 167, 156, 140)  # création de la variable poids (6 obs.)
poids = c( 50, 46, 47, 62, 58, 45)
sexe = c("M","F","F","M","M","F")
H = data.frame(taille,poids,sexe)  # création de la base de données H
H
H$taille  # affichage de la variable taille uniquement
summary(H)  # Statistiques descriptives de H
plot(H$poids,H$taille) # création nuage de points poids x taille