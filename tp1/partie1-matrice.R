A = matrix(1:15,ncol=5)  # définition d’une matrice
A
B = matrix(1:15,nc=5,byrow=TRUE)
B
A[1,3] ; A[,2] ; A[2,] ; A[1:3,1:3] # affichage des composants de A